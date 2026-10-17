# ============================================================
# WEBGIS INDEKS SPEKTRAL | KABUPATEN PROBOLINGGO
# Sentinel-2B L2A | Tile T49MGM | 3 Juli 2026
# Jalankan dengan shiny::runApp(folder_aplikasi)
# Struktur: app.R dan data/hasil/ dalam folder yang sama.
# ============================================================

library(shiny)
library(leaflet)
library(terra)
library(raster)
library(sf)
library(DT)

# Path relatif: dapat digunakan di Windows dan shinyapps.io (Linux).
dir_hasil <- file.path("data", "hasil")
nama_layer <- c("NDVI", "NDWI", "NDCI", "NDBI", "Komposit")
file_raster <- setNames(
  file.path(dir_hasil, paste0(nama_layer, "_4326.tif")),
  nama_layer
)
file_kab <- file.path(dir_hasil, "kabupaten.geojson")
file_kec <- file.path(dir_hasil, "kecamatan.geojson")
file_stat <- file.path(dir_hasil, "Zonal_Statistics_Probolinggo.csv")

wajib <- c(file_raster, file_kab, file_kec, file_stat)
hilang <- wajib[!file.exists(wajib)]
if (length(hilang)) {
  stop(
    "File aplikasi belum lengkap di folder ", normalizePath(getwd(), winslash = "/"),
    ":\n", paste(hilang, collapse = "\n"),
    "\nJalankan dengan shiny::runApp('D:/ETS Pemspa 2/deploy_webgis') ",
    "dan pastikan berkas ada di subfolder data/hasil."
  )
}

kab <- sf::st_read(file_kab, quiet = TRUE)
kec <- sf::st_read(file_kec, quiet = TRUE)
if (is.na(sf::st_crs(kab)) || is.na(sf::st_crs(kec))) {
  stop("CRS GeoJSON tidak valid.")
}
kab <- sf::st_transform(kab, 4326)
kec <- sf::st_transform(kec, 4326)
if (!"Nama_Kecamatan" %in% names(kec)) {
  stop("Kolom Nama_Kecamatan tidak ditemukan pada kecamatan.geojson.")
}
kec$Nama_Kecamatan <- as.character(kec$Nama_Kecamatan)

stat_data <- utils::read.csv(file_stat, check.names = FALSE, stringsAsFactors = FALSE)
if (!"Kecamatan" %in% names(stat_data)) {
  stop("Kolom Kecamatan tidak ditemukan dalam CSV statistik.")
}
stat_data$Kecamatan <- as.character(stat_data$Kecamatan)

# Palet dengan nama TUNGGAL dan konsisten, bukan pal_indeks/pal_kelas.
ind_pal <- leaflet::colorNumeric(
  palette = c("#9e0142", "#fdae61", "#ffffbf", "#a6d96a", "#1a9850"),
  domain = c(-1, 1), na.color = "transparent"
)
water_pal <- leaflet::colorNumeric(
  palette = c("#9e0142", "#fdae61", "#ffffbf", "#74add1", "#2b83ba"),
  domain = c(-1, 1), na.color = "transparent"
)
# Palet fungsi yang memetakan nilai kelas secara eksplisit.
class_colors <- c("#228B22", "#E74C3C", "#2196F3", "#FFFFFF")
class_pal <- leaflet::colorFactor(
  palette = class_colors,
  domain = 1:4,
  na.color = "transparent"
)

# Baca dan sederhanakan SATU raster sesuai pilihan pengguna.
# Tidak memuat semua 5 raster saat startup; menekan RAM shinyapps.io.
siapkan_raster <- function(nama, max_sel = 80000L) {
  stopifnot(nama %in% nama_layer)
  x <- terra::rast(file_raster[[nama]])
  if (terra::nlyr(x) != 1L) stop("Raster ", nama, " harus satu band.")
  if (!terra::same.crs(x, "EPSG:4326")) {
    stop("Raster ", nama, " harus ber-CRS EPSG:4326.")
  }
  faktor <- max(1L, as.integer(ceiling(sqrt(terra::ncell(x) / max_sel))))
  if (faktor > 1L) {
    x <- terra::aggregate(
      x, fact = faktor,
      fun = if (identical(nama, "Komposit")) "modal" else "mean",
      na.rm = TRUE
    )
  }
  # Konversi ke RasterLayer diperlukan untuk leaflet::addRasterImage.
  raster::raster(x)
}

ui <- shiny::fluidPage(
  tags$head(tags$style(HTML('
    body { background: #f5f7f9; }
    .well { background: white; border-radius: 12px; }
    .judul { font-weight: 700; color: #193a45; margin: 14px 0; }
    .leaflet-container { border-radius: 12px; }
  '))),
  titlePanel(div(class = "judul", "WebGIS Indeks Spektral | Kabupaten Probolinggo")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("layer", "Tampilan peta", choices = nama_layer, selected = "NDVI"),
      checkboxInput("tampil_kec", "Tampilkan batas kecamatan", TRUE),
      checkboxInput("tampil_kab", "Tampilkan batas kabupaten", TRUE),
      selectInput(
        "filter_kec", "Filter statistik kecamatan",
        choices = c("Semua", sort(unique(stats::na.omit(stat_data$Kecamatan))))
      ),
      tags$hr(),
      strong("Panduan indeks"),
      p("NDVI: vegetasi | NDWI: perairan | NDBI: kawasan terbangun."),
      p("NDCI: indeks awan eksperimental berbasis B07 dan B03; belum tervalidasi sebagai metode deteksi awan baku."),
      p("Awan pada komposit berasal dari Sentinel-2 SCL (kelas 8, 9, dan 10)."),
      p("Threshold komposit bersifat awal, belum merupakan klasifikasi teruji."),
      p("Sumber citra: Sentinel-2B Level-2A, 3 Juli 2026, tile T49MGM.")
    ),
    mainPanel(
      width = 9,
      leafletOutput("peta", height = "600px"),
      tags$h4("Statistik zonal per kecamatan"),
      DT::DTOutput("tabel"),
      downloadButton("unduh", "Unduh statistik CSV")
    )
  )
)

server <- function(input, output, session) {
  # Cache per sesi: lapisan sama tidak dihitung ulang sewaktu checkbox berubah.
  cache <- new.env(parent = emptyenv())
  
  raster_aktif <- reactive({
    req(input$layer)
    nama <- input$layer
    if (!exists(nama, envir = cache, inherits = FALSE)) {
      assign(nama, siapkan_raster(nama), envir = cache)
    }
    get(nama, envir = cache, inherits = FALSE)
  })
  
  output$peta <- renderLeaflet({
    bb <- sf::st_bbox(kab)
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "&copy; OpenStreetMap contributors"
      ) |>
      fitBounds(
        lng1 = unname(bb["xmin"]), lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]), lat2 = unname(bb["ymax"])
      )
  })
  
  observeEvent(
    list(input$layer, input$tampil_kab, input$tampil_kec),
    {
      req(input$layer)
      x <- raster_aktif()
      m <- leafletProxy("peta", session = session) |>
        clearImages() |>
        clearShapes() |>
        clearControls()
      
      if (identical(input$layer, "Komposit")) {
        m <- leaflet::addRasterImage(
          m, x, colors = class_pal, opacity = 0.85,
          project = FALSE, maxBytes = 20 * 1024 * 1024,
          layerId = "raster"
        )
        m <- leaflet::addLegend(
          m, position = "bottomright",
          colors = class_colors,
          labels = c("Vegetasi", "Terbangun", "Perairan", "Awan (SCL)"),
          title = "Kelas komposit", opacity = 1
        )
      } else {
        pal <- if (identical(input$layer, "NDWI")) water_pal else ind_pal
        # PENTING: gambar raster, bukan hanya legendanya.
        m <- leaflet::addRasterImage(
          m, x, colors = pal, opacity = 0.85,
          project = FALSE, maxBytes = 20 * 1024 * 1024,
          layerId = "raster"
        )
        m <- leaflet::addLegend(
          m, position = "bottomright", pal = pal,
          values = seq(-1, 1, length.out = 100),
          title = input$layer,
          labFormat = leaflet::labelFormat(digits = 2)
        )
      }
      
      if (isTRUE(input$tampil_kab)) {
        m <- leaflet::addPolygons(
          m, data = kab, color = "#171717", weight = 2,
          fill = FALSE, group = "Kabupaten"
        )
      }
      if (isTRUE(input$tampil_kec)) {
        m <- leaflet::addPolygons(
          m, data = kec, color = "#3d4951", weight = 1,
          fillOpacity = 0, group = "Kecamatan",
          label = ~Nama_Kecamatan,
          layerId = ~Nama_Kecamatan,
          highlightOptions = leaflet::highlightOptions(
            weight = 3, color = "#f6bb42", bringToFront = TRUE
          )
        )
      }
    },
    ignoreInit = FALSE
  )
  
  observeEvent(input$peta_shape_click, {
    klik <- input$peta_shape_click
    if (!is.null(klik$id) && length(klik$id) == 1L &&
        !is.na(klik$id) && as.character(klik$id) %in% stat_data$Kecamatan) {
      updateSelectInput(session, "filter_kec", selected = as.character(klik$id))
    }
  })
  
  tabel_filter <- reactive({
    req(input$filter_kec)
    if (identical(input$filter_kec, "Semua")) return(stat_data)
    stat_data[stat_data$Kecamatan == input$filter_kec, , drop = FALSE]
  })
  
  output$tabel <- DT::renderDT({
    dat <- tabel_filter()
    cols_angka <- names(dat)[vapply(dat, is.numeric, logical(1))]
    tb <- DT::datatable(
      dat, options = list(scrollX = TRUE, pageLength = 10),
      rownames = FALSE, filter = "top"
    )
    if (length(cols_angka)) {
      tb <- DT::formatRound(tb, columns = cols_angka, digits = 3)
    }
    tb
  })
  
  output$unduh <- downloadHandler(
    filename = function() "Zonal_Statistics_Probolinggo.csv",
    content = function(file) {
      utils::write.csv(tabel_filter(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
