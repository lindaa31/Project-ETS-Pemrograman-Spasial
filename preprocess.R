# ================================================================
# PREPROCESSING WEBGIS KABUPATEN PROBOLINGGO
# Sentinel-2B L2A, 03 Juli 2026, tile T49MGM
# NDVI, NDWI, NDCI (eksperimen B07/B03), NDBI, komposit
# ================================================================

# Memaksa R menggunakan lingkungan PROJ bawaan R sendiri (mengatasi konflik PostGIS/PostgreSQL)
Sys.unsetenv("PROJ_LIB")
Sys.setenv(PROJ_LIB = system.file("proj", package = "sf"))

library(sf)
library(terra)
library(dplyr)

akar <- 'C:/KULIAH/Semester5/Pemrograman Spasial/ETS Pemspa'
dir_batas <- file.path(akar, 'data', 'batas')
dir_s2 <- file.path(akar, 'data', 'sentinel')
dir_hasil <- file.path(akar, 'data', 'hasil')
dir_temp <- file.path(akar, 'temp_terra')
dir.create(dir_hasil, recursive=TRUE, showWarnings=FALSE)
dir.create(dir_temp, recursive=TRUE, showWarnings=FALSE)
terra::terraOptions(tempdir=dir_temp, memfrac=0.2, progress=1)

safe <- file.path(dir_s2, 'S2B_MSIL2A_20260703T022529_N0512_R046_T49MGM_20260703T052526.SAFE')
if (!dir.exists(safe)) stop('Folder SAFE tidak ditemukan: ', safe)

# 1. Baca shapefile; jangan mengubah file asli
baca_batas <- function(nama) {
  for (ext in c('.shp','.shx','.dbf','.prj')) {
    f <- file.path(dir_batas, paste0(nama,ext))
    if (!file.exists(f)) stop('Berkas shapefile tidak ada: ', f)
  }
  x <- sf::st_read(file.path(dir_batas,paste0(nama,'.shp')), quiet=TRUE)
  if (is.na(sf::st_crs(x))) stop('CRS ', nama, ' tidak ditemukan')
  cat(nama, ': ', nrow(x), ' fitur asli\n', sep='')
  x
}
kab_asli <- baca_batas('Batas Kabupaten')
kec_asli <- baca_batas('Batas Kecamatan')

# 2. Abaikan geometri kosong yang sengaja dihapus dari QGIS
perbaiki_area <- function(x, label) {
  x <- x[!sf::st_is_empty(x), , drop=FALSE]
  if (nrow(x)==0) stop(label, ': semua geometri kosong')
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), , drop=FALSE]
  tipe <- as.character(sf::st_geometry_type(x))
  # st_collection_extract hanya pada fitur GEOMETRYCOLLECTION
  if (any(tipe=='GEOMETRYCOLLECTION')) {
    koleksi <- x[tipe=='GEOMETRYCOLLECTION', , drop=FALSE]
    bagian <- sf::st_collection_extract(koleksi, 'POLYGON', warn=FALSE)
    bagian <- bagian[!sf::st_is_empty(bagian), , drop=FALSE]
    x <- rbind(x[tipe %in% c('POLYGON','MULTIPOLYGON'), , drop=FALSE], bagian)
  } else {
    x <- x[tipe %in% c('POLYGON','MULTIPOLYGON'), , drop=FALSE]
  }
  if (nrow(x)==0) stop(label, ': tidak ada area polygon setelah penyaringan')
  x <- sf::st_make_valid(x)
  if (any(sf::st_is_empty(x))) stop(label, ': geometri menjadi kosong setelah validasi')
  if (!all(as.character(sf::st_geometry_type(x)) %in% c('POLYGON','MULTIPOLYGON'))) {
    stop(label, ': hasil validasi mengandung geometri bukan polygon')
  }
  cat(label, ': ', nrow(x), ' fitur digunakan\n', sep='')
  x
}

# 3. Pilih Kabupaten Probolinggo dari atribut, kecualikan Kota Probolinggo
kab_valid <- perbaiki_area(kab_asli, 'Kabupaten (tidak kosong)')
atribut <- sf::st_drop_geometry(kab_valid)
kolom_admin <- intersect(c('WADMKK','NAMOBJ','WILAYAH','KABUPATEN','KAB_KOTA'), names(atribut))
if (!length(kolom_admin)) stop('Kolom nama kabupaten tidak ditemukan. Lihat names(kab_asli).')
hit_kab <- rep(FALSE,nrow(kab_valid))
hit_kota <- rep(FALSE,nrow(kab_valid))
for (nm in kolom_admin) {
  teks <- tolower(trimws(as.character(atribut[[nm]])))
  teks[is.na(teks)] <- ''
  hit_kab <- hit_kab | grepl('probolinggo',teks,fixed=TRUE)
  hit_kota <- hit_kota | grepl('kota probolinggo',teks,fixed=TRUE)
}
kabupaten <- kab_valid[hit_kab & !hit_kota, , drop=FALSE]
if (!nrow(kabupaten)) stop('Kabupaten Probolinggo tidak ditemukan dalam atribut shapefile.')
kabupaten <- sf::st_transform(kabupaten,32749)
g <- sf::st_union(sf::st_geometry(kabupaten))
if (length(g)!=1 || any(sf::st_is_empty(g))) stop('Union Kabupaten Probolinggo kosong')
kabupaten <- sf::st_sf(Nama_Kabupaten='Probolinggo', geometry=g)
roi <- terra::vect(kabupaten)
cat('ROI kabupaten terbentuk.\n')

# 4. Siapkan kecamatan sebelum operasi raster (PERBAIKAN ERROR OBJECT NOT FOUND)
kecamatan <- perbaiki_area(kec_asli,'Kecamatan')
if (!'WADMKC' %in% names(kecamatan)) stop('Kolom WADMKC tidak ada pada batas kecamatan')
kecamatan$Nama_Kecamatan <- trimws(as.character(kecamatan$WADMKC))
if (anyNA(kecamatan$Nama_Kecamatan) || any(!nzchar(kecamatan$Nama_Kecamatan))) {
  stop('Nama kecamatan ada yang kosong pada WADMKC')
}
kecamatan <- sf::st_transform(kecamatan,32749)
# Hanya polygon yang beririsan area positif dengan kabupaten
calon <- lengths(sf::st_intersects(kecamatan,kabupaten))>0
kecamatan <- kecamatan[calon, , drop=FALSE]
if (!nrow(kecamatan)) stop('Tidak ada kecamatan beririsan dengan Kabupaten Probolinggo')
kecamatan <- suppressWarnings(sf::st_intersection(kecamatan,kabupaten))
kecamatan <- perbaiki_area(kecamatan,'Kecamatan hasil irisan')
# Satukan bagian-bagian dengan nama sama
kecamatan <- kecamatan |>
  dplyr::group_by(Nama_Kecamatan) |>
  dplyr::summarise(do_union=TRUE,.groups='drop')
kecamatan <- perbaiki_area(kecamatan,'Kecamatan akhir')
cat('Jumlah kecamatan akhir: ',nrow(kecamatan),'\n',sep='')
print(kecamatan$Nama_Kecamatan)
if (nrow(kecamatan)!=24) {
  stop('Jumlah kecamatan harus 24, didapat ',nrow(kecamatan),
       '. Periksa nama kecamatan dan batas; tidak aman menghitung statistik.')
}
kec_vect <- terra::vect(kecamatan)

# 5. Membaca band Sentinel-2B L2A
jp2 <- list.files(safe,pattern='\\.jp2$',recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
ambil_band <- function(band,res_m=20) {
  cocok <- grep(paste0('_',band,'_',res_m,'m\\.jp2$'),jp2,value=TRUE,ignore.case=TRUE)
  if (length(cocok)!=1) stop('Band ',band,' ',res_m,'m ditemukan ',length(cocok),' kali')
  r <- terra::rast(cocok[1])
  if (!nzchar(terra::crs(r))) stop('CRS JP2 kosong: ',cocok[1],'. Periksa GDAL/PROJ')
  if (!terra::same.crs(r,'EPSG:32749')) stop('CRS band bukan EPSG:32749: ',cocok[1])
  r
}

tes <- ambil_band('B03',20)
# Bounding box intersect tidak sama dengan seluruh wilayah tercakup
irisan_bbox <- terra::intersect(terra::ext(tes),terra::ext(roi))
if (is.null(irisan_bbox)) stop('Tile tidak beririsan dengan ROI; perlu tile lain')
cat('Tile dan ROI beririsan. Cakupan penuh belum dijamin.\n')

crop_band <- function(band,res_m=20) {
  cat('Crop ',band,' ...\n',sep='')
  r <- terra::crop(ambil_band(band,res_m),roi,snap='out')
  if (band=='SCL') r <- terra::ifel(r==0,NA,r)
  else r <- terra::ifel(r==0 | r==65535,NA,r)
  r
}
B03 <- crop_band('B03'); B04 <- crop_band('B04')
B07 <- crop_band('B07'); B11 <- crop_band('B11')
SCL <- crop_band('SCL'); B08_10 <- crop_band('B08',10)
B08 <- terra::resample(B08_10,B03,method='bilinear')
selaras <- function(r,method='bilinear') {
  if (terra::compareGeom(r,B03,stopOnError=FALSE)) return(r)
  terra::resample(r,B03,method=method)
}
B04 <- selaras(B04); B07 <- selaras(B07); B11 <- selaras(B11)
SCL <- selaras(SCL,'near')
mask_simpan <- function(r,nama) terra::mask(
  r,roi,filename=file.path(dir_hasil,paste0('CROP_',nama,'.tif')),overwrite=TRUE)
B03 <- mask_simpan(B03,'B03'); B04 <- mask_simpan(B04,'B04')
B07 <- mask_simpan(B07,'B07'); B08 <- mask_simpan(B08,'B08')
B11 <- mask_simpan(B11,'B11'); SCL <- mask_simpan(SCL,'SCL')
if (!terra::compareGeom(B03,B04,B07,B08,B11,SCL,stopOnError=FALSE)) stop('Grid band tidak sama')

# 6. Ubah DN menjadi reflectance: (DN - 1000) / 10000
ref <- function(r) (r-1000)/10000
green <- ref(B03); red <- ref(B04); rededge <- ref(B07)
nir <- ref(B08); swir <- ref(B11)

# 7. Indeks. NDCI di sini EKSPERIMEN B07/B03, bukan indeks cloud baku.
# B04/B08 dengan (B04-B08)/(B04+B08) hanyalah -NDVI.
hitung_indeks <- function(a,b,nama,mask_cloud=TRUE) {
  den <- a+b
  x <- terra::ifel(abs(den)<1e-6,NA,(a-b)/den)
  bad <- is.na(SCL) | SCL %in% c(0,1)
  if (mask_cloud) bad <- bad | SCL %in% c(3,8,9,10,11)
  x <- terra::ifel(bad | !is.finite(x),NA,x)
  # Indeks normalized difference secara teoritis [-1,1] untuk input nonnegatif
  # Hindari nilai outlier raster pada palet peta; jangan klip nilai mentah.
  x <- terra::writeRaster(x,file.path(dir_hasil,paste0(nama,'.tif')),
                          overwrite=TRUE,datatype='FLT4S')
  names(x) <- nama
  x
}
NDVI <- hitung_indeks(nir,red,'NDVI')
NDWI <- hitung_indeks(green,nir,'NDWI')
NDCI <- hitung_indeks(rededge,green,'NDCI',mask_cloud=FALSE)
NDBI <- hitung_indeks(swir,nir,'NDBI')

# 8. Komposit: 1 vegetasi, 2 bangunan, 3 air, 4 awan
komposit <- terra::lapp(c(NDVI,NDWI,NDBI,SCL), fun=function(v,w,b,s) {
  out <- rep(NA_integer_,length(s))
  ok <- !is.na(s) & !(s %in% c(0,1,3,8,9,10,11))
  out[which(ok & !is.na(v) & v>0.3)] <- 1L
  out[which(ok & !is.na(v) & !is.na(b) & b>0.1 & b>v)] <- 2L
  out[which(ok & !is.na(w) & w>0.15)] <- 3L
  out[which(!is.na(s) & s %in% c(8,9,10))] <- 4L
  out
},filename=file.path(dir_hasil,'Komposit.tif'),overwrite=TRUE,
wopt=list(datatype='INT1U'))
names(komposit) <- 'Komposit'

# 9. Zonal statistics hanya setelah kecamatan dan kec_vect terdefinisi
r_stack <- c(NDVI,NDWI,NDCI,NDBI)
tabel <- data.frame(ID=seq_len(nrow(kecamatan)),Kecamatan=kecamatan$Nama_Kecamatan)
stat_fun <- list(mean=mean,min=min,max=max,median=median,sd=sd)
for (metode in names(stat_fun)) {
  cat('Zonal statistik: ',metode,'\n',sep='')
  z <- terra::extract(r_stack,kec_vect,fun=stat_fun[[metode]],na.rm=TRUE)
  z <- z[match(tabel$ID,z$ID),,drop=FALSE]
  if (nrow(z)!=nrow(tabel) || anyNA(z$ID)) stop('ID zonal tidak cocok')
  names(z)[-1] <- paste0(names(z)[-1],'_',metode)
  tabel <- cbind(tabel,z[,-1,drop=FALSE])
}
tabel$ID <- NULL
write.csv(tabel,file.path(dir_hasil,'Zonal_Statistics_Probolinggo.csv'),row.names=FALSE,na='')

# 10. Batas GeoJSON
sf::st_write(sf::st_transform(kabupaten,4326),file.path(dir_hasil,'kabupaten.geojson'),delete_dsn=TRUE,quiet=TRUE)
sf::st_write(sf::st_transform(kecamatan,4326),file.path(dir_hasil,'kecamatan.geojson'),delete_dsn=TRUE,quiet=TRUE)

# 11. Raster EPSG:4326 untuk Leaflet
template <- terra::project(NDVI,'EPSG:4326',method='bilinear')
for (nama in c('NDVI','NDWI','NDCI','NDBI')) {
  cat('WebGIS ',nama,'\n',sep='')
  terra::project(terra::rast(file.path(dir_hasil,paste0(nama,'.tif'))),template,
                 method='bilinear',filename=file.path(dir_hasil,paste0(nama,'_4326.tif')),
                 overwrite=TRUE,wopt=list(datatype='FLT4S'))
}
terra::project(komposit,template,method='near',
               filename=file.path(dir_hasil,'Komposit_4326.tif'),overwrite=TRUE,
               wopt=list(datatype='INT1U'))

# 12. Pratinjau
png(file.path(dir_hasil,'Preview_Indeks.png'),width=1400,height=1100)
par(mfrow=c(2,2))
plot(NDVI,main='NDVI - Vegetasi'); plot(NDWI,main='NDWI - Perairan')
plot(NDCI,main='NDCI - Awan (eksperimen B07/B03)'); plot(NDBI,main='NDBI - Bangunan')
dev.off()
png(file.path(dir_hasil,'Preview_Komposit.png'),width=1200,height=900)
plot(komposit,col=c('#228B22','#E74C3C','#2196F3','#FFFFFF'),
     type='classes',main='Komposit Probolinggo')
dev.off()
cat('\nPREPROCESSING SELESAI: ',dir_hasil,'\n',sep='')
