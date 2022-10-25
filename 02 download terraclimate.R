
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, terra, climateR, openxlsx, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------

lbls <- tibble(season = c(rep('Otono', 4), rep('Primavera', 3), rep('Verano', 3)), months = c('Mar', 'Abr', 'May', 'Jun', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb'))

shpf <- st_read('./gpkg/base/study_zone_areasp.gpkg')
pnts <- read_excel('./tble/points/all_points.xlsx')
pnts$basename

startDate <- as.Date('2016-05-01', format = '%Y-%m-%d')
endDate <- as.Date('2019-03-01', format = '%Y-%m-%d')

# To download -------------------------------------------------------------

prec <- getTerraClim(AOI = shpf, param = 'prcp', startDate = startDate, endDate = endDate)
prec <- prec[[1]]

tmax <- getTerraClim(AOI = shpf, param = 'tmax', startDate = startDate, endDate = endDate)
tmax <- tmax[[1]]

tmin <- getTerraClim(AOI = shpf, param = 'tmin', startDate = startDate, endDate = endDate)
tmin <- tmin[[1]]

tavg <- (tmax + tmin) / 2

# To make the crop --------------------------------------------------------
prec <- raster::crop(prec, as(shpf, 'Spatial'))
prec <- raster::mask(prec, as(shpf, 'Spatial'))

tmax <- raster::crop(tmax, as(shpf, 'Spatial'))
tmax <- raster::mask(tmax, as(shpf, 'Spatial'))

tmin <- raster::crop(tmin, as(shpf, 'Spatial'))
tmin <- raster::mask(tmin, as(shpf, 'Spatial'))

# To write ----------------------------------------------------------------
dout <- './tif/terraclimate/extent'
dir_create(dout)

raster::writeRaster(x = prec, filename = glue('{dout}/prec.tif'))
raster::writeRaster(x = tmax, filename = glue('{dout}/tmax.tif'))
raster::writeRaster(x = tmin, filename = glue('{dout}/tmin.tif'))
raster::writeRaster(x = tavg, filename = glue('{dout}/tavg.tif'))

# To check duplicated cells -----------------------------------------------

mask <- tavg[[1]] * 0 + 1
clls <- raster::extract(mask, pnts[,c('coor_x', 'coor_y')], cellnumber = T)

table(clls[,1]) %>% length()


shpf
pnts
colnames(pnts)
pnts$idcodyceps %>% table()

# To download the elevation -----------------------------------------------
srtm <- get_elev_raster(locations = shpf, z = 10)
srtm <- raster::crop(srtm, as(shpf, 'Spatial'))
srtm <- raster::mask(srtm, as(shpf, 'Spatial'))

raster::writeRaster(x = srtm, filename = './tif/srtm/srtm_150m_raw.tif')

clls <- raster::extract(srtm, pnts[,c('coor_x', 'coor_y')], cellnumber = T)
table(clls[,1]) %>% length()


