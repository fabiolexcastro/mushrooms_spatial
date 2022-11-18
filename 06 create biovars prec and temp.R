
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, dismo, rmapshaper, terra, gtools, rnaturalearth, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------
fles <- dir_ls('tif/terraclimate/extent/individual') %>% as.character(.)

# Grepping (filtering)
prec <- grep('prec', fles, value = TRUE) %>% grep('gwr_raw_305m_v2.tif$', ., value = T) %>% mixedsort()
etps <- grep('etp', fles, value = TRUE) %>% mixedsort()
tmax <- grep('tmax', fles, value = TRUE) %>% grep('gwr_raw_305m_v2.tif$', ., value = T) %>% mixedsort()
tmin <- grep('tmin', fles, value = TRUE) %>% grep('gwr_raw_305m_v2.tif$', ., value = T) %>% mixedsort()

# Regions 
rgns <- sf::st_read('./gpkg/base/study_zone_areasp.gpkg')
rgns <- terra::vect(rgns)
rgns$gid <- 1:nrow(rgns)

# Function ----------------------------------------------------------------

# To calculate bioclimatic normal variables
makeBIOS <- function(yr){
  
  yr <- 2016
  
  cat('Start ', yr, '\n')
  ppt <- grep(yr, prec, value = TRUE)
  tmx <- grep(yr, tmax, value = TRUE)
  tmn <- grep(yr, tmin, value = TRUE)
  
  # Read as raster
  ppt <- raster::stack(ppt)
  tmx <- raster::stack(tmx)
  tmn <- raster::stack(tmn)
  
  # To calculate bioclimatic variables 
  purrr::map(.x = 1:nrow(rgns), .f = function(i){
    
    cat(z, '\n') 
    rgn <- rgns[z,]
    pp  <- raster::crop(ppt, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    tx  <- raster::crop(tmx, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    tn  <- raster::crop(tmn, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    bi  <- dismo::biovars(prec = pp, tmax = tx, tmin = tn)
    ou  <- glue('tif/terraclimate/extent/individual/zones/bios_{rgn$gid}.tif')
    raster::writeRaster(x = bi, filename = ou, overwrite = TRUE)
    cat('Done!\n')
    
  })
  

  
}

