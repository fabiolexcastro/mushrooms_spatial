
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
  
  yr <- 2017
  
  cat('Start ', yr, '\n')
  ppt <- grep(yr, prec, value = TRUE)
  tmx <- grep(yr, tmax, value = TRUE)
  tmn <- grep(yr, tmin, value = TRUE)
  
  # Read as raster
  ppt <- raster::stack(ppt)
  tmx <- raster::stack(tmx)
  tmn <- raster::stack(tmn)
  
  # To calculate bioclimatic variables 
  purrr::map(.x = 1:nrow(rgns), .f = function(z){
    
    cat(z, '\n') 
    rgn <- rgns[z,]
    pp  <- raster::crop(ppt, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    tx  <- raster::crop(tmx, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    tn  <- raster::crop(tmn, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    bi  <- dismo::biovars(prec = pp, tmax = tx, tmin = tn)
    ou  <- glue('tif/terraclimate/extent/individual/zones/v2/bios_{rgn$gid}_{yr}.tif')
    raster::writeRaster(x = bi, filename = ou, overwrite = TRUE)
    cat('Done!\n')
    
  })
  
}

rstr <- terra::rast('tif/terraclimate/extent/individual/zones/v2/bios_1_2016.tif')

# Mosaicking each bioclimatic var 
vars <- glue('bios_{1:19}') %>% as.character()
fles <- dir_ls('./tif/terraclimate/extent/individual/zones/v2', regexp = '.tif$') %>% as.character() %>% grep(paste0(vars, collapse = '|'), ., value = T)
# i <- 6; j <- 4
yrs <- 2016:2019

purrr::map(.x = 1:length(yrs), .f = function(i){ # ciclo por variable, (bio 21 a bio 29)
  
  cat('Start ', yrs[i], '\n')
  fls <- grep(yrs[i], fles, value = T)
  rst <- map(fls, rast)
  
  purrr::map(.x = 2:19, .f = function(j){ # ciclo por ano, (2016 a 2019)
    
    cat(vars[j], '\t')
    lst <- map(.x = 1:length(rst), .f = function(k) rst[[k]][[j]])
    lst <- sprc(lst)
    msc <- terra::mosaic(lst)
    out <- glue('./tif/terraclimate/extent/individual/bios/v2/{vars[j]}_{yrs[i]}.tif')
    terra::writeRaster(x = msc, filename = out, overwrite = TRUE)
    rm(lst, msc); gc()
    Sys.sleep(2)
    cat('Done!\n')
    
  })
  
})


