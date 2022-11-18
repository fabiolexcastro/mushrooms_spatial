
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, dismo, rmapshaper, terra, gtools, rnaturalearth, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

source('bioclimatic functions.R')

# Function ----------------------------------------------------------------
makeBIOetp <- function(yr){
  
  # yr <- 2019
  
  cat('Start ', yr, '\n')
  ppt <- grep(yr, prec, value = TRUE)
  tmx <- grep(yr, tmax, value = TRUE)
  tmn <- grep(yr, tmin, value = TRUE)
  etp <- grep(yr, etps, value = TRUE)
  
  # Read as raster
  ppt <- raster::stack(ppt)
  tmx <- raster::stack(tmx)
  tmn <- raster::stack(tmn)
  etp <- raster::stack(etp)
  
  # To extract by mask (each zone individually)
  purrr::map(.x = 1:nrow(rgns), .f = function(z){
    
    cat(z, '\n') # Imprime en pantalla el numero de la región por donde va
    rgn <- rgns[z,]
    pp  <- raster::crop(ppt, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    tx  <- raster::crop(tmx, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    tn  <- raster::crop(tmn, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    et  <- raster::crop(etp, as(rgn, 'Spatial')) %>% raster::mask(., as(rgn, 'Spatial'))
    
    ta  <- (tx + tn) / 2
    
    names(pp) <- glue('prec_{1:12}')
    names(et) <- glue('etp_{1:12}') # glue es para pegar, similar al paste0
    names(ta) <- glue('tmean_{1:12}')
    
     # Convert to matrix and create the main matrix
    etpr <- cbind(as.matrix(et),as.matrix(pp),as.matrix(ta))
  
    # To create the etp bioclimatic variables
    etbi <- t(apply(etpr, 1, etpvars))
    nmes <- paste0('bio_', 21:29)
    zero <- pp[[1]]
    zero <- zero * 0 + 1
    names(zero) <- 'zero'
    
    name <- rgn$gid
    
    # To write these results
    dout <- 'tif/terraclimate/extent/individual/zones/'
    
    purrr::map(.x = 1:ncol(etbi), .f = function(k){
      cat(k, '\n')
      lyer <- zero
      values(lyer) <- etbi[,k]
      raster::writeRaster(lyer, filename = glue('{dout}/{nmes[k]}_{yr}_{name}.tif'), overwrite = TRUE)
      cat('Done!\n')
    })
    
    cat('Finish!\n')
    
})
  
  
}

# Load data ---------------------------------------------------------------
fles <- dir_ls('tif/terraclimate/extent/individual') %>% as.character(.)

# Grepping (filtering)
prec <- grep('prec', fles, value = TRUE) %>% grep('gwr_raw_305m_v2.tif$', ., value = T) %>% mixedsort()
etps <- grep('etp', fles, value = TRUE) %>% mixedsort()
tmax <- grep('tmax', fles, value = TRUE) %>% grep('gwr_raw_305m_v2.tif$', ., value = T) %>% mixedsort()
tmin <- grep('tmin', fles, value = TRUE) %>% grep('gwr_raw_305m_v2.tif$', ., value = T) %>% mixedsort()

# Years 
year <- 2016:2019 # aqui creo un objeto que va desde 2016 hasta el 2019

# Regions 
rgns <- sf::st_read('./gpkg/base/study_zone_areasp.gpkg')
rgns <- terra::vect(rgns)
rgns$gid <- 1:nrow(rgns)

View(as.data.frame(rgns))

# To calculate the ETP variables ------------------------------------------
map(2016:2019, makeBIOetp)

# To make the mosaic  -----------------------------------------------------







