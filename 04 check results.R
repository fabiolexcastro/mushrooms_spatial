
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, terra, gtools, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------
path <- './tif/terraclimate/extent/individual'
fles <- dir_ls(path)
fles <- grep('.tif$', fles, value = T)
fles <- as.character(fles)
fles <- grep('305', fles, value = TRUE)

# To check precipitation --------------------------------------------------
prec <- grep('prec', fles, value = T)
prec <- map(.x = prec, .f = terra::rast)

prec <- map(.x = 1:length(prec), .f = function(i){
  cat(i, '\t')
  ppt <- prec[[i]]
  ppt[which.lyr(ppt < 0)] <- 0
  out <- glue('{path}/{names(ppt)}_v2.tif')
  terra::writeRaster(x = ppt, filename = out, overwrite = TRUE)
  cat('Done!\n')
})

# To check tmax and tmin --------------------------------------------------




