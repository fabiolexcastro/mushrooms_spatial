
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, terra, gtools, rnaturalearth, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------

# Study zone
zone <- st_read('./gpkg/base/study_zone_areasp.gpkg')
zone <- terra::vect(zone)

# Files
path <- './tif/terraclimate/extent/individual'
fles <- dir_ls(path)
fles <- grep('.tif$', fles, value = T)
fles <- as.character(fles)
fles <- grep('305m_v2', fles, value = TRUE)

# To filter
prec <- grep('prec', fles, value = TRUE)
tmax <- grep('tmax', fles, value = TRUE)
tmin <- grep('tmin', fles, value = TRUE)

# Solar radiation (extraterrestre)
srad <- dir_ls('D:/data/SRAD_EXTR/ET_SolRad') %>% as.character() %>% mixedsort()
srad <- srad[1:12]
srad <- terra::rast(srad)
wrld <- ne_countries(returnclass = 'sf', scale = 50)
srad <- terra::crop(srad, zone)

plot(srad[[1]])
plot(st_geometry(wrld), add = TRUE)

# Function ----------------------------------------------------------------
calcETP <- function(year){
  
  year <- 2016
  cat(year, '\n')
  ppt <- grep(year, prec, value = T)
  ppt <- terra::rast(ppt)
  
  tmx <- grep(year, tmax, value = T)
  tmx <- terra::rast(tmx)
  
  tmn <- grep(year, tmin, value = T)
  tmn <- terra::rast(tmn)
  
  srd <- terra::resample(srad, tmn)
  srd <- terra::mask(srd, zone) # mask: recortar el contorno
  
  tav <- (tmx + tmn) / 2
  
  # To calculate the ETP (hargreaves modified)
  etp <- 0.0013 * 0.408 * srd * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76
  names(etp) <- glue('etp_{1:12}')
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  out <- './tif/terraclimate/extent/individual'
  
  purrr::map(.x = 1:nlayers(etp), .f = function(j){
    raster::writeRaster(x = etp[[j]], filename = glue('{out}/etp_{year}_{j}.tif'), overwrite = TRUE)  
  })
  
  rm(etp, srd, tav, tmx, tmn, ppt)
  gc()
  
}

# Apply the function ------------------------------------------------------
etps <- map(2016:2019, calcETP)

