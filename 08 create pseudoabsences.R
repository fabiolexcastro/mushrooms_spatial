
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, usdm, rJava, dismo, rmapshaper, terra, gtools, rnaturalearth, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------
pnts <- read_excel('tble/points/all_points_cleanDup.xlsx')
pnts

# To read the climate -----------------------------------------------------
fles <- c(dir_ls('./tif/terraclimate/extent/individual/bios', regexp = '.tif$'),
          dir_ls('./tif/terraclimate/extent/individual/bios/v2', regexp = '.tif$')) %>% as.character() %>% mixedsort()

mask <- terra::rast(fles[1]) * 0 + 1

# VIF analysis ------------------------------------------------------------
clss <- unique(pnts$sustratoOrden_rcl)
vars <- purrr::map_dfr(.x = 1:length(clss), .f = function(i){
  
  cat(clss[i], '\t')
  
  # Filtering the table
  cls <- clss[i]  
  pnt <- filter(pnts, sustratoOrden_rcl == cls)
  mtx <- dplyr::select(pnt, bio_1:bio_28)
  
  # Corrleation analysis
  mtx <- as.data.frame(mtx)
  cor <- cor(mtx)
  
  # VIF analysis
  vif <- vifstep(x = as.data.frame(mtx), th = 10)
  vrs <- vif@results$Variables %>% as.character()
  vrs <- tibble(specie = cls, variables = vrs)
  return(vrs)
  
})
write.xlsx(vars, 'tble/points/vars_species.xlsx')

# To create the pseudo-absence --------------------------------------------
fnal <- purrr::map(.x = 1:length(clss), .f = function(i){
  
  cat(clss[i], '\n')
  cls <- clss[i]
  pnt <- filter(pnts, sustratoOrden_rcl == cls)
  cls <- terra::extract(mask, pnt[,c('coor_x', 'coor_y')], cells = T)
  cls <- pull(cls, cell)
  msk <- mask
  msk[cls] <- NA
  
  # Pseudoabsences
  bck <- randomPoints(mask = raster(msk), n = nrow(pnt) * 2)
  bck <- as.data.frame(bck)
  bck <- as_tibble(bck)
  bck <- mutate(bck, pb = 0)
  bck <- relocate(bck, pb)
  
  # Presences
  pnt <- dplyr::select(pnt, coor_x, coor_y)
  pnt <- mutate(pnt, pb = 1)
  pnt <- rename(pnt, x = coor_x, y = coor_y)
  pnt <- relocate(pnt, pb)
  
  # Join both tables into only one
  all <- rbind(pnt, bck)
  all <- mutate(all, specie = clss[i])
  all <- relocate(all, specie, pb)
  
  cat('Done!\n')
  return(all)
  
})

fnal <- bind_rows(fnal)
write.xlsx(fnal, 'tble/points/occr_back_species.xlsx')





