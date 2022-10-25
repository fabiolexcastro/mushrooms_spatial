

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, terra, openxlsx, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- 'E:/asesorias/byron/data/sh_tutoria_R_oct_2022'

dir_ls(path) %>% as.character()

# Matrix ambiental season 
mtrx <- grep('season.xlsx$', dir_ls(path), value = TRUE) %>% as.character()
mtrx <- read_excel(mtrx)

#  To make study area -----------------------------------------------------
dirs <- dir_ls(path) %>% 
  grep('area', ., value = TRUE) %>% 
  as.character() 
shpf <- map(dirs, dir_ls) %>% unlist() %>% grep('.shp', ., value = T) %>% as.character() %>% grep('proc', ., value = T)
shpf <- map(shpf, st_read)

for(i in 1:length(shpf)){
  plot(st_geometry(shpf[[i]]))
}

# Join all the areas into only one shapefile 
shpf <- bind_rows(shpf)
dout <- './gpkg/base'
st_write(shpf, './gpkg/base/study_zone_areasp.gpkg')

# To join all the tables into only one ------------------------------------
fles <- dir_ls(path) %>% grep('tabs', ., value = T) %>% as.character() %>% dir_ls() %>% grep('.xlsx$', ., value = T) %>% as.character()
lbls <- tibble(season = c(rep('Otono', 4), rep('Primavera', 3), rep('Verano', 3)), months = c('Mar', 'Abr', 'May', 'Jun', 'Sep', 'Oct', 'Nov', 'Dic', 'Ene', 'Feb'))

tbls <- purrr::map_dfr(.x = 1:length(fles), .f = function(i){
  cat(basename(fles[i]), '\n')
  tbl <- fles[i]
  tbl <- read_excel(tbl)
  nme <- basename(fles[i])
  nme <- gsub('.xlsx', '.csv', nme)
  tbl <- mutate(tbl, basename = nme)
  tbl <- dplyr::select(tbl, basename, everything())
  return(tbl)
})

dout <- './tble/points'
write.xlsx(x = tbls, file = glue('{dout}/all_points.xlsx'))
