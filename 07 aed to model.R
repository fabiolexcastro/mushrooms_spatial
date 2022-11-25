
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, lubridate, usdm, corrplot, elevatr, dismo, rmapshaper, terra, gtools, rnaturalearth, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------
pnts <- read_excel('./tble/points/all_points.xlsx')
year <- 2016:2019

# Climate 
fles <- c(dir_ls('./tif/terraclimate/extent/individual/bios', regexp = '.tif$'),
          dir_ls('./tif/terraclimate/extent/individual/bios/v2', regexp = '.tif$'))
fles <- as.character(fles)
fles <- mixedsort(fles)

# Check the points --------------------------------------------------------
head(pnts)
lbls <- as.data.frame(table(pnts$sustrato_orden)) %>% setNames(c('sustratoOrden', 'n'))
lbls <- filter(lbls, sustratoOrden != '0')
lbls <- lbls %>% arrange(desc(n))
lbls <- lbls %>% mutate(sustratoOrden_rcl = sustratoOrden)

data_edit(lbls)
lbls <- read_csv('./tble/labels_species.csv')[,-1]
pnts <- inner_join(pnts, lbls, by = c('sustrato_orden' = 'sustratoOrden'))
pnts <- mutate(pnts, year = year(dat_colec))

pnts %>% 
  group_by(sustratoOrden_rcl, year) %>% 
  summarise(count = n()) %>% 
  ungroup()

# To clean the database ---------------------------------------------------
pnts <- pnts %>% dplyr::select(id, sustratoOrden_rcl, year, coor_x, coor_y, z_m)

# To extract the values ---------------------------------------------------
vles <- purrr::map(.x = 1:length(year), .f = function(i){
  
  cat(year[i], '\n')
  yea <- year[i]
  pnt <- filter(pnts, year == yea)
  fls <- grep(yea, fles, value = T)
  fls <- mixedsort(fls)
  fls <- c(grep('v2', fls, value = T), grep(paste0('bio_'), fls, value = T))
  
  lng <- print(length(fls))
  
  if(lng != 28){stop()}
  
  vls <- purrr::map(.x = 1:length(fls), .f = function(j){
    
    cat(j, '\n')
    nme <- glue('bio_{j}')
    rst <- terra::rast(fls[j])
    names(rst) <- nme
    vls <- terra::extract(rst, pnt[,c('coor_x', 'coor_y')])
    rm(rst); gc()
    return(vls)

  })
  
  vls <- vls %>% purrr::reduce(., inner_join, by = 'ID') %>% as_tibble()
  vls <- as_tibble(cbind(pnt, vls))
  vls <- mutate(vls, year = yea)
  cat("Done!\n")
  return(vls)
  
})

map_dbl(vles, nrow); map_dbl(vles, ncol)

# To join all the tables into only one
vles <- bind_rows(vles)
write.xlsx(vles, './tble/values_points.xlsx')

# To remove duplicated by cell --------------------------------------------
mask <- terra::rast(fles[1]) * 0 + 1
clls <- terra::extract(mask, vles[,c('coor_x', 'coor_y')], cells = T)
vles <- mutate(vles, cell = clls$cell)

clss <- unique(vles$sustratoOrden_rcl)

rslt <- purrr::map_dfr(.x = 1:length(clss), .f = function(i){
  cat(i, '\n')
  vls <- filter(vles, sustratoOrden_rcl == clss[i])
  dup <- duplicated(vls$cell)
  vls <- vls[!dup,]
  return(vls)
})

