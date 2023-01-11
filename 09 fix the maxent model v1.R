
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, elevatr, usdm, rJava, dismo, rmapshaper, terra, gtools, rnaturalearth, climateR, openxlsx, RSAGA, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1, encoding = 'latin1')

# Load data ---------------------------------------------------------------

# Climate
fles <- c(dir_ls('./tif/terraclimate/extent/individual/bios', regexp = '.tif$'),
          dir_ls('./tif/terraclimate/extent/individual/bios/v2', regexp = '.tif$')) %>% as.character() %>% mixedsort()

# Presences
pnts <- read_excel('tble/points/occr_back_species.xlsx')
vars <- read_excel('tble/points/vars_species.xlsx')
spcs <- unique(pnts$specie)

yrss <- 2016:2019

# Function  ---------------------------------------------------------------
fixModel <- function(spce, year){
  
  spce <- spcs[1]
  year <- 2016
  
  # Filtering presences / pseudo-absences
  cat(spce, '\n')
  tble <- filter(pnts, specie == spce)
  occr <- filter(tble, pb == 1)
  back <- filter(tble, pb == 0)
  
  # Cross-validation
  fld_occ <- kfold(occr, k = 5)
  fld_bck <- kfold(back, k = 5)
  
  purrr::map(.x = 1:5, .f = function(i){
    
    # Cross-select
    tst <- occr[fld_occ == i,]
    trn <- occr[fld_occ != i,]
    tst_bck <- back[fld_bck == i,]
    trn_bck <- back[fld_bck != i,]
    
    # Presences and pseudoabsences
    env <- rbind(trn, trn_bck)
    y <- c(trn$pb, trn_bck$pb)
    
    # To extract the values from the presences # Climate 
    flss <- grep(year, fles, value = T)
    rstr <- terra::rast(flss)
    names(rstr) <- gsub(paste0('_', year,  '.tif'), '', names(rstr))
    vles <- terra::extract(rstr, env[,c('x', 'y')])
    vles <- as_tibble(vles)
    vles <- dplyr::select(vles, bios_1:bios_19, bio_21:bio_29)
    vles <- cbind(env, vles)
    vles <- as_tibble(vles)
    colnames(vles)[5:ncol(vles)] <- c(paste0('bio_', 1:19), paste0('bio_', 21:29))
    
    # To select just the main variables 
    vrss <- filter(vars, specie == spce) %>% pull(variables)
    vles <- dplyr::select(vles, specie:y, vrss)
    
    # Output directory 
    dout <- glue('models/maxent/{spce}/run_1/model_{i}')
    ifelse(!file.exists(dout), dir_create(dout), print('ya existe'))
    
    # To make the model
    mxnt <- maxent(as.data.frame(vles)[5:ncol(vles)], y,  argcs = c('addsamplestobackground=true', 'responsecurves'), path = dout)
    post <- grep(paste0(paste0('_', parse_number(vrss), '$'), collapse = '|'), names(rstr), value = F)
    rstr <- rstr[[post]]
    names(rstr) <- gsub('s', '', names(rstr))
    prdc <- terra::predict(mxnt, rstr)
    
  })
  
}

