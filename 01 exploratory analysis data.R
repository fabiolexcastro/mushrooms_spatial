

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, terra, readxl, rgdal, rgeos, stringr, sf, tidyverse, fs, glue)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- 'E:/asesorias/byron/data/sh_tutoria_R_oct_2022'

dir_ls(path) %>% as.character()

# Matrix ambiental season 
mtrx <- grep('season.xlsx$', dir_ls(path), value = TRUE) %>% as.character()
mtrx <- read_excel(mtrx)

#  To make study area -----------------------------------------------------
dirs <- dir_ls(path) %>% 
  grep('area_proc', ., value = TRUE) %>% 
  as.character() 
shpf <- map(dirs, dir_ls)


