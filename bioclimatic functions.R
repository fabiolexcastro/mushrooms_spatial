
read_raster <- function(x){grep(x, fles, value = TRUE) %>% mixedsort() %>% stack()}


cumTemp <- function(x) {
  
  p <- matrix(nrow = 1, ncol = 4)
  colnames(p) <- paste('bio', 21:24, sep = '')
  
  w <- x[25:36] ### tmax
  y <- x[13:24] ### tmean
  x <- x[1:12]  ### Prec-PET
  z <- x
  
  ### if the values are NA the bios are NA
  if(all(is.na(x))) {
    p[,'bio21'] <- NA
    p[,'bio22'] <- NA
    p[,'bio23'] <- NA
    p[,'bio24'] <- NA
  } else {
    
    ## cumulative deficit to determine dry season (=Bio22)
    
    # print('Bio 22...')
    
    x <- z
    lng <- length(x)
    x <- c(x, x[1:12])
    x[x>0] <- NA
    cumdef <- matrix(ncol = 12, nrow = lng)
    for (i in 1:12) {
      cumdef[, i] <- x[i:(lng + i - 1)]
    }
    p[,'bio22'] <- min(c(0,apply(cumdef, MARGIN = 1, FUN = cumsum)),na.rm=T)
    
    ## cumulative surplus to determine growing season
    x <- z
    lng <- length(x)
    x <- c(z, z[1:12])
    x[x<0] <- NA
    cumplus <- matrix(ncol = 12, nrow = lng)
    
    for (i in 1:12) {
      
      cumplus[, i] <- x[i:(lng + i - 1)]
      
    }
    
    ### If there is no dry season
    ### the length becomes 0
    ### the growing season temp is the mean of monthly mean temp
    ### the dry season max temp is the max temp of the driest month 
    
    if(p[,'bio22']==0){
      
      p[,'bio21'] <- 0
      p[,'bio23'] <- mean(y)
      p[,'bio24'] <- w[which.min(z)]
      
    } else {
      
      ### the mean temperatures for all possible seasons
      y <- c(y, y[1:12])
      n <- matrix(ncol = 12, nrow = lng)
      for (i in 1:12) {
        
        n[, i] <- y[i:(lng + i - 1)]
        
      }
      
      meantemp <- apply(n, MARGIN = 1, FUN = cumsum)
      
      ### the max temperatures for all possible seasons
      w <- c(w, w[1:12])
      n <- matrix(ncol = 12, nrow = lng)
      
      for (i in 1:12) {
        
        n[, i] <- w[i:(lng + i - 1)]
        
      }
      maxtemp <- apply(n, MARGIN = 1, FUN = cumsum)
      
      ### Consecutive months with Prec<PET (=bio21)
      x <- z
      x <- c(x, x[1:12])
      x[x>0] <- NA
      x[x<0] <- 1
      o <- matrix(ncol = 12, nrow = lng)
      
      for (i in 1:12) {
        
        o[, i] <- x[i:(lng + i - 1)]
        
      }
      
      con_months <- max(apply(o,1,cumsum),na.rm=T)
      p[,'bio21'] <- con_months
      
      ### if the dry season is 12 months the growing season mean is the mean of the wettest month
      
      if(con_months==12){
        
        p[,'bio23'] <- y[which.max(z)]
        
      } else { 
        
        ### The meantemp of the wettest season
        p[,'bio23'] <- meantemp[which.max(apply(cumplus, MARGIN = 1, FUN = cumsum))]/(12-con_months)
        
      }
      ### The mean maxtemp of the driest season
      
      p[,'bio24'] <- maxtemp[which.min(apply(cumdef, MARGIN = 1, FUN = cumsum))]/con_months    
      
    }
    
  }
  
  return(p)
  
}

# Function to calculate ETPbios

etpvars <- function(x){
  
  p <- matrix(nrow = 1, ncol = 9)
  colnames(p) = paste('bio', 25:33, sep = '')
  
  tavg <- x[25:36] ### Temp
  prec <- x[13:24] ### PREC
  pet <- x[1:12]  ### PET
  
  ### if the values are NA the bios are NA
  if(all(is.na(x))) { 
    return(p)
  } else {
    
    window <- function(x)  { 
      lng <- length(x)
      x <- c(x,  x[1:3])
      m <- matrix(ncol = 3, nrow = lng)
      for (i in 1:3) { m[,i] <- x[i:(lng+i-1)] }
      apply(m, MARGIN = 1, FUN = sum)
    }
    
    ### BIO_23: Annual PET
    p[,1] <- sum(pet)
    ### BIO_24: PET seasonality (Coefficient of Variation)
    p[,2] <- cv(pet)
    ### BIO_25: MAX PET
    p[,3] <- max(pet)
    ### BIO_26: Min PET
    p[,4] <- min(pet)
    ### BIO_27: Range of PET (PETmax-PETmin)
    p[,5] <- p[,3]-p[,4]
    
    wet <- window(prec)
    hot <- window(tavg)/3
    pet2 <- c(pet,pet[1:2])
    
    ### BIO_28: PET of wettest quarter
    p[,6] <- sum(pet2[c(which.max(wet):(which.max(wet)+2))])
    ### BIO_29:	PET of driest quarter
    p[,7] <- sum(pet2[c(which.min(wet):(which.min(wet)+2))])
    ### BIO_30:	PET of warmest quarter
    p[,8] <- sum(pet2[c(which.max(hot):(which.max(hot)+2))])
    ### BIO_31:	PET of coldest quarter
    p[,9] <- sum(pet2[c(which.min(hot):(which.min(hot)+2))])
    
  }
  
  round(p, digits = 2)
  return(p)
  
} 

# Divid temperature variables by 10

dividVar <- function(files, nameVar){
  
  if(any(nameVar == 'prec')){
    
    print('Prec')
    layers <- grep(nameVar, files, value = T) %>%
      mixedsort() %>% 
      stack()
    
    return(layers) 
    
  } else {
    
    print('Temp')
    
    layers <- grep(nameVar, files, value = T) %>%
      mixedsort() %>% 
      stack()
    
    layers <- layers/10
    
    return(layers) 
    
    
  }
  
}