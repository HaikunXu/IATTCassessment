#' Generate area code for longline
#' 
#' \code{area_code} This function generates an area code for longline catch allocation
#' 
#' @export

area_code = function(Lat, Lon, Species) {
    Areas <- rep(1, length(Lat))
    
    if (Species == "BET") {
      i2 <- which(Lat > -5 & Lat < 10 & Lon < (-105))
      Areas[i2] <- 2
      i3 <- which(Lat < (-5) & Lat > -15 & Lon < (-130) & Lon > (-150))
      Areas[i3] <- 3
      i4 <- which(Lat < (-5) & Lat > -15 & Lon < (-105) & Lon > (-130))
      Areas[i4] <- 4
      i5 <- which(Lon > -105 & Lat > -15)
      Areas[i5] <- 5
      i6 <- which(Lat < (-15) & Lon < (-90))
      Areas[i6] <- 6
      i7 <- which(Lat < (-15) & Lon > (-90))
      Areas[i7] <- 7
    }
    
    if (Species == "YFT") {
      YFT_DEL <- read.csv("D:/OneDrive - IATTC/IATTC/2024/Irregular clustering/YFT DEL/cluster_YFT.csv")
      
      Locations <- data.frame("lat" = Lat, "lon" = Lon)
      Locations <- dplyr::left_join(Locations, YFT_DEL)
      
      for (i in 1:length(Lat)) {
        if((Locations$lat[i] > 10 & Locations$lon[i] > (-105))) Locations$area[i] <- 1
        if(Locations$lat[i] < 0) Locations$area[i] <- 3
        if(Locations$lon[i] < (-130)) Locations$area[i] <- 3
        if(Locations$lat[i] > 15 & Locations$lon[i] < (-125)) Locations$area[i] <- 3
        if(Locations$lat[i] > 30) Locations$area[i] <- 1
        if(Locations$lat[i] > 15 & Locations$lon[i] < (-125)) Locations$area[i] <- 3
        if(Locations$lat[i] > 5 & Locations$lat[i] < 30 & Locations$lon[i] < (-115) & Locations$lon[i] > (-125)) Locations$area[i] <- 2
        if(Locations$lat[i] < 5 & Locations$lon[i] > (-95)) Locations$area[i] <- 4
        if(Locations$lat[i] == 2.5 & Locations$lon[i] == -77.5) Locations$area[i] <- 2
        if(Locations$lat[i] > -20 & Locations$lat[i] < -10 & Locations$lon[i] < -90) Locations$area[i] <- 3
        if(Locations$lat[i] > 10 & Locations$lon[i] < (-125)) Locations$area[i] <- 5
        if(Locations$lat[i] == 12.5 & Locations$lon[i] == -127.5) Locations$area[i] <- 2
      }
      
      if(sum(is.na(Locations$area)) > 0) {
        print(Locations[which(is.na(Locations$area)==1),1:2])
        stop("Error YFT LL area definitions!")
      }
      
      Areas <- Locations$area
    }
    
    return(Areas)
}
