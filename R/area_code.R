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

      Locations <- data.frame("lat" = Lat, "lon" = Lon)
      Locations <- dplyr::left_join(Locations, YFT_area)
      
      if(sum(is.na(Locations$area)) > 0) {
        print(Locations[which(is.na(Locations$area)==1),1:2])
        stop("Error YFT LL area definitions!")
      }
      
      Areas <- Locations$area
    }
    
    return(Areas)
}
