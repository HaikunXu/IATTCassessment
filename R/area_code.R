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
        i3 <- which(Lat < (-5) & Lat > -15 & Lon < (-105))
        Areas[i3] <- 3
        i4 <- which(Lon > -105 & Lat > -15)
        Areas[i4] <- 4
        i5 <- which(Lat < (-15) & Lon < (-90))
        Areas[i5] <- 5
        i6 <- which(Lat < (-15) & Lon > -90)
        Areas[i6] <- 6
    }
    
    if (Species == "YFT") {
        i2 <- which(Lon > -120 & Lon < -90)
        Areas[i2] <- 2
        i3 <- which(Lon > -90)
        Areas[i3] <- 3
    }
    
    return(Areas)
}
