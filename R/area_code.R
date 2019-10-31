#' Generate area code for longline
#' 
#' \code{area_code} This function generates an area code for longline catch allocation
#' 
#' @export

area_code = function(Lat, Lon, Species) {
    Areas <- rep(1, length(Lat))
    
    if (Species == "BET") {
        i2 <- which(Lat > -10 & Lat < 10 & Lon > -150 & Lon < (-110))
        Areas[i2] <- 2
        i3 <- which(Lat > -10 & Lon > -110)
        Areas[i3] <- 3
        i4 <- which(Lat < (-10) & Lon < (-110))
        Areas[i4] <- 4
        i5 <- which(Lat < (-10) & Lon > -110)
        Areas[i5] <- 5
        i6 <- which(Lon > -90 & Lat < (-15))
        Areas[i6] <- 6
    }
    
    if (Species == "YFT") {
        i2 <- which(Lon > -110 & Lat > -5)
        Areas[i2] <- 2
        i3 <- which(Lat < -5)
        Areas[i3] <- 3
    }
    
    return(Areas)
}
