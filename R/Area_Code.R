Areas = function(Lat,Lon,Scenario){
  Areas <- rep(1,length(Lat))
  
  if(Scenario==1) {
    i2 <- which(Lat>-10&Lat<10&Lon>-150&Lon<(-110))
    Areas[i2] <- 2
    i3 <- which(Lat>-10&Lon>-110)
    Areas[i3] <- 3
    i4 <- which(Lat<(-10)&Lon<(-110))
    Areas[i4] <- 4
    i5 <- which(Lat<(-10)&Lon>-110)
    Areas[i5] <- 5
    i6 <- which(Lon>-90&Lat<(-15))
    Areas[i6] <- 6
  }
  
  if(Scenario==2) {
    i2 <- which(Lat>-10&Lat<10&Lon>-150&Lon<(-130))
    Areas[i2] <- 2
    i3 <- which(Lat>-10&Lat<10&Lon>-130&Lon<(-110))
    Areas[i3] <- 3
    i4 <- which(Lat>-10&Lon>-110)
    Areas[i4] <- 4
    i5 <- which(Lat<(-10)&Lon<(-110))
    Areas[i5] <- 5
    i6 <- which(Lat<(-10)&Lon>-110)
    Areas[i6] <- 6
    i7 <- which(Lon>-90&Lat<(-15))
    Areas[i7] <- 7
  }
  
  if(Scenario==3) {
    i2 <- which(Lat>0&Lat<10&Lon>-150&Lon<(-110))
    Areas[i2] <- 2
    i3 <- which(Lat>-10&Lat<0&Lon>-150&Lon<(-110))
    Areas[i3] <- 3
    i4 <- which(Lat>-10&Lon>-110)
    Areas[i4] <- 4
    i5 <- which(Lat<(-10)&Lon<(-110))
    Areas[i5] <- 5
    i6 <- which(Lat<(-10)&Lon>-110)
    Areas[i6] <- 6
    i7 <- which(Lon>-90&Lat<(-15))
    Areas[i7] <- 7
  }
  
  if(Scenario==4) {
    i3 <- which(Lat<0)
    Areas[i3] <- 3
    i2 <- which(Lat>-5&Lat<5&Lon>-110)
    Areas[i2] <- 2
  }
  
  Areas
}