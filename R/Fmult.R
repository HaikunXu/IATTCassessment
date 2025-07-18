#' Extract the Fumltiplier from a ss run
#' 
#' \code{Fmult} This function extracts Fmult
#' 
#' @export

Fmult = function(Path) {
  
  ForeRepName <- paste(Path, "Forecast-report.SSO", sep = "")
  
  # Get management report
  ForeRepStart <- grep("Management_report", readLines(ForeRepName))
  ForeRepEnd <- grep("THIS FORECAST IS FOR PURPOSES", readLines(ForeRepName))[1]
  
  # ForeDat <- read.table(file=ForeRepName,col.names=c(seq(1,10,by=1)),fill=T,quote='',colClasses='character',
  # nrows=45, skip = ForeRepStart-1)
  ForeDat <- read.table(file = ForeRepName, col.names = c(seq(1, 10, by = 1)), fill = T, quote = "", colClasses = "character", 
                        nrows = ForeRepEnd - ForeRepStart, skip = ForeRepStart - 1)
  ForeDat <- as.data.frame(ForeDat)
  
  FvectorRepStart <- grep("Seasonal_apicalF=Fmult", readLines(ForeRepName))
  Fvector <- read.table(file = ForeRepName, nrows = 1, skip = FvectorRepStart[1] + 1)
  Fvector <- Fvector[3:length(Fvector)]
  FmultScale <- sum(Fvector) # F
  
  # # Fmultiplier
  Fmult <- as.numeric(ForeDat[ForeDat[, 1] == c("Fmult"), 2])[3] # FMSY
  Fmult <- Fmult/FmultScale # FMSY/F
  
  return(Fmult)
}