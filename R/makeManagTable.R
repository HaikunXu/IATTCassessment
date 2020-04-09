#' Make the management table based on SS output
#' 
#' \code{makeManagTable} This code make the mangament table for IATTC stock assessments
#' 
#' @export

makeManagTable <- function(replist, Path, FFleets) {
    # replist <- myreplist0 Path <- Path0 Get quantities from replist
    TimeSeries <- replist$timeseries
    # numFleets <- replist$nfleets # all fleets including surveys
    # numFleets <- replist$nfishfleets  # only fisheries fleets <>< Change 15 March 2016
    endYr <- replist$endyr
    
    # Make forecast management report name
    ForeRepName <- paste(Path, "Forecast-report.SSO", sep = "")
    # Get management report
    ForeRepStart <- grep("Management_report", readLines(ForeRepName))
    ForeRepEnd <- grep("THIS FORECAST FOR PURPOSES", readLines(ForeRepName))[1]
    
    # ForeDat <- read.table(file=ForeRepName,col.names=c(seq(1,10,by=1)),fill=T,quote='',colClasses='character',
    # nrows=45, skip = ForeRepStart-1)
    ForeDat <- read.table(file = ForeRepName, col.names = c(seq(1, 10, by = 1)), fill = T, quote = "", colClasses = "character", 
        nrows = ForeRepEnd - ForeRepStart, skip = ForeRepStart - 1)
    ForeDat <- as.data.frame(ForeDat)
    
    # Make catch headers to subset
    HeadersC <- rep(NA, length(FFleets))
    for (ifleet in 1:length(HeadersC)) {
        headerTemp <- paste("sel(B):_", FFleets[ifleet], sep = "")
        HeadersC[ifleet] <- headerTemp
    }
    # Make table with forecast time series
    ForeTS <- subset(TimeSeries, select = c("Yr", "Era", "Bio_smry", "SpawnBio", HeadersC))
    # Brecent <- ForeTS[ForeTS$Yr==endYr+1,3]/1000 Srecent <- ForeTS[ForeTS$Yr==endYr+1,4]/1000 Crecent <-
    # sum(ForeTS[ForeTS$Yr%in%seq(endYr-3,endYr,1),5:dim(ForeTS)[2]])/1000
    Brecent <- ForeTS[ForeTS$Yr == endYr + 1, 3]
    Srecent <- ForeTS[ForeTS$Yr == endYr + 1, 4]
    
    Crecent <- sum(ForeTS[ForeTS$Yr %in% seq(endYr - 3, endYr, 1), 5:dim(ForeTS)[2]])
    
    options(scipen = 2)  # Do not use scientific notation in plotting
    # Get management quantities msy msy <- as.numeric(ForeDat[ForeDat[,1]==c('MSY_for_optimize'),5])*4/1000
    msy <- as.numeric(ForeDat[ForeDat[, 1] == c("MSY_for_optimize"), 2]) * 4
    # Bmsy Bmsy <- as.numeric(ForeDat[ForeDat[,1]==c('Biomass_Smry'),5])/1000
    Bmsy <- as.numeric(ForeDat[ForeDat[, 1] == c("Biomass_Smry"), 2])
    Bmsy <- Bmsy[length(Bmsy)]
    # Smsy Smsy <- as.numeric(ForeDat[ForeDat[,1]==c('SPBio'),5])/1000
    Smsy <- as.numeric(ForeDat[ForeDat[, 1] == c("SPBio"), 2])
    Smsy <- Smsy[length(Smsy)]
    # Bmsy/B0 Bzero <- as.numeric(ForeDat[ForeDat[,1]==c('BIO_Smry_unfished'),5])/1000
    Bzero <- as.numeric(ForeDat[ForeDat[, 1] == c("BIO_Smry_unfished(Bmark)"), 2])
    BmsyBzero <- Bmsy/Bzero
    # Smsy/S0
    SmsySzero <- as.numeric(ForeDat[ForeDat[, 1] == c("SPBmsy/SPB_virgin"), 2])
    CrecentMsy <- Crecent/msy
    # Brecent/Bmsy
    BrecentBmsy <- Brecent/Bmsy
    # S recent/Smsy
    SrecentSmsy <- Srecent/Smsy
    
    # Methot takes the F by fishery averaged over the given years and makes it sum to 1 So the Fmult to use is not
    # the one given in the output but Fmult/sum(F1,F2,...)  Compute the average F vector in absolute (rather than
    # scaled to 1 terms)
    FvectorRepStart <- grep("Seasonal_apicalF=Fmult", readLines(ForeRepName))
    Fvector <- read.table(file = ForeRepName, nrows = 1, skip = FvectorRepStart[1] + 1)
    Fvector <- Fvector[3:length(Fvector)]
    FmultScale <- sum(Fvector)
    # Fmultiplier
    Fmult <- as.numeric(ForeDat[ForeDat[, 1] == c("Fmult"), 2])[3]
    Fmult <- Fmult/FmultScale
    
    # Make table with management quantities
    RowNames <- c("msy", "Bmsy", "Smsy", "Bmsy/Bzero", "Smsy/Szero", "Crecent/msy", "Brecent/Bmsy", "Srecent/Smsy", 
        "Fmultiplier","NLL","R_shift")
    ManagTable <- matrix(NA, length(RowNames), 2)
    ManagTable <- data.frame(ManagTable)
    names(ManagTable) <- c("quant", "val")
    # Populate table with quantities
    ManagTable[, 1] <- RowNames
    ManagTable[1, 2] <- format(msy, digits = 1)
    ManagTable[2, 2] <- format(Bmsy, digits = 1)
    ManagTable[3, 2] <- format(Smsy, digits = 1)
    ManagTable[4, 2] <- format(BmsyBzero, digits = 2, nsmall = 2)
    ManagTable[5, 2] <- format(SmsySzero, digits = 2, nsmall = 2)
    ManagTable[6, 2] <- format(CrecentMsy, digits = 2, nsmall = 2)
    ManagTable[7, 2] <- format(BrecentBmsy, digits = 2, nsmall = 2)
    ManagTable[8, 2] <- format(SrecentSmsy, digits = 2, nsmall = 2)
    ManagTable[9, 2] <- format(Fmult, digits = 2, nsmall = 2)
    ManagTable[10, 2] <- format(replist$likelihoods_used$values[1], digits = 2, nsmall = 2)
    ManagTable[11, 2] <- format(R_shift(replist), digits = 2, nsmall = 2)
    
    Out <- list(Fvector = Fvector, FmultScale = FmultScale, ManagTable = ManagTable)

    return(Out)
}
