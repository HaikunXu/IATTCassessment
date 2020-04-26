#' Make the management table based on SS output
#' 
#' \code{makeManagTable} This code make the mangament table for IATTC stock assessments
#' 
#' @export

Path <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/Document/SS for reference points/R-1(old)/"
stdPath <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/Document/SS for reference points/R-1(new)/"
FFleets <- c(1,23)

makeManagTable <- function(Path, FFleets, stdPath, nyears) {
    replist <- r4ss::SS_output(dir = Path, ncols = 400, covar = T, printstats = F, verbose = FALSE)
    TimeSeries <- replist$timeseries
    # numFleets <- replist$nfleets # all fleets including surveys
    # numFleets <- replist$nfishfleets  # only fisheries fleets <>< Change 15 March 2016
    endYr <- replist$endyr
    
    # Make forecast management report name
    ForeRepName <- paste(stdPath, "Forecast-report.SSO", sep = "")
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
    Szero<- as.numeric(ForeDat[ForeDat[, 1] == c("SSB_unfished(Bmark)"),2])
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
    # # Fmultiplier
    # Fmult <- as.numeric(ForeDat[ForeDat[, 1] == c("Fmult"), 2])[3]
    # Fmult <- Fmult/FmultScale
    
    # new code to extract the std of F multiplier using the new ss; 04/26/2020
    
    STD <- read.table(file = paste0(stdPath,"ss.std"),skip = 1)
    names(STD) <- c("index", "name", "value", "std")
    
    FrecentFmsy_line <- which(STD$name=="F_std")[nyears] # the last 12 quarters
    FrecentFmsy <- STD$value[FrecentFmsy_line]
    FrecentFmsy_std <- STD$std[FrecentFmsy_line]
    
    Fmult <- 1/FrecentFmsy
        
    ### carolina's code to add S0_dynamic
    RepName <- paste0(Path, "Report.sso")
    RepStart <- grep("Spawning_Biomass_Report_1 No_fishery_for_Z=M_and_dynamic_Bzero", readLines(RepName))
    RepStart<- RepStart+2
    RepEnd <- grep("NUMBERS_AT_AGE_Annual_1 No_fishery_for_Z=M_and_dynamic_Bzero", readLines(RepName))
    RepEnd<- RepEnd -2
    #RepDat<-readr::read_table2(RepName,col_names=FALSE,skip=RepStart,
    #                          n_max = (RepEnd - RepStart),skip_empty_rows=FALSE)
    RepDat<-read.table(RepName,header=FALSE,skip=RepStart,
                       nrows = (RepEnd - RepStart),blank.lines.skip=FALSE)
    names(RepDat) = c("Yrr","type","S")
    S0_d<-as.numeric(RepDat$S[RepDat$Yrr==endYr+1])
    
    SrecentSlim <- Srecent/(0.077*Szero)
    ###
    
    # Make table with management quantities
    RowNames <- c("msy", "Bmsy", "Smsy", "Bmsy/Bzero", "Smsy/Szero", "Crecent/msy", "Brecent/Bmsy", "Srecent/Smsy", 
                     "Fmultiplier","Szero","Szero_dynamic","Srecent/dSmsy","Srecent/Slim")
    ManagTable <- matrix(NA, length(RowNames), 2)
    ManagTable <- data.frame(ManagTable)
    names(ManagTable) <- c("quant", "val")
    # Populate table with quantities
    ManagTable[, 1] <- RowNames
    ManagTable[1, 2] <- format(msy, digits = 1)
    ManagTable[2, 2] <- format(Bmsy, digits = 1)
    ManagTable[3, 2] <- format(Smsy, digits = 1)
    ManagTable[4, 2] <- format(BmsyBzero, digits = 4, nsmall = 4)
    ManagTable[5, 2] <- format(SmsySzero, digits = 4, nsmall = 4)
    ManagTable[6, 2] <- format(CrecentMsy, digits = 4, nsmall = 4)
    ManagTable[7, 2] <- format(BrecentBmsy, digits = 4, nsmall = 4)
    ManagTable[8, 2] <- format(SrecentSmsy, digits = 4, nsmall = 4)
    ManagTable[9, 2] <- format(Fmult, digits = 4, nsmall = 4)
    ManagTable[10, 2] <- format(Szero, digits = 4, nsmall = 4)
    ManagTable[11, 2] <- format(S0_d, digits = 4, nsmall = 4)
    ManagTable[12, 2] <- format(Srecent/(S0_d*SmsySzero), digits = 4, nsmall = 4)
    ManagTable[13, 2] <- format(SrecentSlim, digits = 4, nsmall = 4)
    
    Out <- list(Fvector = Fvector, FmultScale = FmultScale, ManagTable = ManagTable)
    
    return(Out)
}
