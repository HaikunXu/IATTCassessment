#' Make the management table based on SS output (SAC11)
#' 
#' \code{makeManagTable.new} This code make the mangament table for IATTC stock assessments (SAC11)
#' 
#' @export

makeManagTable.new <- function(Path, FFleets, FlimitPath, dMSYPath, F30Path = NA) {
    replist <- r4ss::SS_output(dir = Path, covar = T, printstats = F, verbose = FALSE)
    TimeSeries <- replist$timeseries
    # numFleets <- replist$nfleets # all fleets including surveys
    # numFleets <- replist$nfishfleets  # only fisheries fleets <>< Change 15 March 2016
    endYr <- replist$endyr
    startYr <- replist$startyr
    
    fyear <- floor(1975 + (startYr/4) - 0.25)
    lyear <- floor(1975 + (endYr/4) - 0.25)
    
    # Make forecast management report name
    ForeRepName <- paste(Path, "Forecast-report.SSO", sep = "")
    # Get management report
    ForeRepStart <- grep("Management_report", readLines(ForeRepName))
    ForeRepEnd <- grep("THIS FORECAST IS FOR PURPOSES", readLines(ForeRepName))[1]
    
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
    
    # Crecent <- sum(ForeTS[ForeTS$Yr %in% seq(endYr - 3, endYr, 1), 5:dim(ForeTS)[2]])
    
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
    # CrecentMsy <- Crecent/msy
    # Brecent/Bmsy
    # SrecentSzero <- Srecent/Szero
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
    
    STD <- read.table(file = paste0(Path,"ss3.std"),skip = 1)
    names(STD) <- c("index", "name", "value", "std")
    
    FrecentFmsy_line <- which(STD$name=="F_std")[endYr-startYr+1] # the last 12 quarters
    FrecentFmsy <- STD$value[FrecentFmsy_line]
    FrecentFmsy_std <- STD$std[FrecentFmsy_line]
    
    Fmult <- 1/FrecentFmsy
    
    # get the std of Srecent/S0; 5/1/2025
    SrecentS0_line <- which(STD$name=="depletion")[endYr-startYr+2] # the first forecast year
    SrecentS0 <- STD$value[SrecentS0_line]
    SrecentS0_std <- STD$std[SrecentS0_line]
    
    ### carolina's code to add S0_dynamic
    RepName <- paste0(Path, "Report.sso")
    RepStart <- grep("Spawning_Biomass_Report_1 No_fishery_for_Z=M_and_dynamic_Bzero", readLines(RepName))
    RepStart <- RepStart+2
    RepEnd <- grep("NUMBERS_AT_AGE_Annual_1 No_fishery_for_Z=M_and_dynamic_Bzero", readLines(RepName))
    RepEnd <- RepEnd-2
    #RepDat<-readr::read_table2(RepName,col_names=FALSE,skip=RepStart,
    #                          n_max = (RepEnd - RepStart),skip_empty_rows=FALSE)
    RepDat<-read.table(RepName,header=FALSE,skip=RepStart,
                       nrows = (RepEnd - RepStart),blank.lines.skip=FALSE)
    names(RepDat) = c("Yrr","type","S")
    S0_d<-as.numeric(RepDat$S[RepDat$Yrr==endYr+1])
    
    SrecentdS0 <- Srecent/S0_d
    
    # get Srecent/Slimit (5/5/2020)
    cor_mat <- read.table(paste0(Path, "ss3.std"), skip = 1, fill = NA, header = FALSE)
    names(cor_mat) <- c("index","name","value","std.dev")
    SrecentSlim <- cor_mat$value[max(which(cor_mat$name == "depletion"))]/0.077
    SrecentSlim_std <- cor_mat$std.dev[max(which(cor_mat$name == "depletion"))]/0.077 # std(x/c)=std(x)/c
    
    SrecentS0 <- cor_mat$value[max(which(cor_mat$name == "depletion"))]
    SrecentS0_std <- cor_mat$std.dev[max(which(cor_mat$name == "depletion"))]
    
    Prob_Slimit <- pnorm(1,SrecentSlim,SrecentSlim_std) # P(Scur<Slimit)
    
    
    # Get Frecent/Flimit (5/5/2020)
    STD <- read.table(file = paste0(FlimitPath,"ss3.std"),skip = 1)
    names(STD) <- c("index", "name", "value", "std")
    
    FrecentFlim_line <- which(STD$name=="F_std")[endYr-startYr+1] # the last 12 quarters
    FrecentFlim <- STD$value[FrecentFlim_line]
    FrecentFlim_std <- STD$std[FrecentFlim_line]
    Prob_Flimit <- 1 - pnorm(1,FrecentFlim,FrecentFlim_std) # P(Fcur>Flimit)
    
    
    # Get Frecent/F30% (5/6/2025)
    if(is.na(F30Path) == TRUE) {
      FrecentF30 <- NA
      FrecentF30_std <- NA
    }
    else {
      STD <- read.table(file = paste0(F30Path,"ss3.std"),skip = 1)
      names(STD) <- c("index", "name", "value", "std")
      
      FrecentF30_line <- which(STD$name=="F_std")[endYr-startYr+1] # the last 12 quarters
      FrecentF30 <- STD$value[FrecentF30_line]
      FrecentF30_std <- STD$std[FrecentF30_line]
    }
    
    
    ### dynamic SMSY (5/13/2020); from function makeManagTable.new
    Dynamic.rep <- r4ss::SS_output(dir = dMSYPath, covar = F, verbose = F, printstats = F)  # dyanmic Smsy
    
    dSPBdat <- Dynamic.rep$timeseries
    dSPBdat$Yr2 <- 1975 + (dSPBdat$Yr/4) - 0.25
    x <- dSPBdat$Yr2[(3+(lyear-fyear+1)*4):(length(dSPBdat$Yr2))]
    # y <- dSPBdat$SpawnBio[(3+(lyear-fyear+1)*4):(length(dSPBdat$SpawnBio))]
    
    # new dmsy code updated in May 7, 2024
    ForeRepName <- paste(dMSYPath, "Forecast-report.SSO", sep = "")
    # Get management report
    ForeRepStart <- grep("FORECAST:_With_Constant_F=Fofl;_No_Input_Catches_or_Adjustments;_Equil_Recr;_No_inpl_error", readLines(ForeRepName))
    ForeRepEnd <- grep("FORECAST:_With_F=Fabc;_With_Input_Catches_and_Catch_Adjustments;_Equil_Recr;_No_inpl_error", readLines(ForeRepName))[1]
    
    ForeDat <- read.table(file = ForeRepName, header = TRUE, fill = T, quote = "", colClasses = "character", 
                          nrows = ForeRepEnd - ForeRepStart - 2, skip = ForeRepStart)
    ForeDat <- as.data.frame(ForeDat)
    y <- as.numeric(ForeDat$SpawnBio)
    
    x2 <- unique(floor(x)) 
    y2 <- y[x %in% x2]
    dSpawnBioYr.Out <- cbind(x2-(lyear-fyear+1), y2)
    dSpawnBioYr.Out <- data.frame(dSpawnBioYr.Out)
    names(dSpawnBioYr.Out) <- c("Year", "SB")
    
    SrecentdSmsy <- Srecent/dSpawnBioYr.Out[lyear-fyear+2,2]
    ###
    
    # add targer probabilities (5/22/2020)
    Prob_Ftarget <- 1 - pnorm(1,FrecentFmsy,FrecentFmsy_std) # P(fcur>Ftarget)
    Prob_Starget <- pnorm(1,SrecentdSmsy,SrecentdSmsy*FrecentFmsy_std/FrecentFmsy) # P(Scur<Starget); assume CV(S/Smsy)=CV(F/Fmsy)
    
    ### dynamic MSY (5/13/2020); from function makeManagTable.new
    # get Ccurrent
    Cdat <- TimeSeries
    Cdat$Yr2 <- 1975 + (Cdat$Y/4) - 0.25
    Ccol <- which(substr(names(Cdat),start=1,stop=7)=="dead(B)")
    x <- Cdat$Yr2[3:(length(Cdat$Yr2))]
    y <- Cdat[3:nrow(Cdat),Ccol]
    x2 <- unique(floor(x))
    y2 <- x2
    for (yy in 2:length(x2)) y2[yy] <- sum(y[floor(x) %in% (x2[yy]-1),]) # annual catch
    Crecent <- y2[length(x2)] # MSY in lyear
    # get MSY
    MSYdat <- Dynamic.rep$timeseries
    MSYdat$Yr2 <- 1975 + (MSYdat$Y/4) - 0.25
    x <- MSYdat$Yr2[(3+(lyear-fyear+1)*4):(length(MSYdat$Yr2))]
    y <- MSYdat[(3+(lyear-fyear+1)*4):nrow(MSYdat),Ccol]
    x2 <- unique(floor(x))
    y2 <- x2
    for (yy in 2:length(x2)) y2[yy] <- sum(y[floor(x) %in% (x2[yy]-1),]) # annual projected catch under FMSY
    msy_d <- y2[length(x2)] 
    
    CrecentMsy <- Crecent/msy_d
    
    # Make table with management quantities
    RowNames <- c("msy", "msy_d", "Smsy", "Srecent/Szero", "Smsy/Szero", "Crecent/msy_d", "Brecent/Bmsy",
                  "Srecent/Smsy", "Fmultiplier","Szero", "Szero_dynamic","Srecent/dSmsy","Srecent/Slim",
                  "P(Srecent<Slim)", "Frecent/Fmsy", "Frecent/Fmsy std","Frecent/Flim","P(Frecent>Flim)",
                  "Srecent/dS0","P(Srecent<Starget)","P(Frecent>Ftarget)", "Srecent/Slimit std","Frecent/Flimit std", "Srecent/Szero std",
                  "FrecentF30", "FrecentF30_std")
    
    ManagTable <- matrix(NA, length(RowNames), 2)
    ManagTable <- data.frame(ManagTable)
    names(ManagTable) <- c("quant", "val")
    # Populate table with quantities
    ManagTable[, 1] <- RowNames
    ManagTable[1, 2] <- format(msy)
    ManagTable[2, 2] <- format(msy_d)
    ManagTable[3, 2] <- format(Smsy, digits = 1)
    ManagTable[4, 2] <- format(SrecentS0, digits = 8, nsmall = 8)
    ManagTable[5, 2] <- format(SmsySzero, digits = 8, nsmall = 8)
    ManagTable[6, 2] <- format(CrecentMsy, digits = 8, nsmall = 8)
    ManagTable[7, 2] <- format(NA, digits = 8, nsmall = 8)
    ManagTable[8, 2] <- format(SrecentSmsy, digits = 8, nsmall = 8)
    ManagTable[9, 2] <- format(Fmult, digits = 8, nsmall = 8)
    ManagTable[10, 2] <- format(Szero, digits = 8, nsmall = 8)
    ManagTable[11, 2] <- format(S0_d, digits = 8, nsmall = 8)
    ManagTable[12, 2] <- format(SrecentdSmsy, digits = 8, nsmall = 8)
    ManagTable[13, 2] <- format(SrecentSlim, digits = 8, nsmall = 8)
    ManagTable[14, 2] <- format(Prob_Slimit, digits = 8, nsmall = 8)
    ManagTable[15, 2] <- format(FrecentFmsy, digits = 8, nsmall = 8)
    ManagTable[16, 2] <- format(FrecentFmsy_std, digits = 8, nsmall = 8)
    ManagTable[17, 2] <- format(FrecentFlim, digits = 8, nsmall = 8)
    ManagTable[18, 2] <- format(Prob_Flimit, digits = 8, nsmall = 8)
    ManagTable[19, 2] <- format(SrecentdS0, digits = 8, nsmall = 8)
    ManagTable[20, 2] <- format(Prob_Starget, digits = 8, nsmall = 8)
    ManagTable[21, 2] <- format(Prob_Ftarget, digits = 8, nsmall = 8)
    ManagTable[22, 2] <- format(SrecentSlim_std, digits = 8, nsmall = 8)
    ManagTable[23, 2] <- format(FrecentFlim_std, digits = 8, nsmall = 8)
    ManagTable[24, 2] <- format(SrecentS0_std, digits = 8, nsmall = 8)
    ManagTable[25, 2] <- format(FrecentF30, digits = 8, nsmall = 8)
    ManagTable[26, 2] <- format(FrecentF30_std, digits = 8, nsmall = 8)
    
    Out <- list(Fvector = Fvector, FmultScale = FmultScale, ManagTable = ManagTable)
    
    return(Out)
}
