#' Make Kobe table for sAC11
#' 
#' \code{make_kobetable_SAC11} This function automayically generates the table for kobe plot
#' 
#' @export

make_kobetable_SAC11 <- function(Path, KobePath, FFleets, STD_only = TRUE, newSS, FstdPath, FlimitPath, DynamicPath) {
  ##################################################################################################################### STEP 1 - Get time series of BioSmr and SBR from the base case run
  if(STD_only==FALSE) print("change starter file (use par and do not estimate) in KobePath before this section!!!")
  
  # Get the base case rep list
  # print(c("Getting the base case rep list"))
  BaseCase.rep <- r4ss::SS_output(dir = Path, ncols = 400, covar = F, verbose = F, printstats = F)  # Need base case BaseCase.rep to extract endyr
  Dynamic.rep <- r4ss::SS_output(dir = DynamicPath, ncols = 400, covar = F, verbose = F, printstats = F)  # dyanmic Smsy
  
  # print(c("Base case rep list was read"))
  
  # Get BioSmr series
  startYr <- BaseCase.rep$startyr
  endYr <- BaseCase.rep$endyr
  fyear <- floor(1975 + (startYr/4) - 0.25)
  lyear <- floor(1975 + (endYr/4) - 0.25)
  numSeasons <- BaseCase.rep$nseasons
  numAreas <- BaseCase.rep$nareas
  TSraw <- BaseCase.rep$timeseries
  TSraw$Yr <- TSraw$Yr + (TSraw$Seas - 1)/numSeasons
  # Subset for Era of interest - Option 1 (HISTORICAL ERA)
  TSdat <- TSraw[TSraw$Yr <= endYr + 1, ]  # Subset TimeSeries for historical era (include first forecast yr)\t
  TSdat <- TSdat[TSdat$Seas == 1 & TSdat$Area == 1, ]
  # Subset Years and Total Biomass quantities for plot
  YearsRaw <- TSdat$Yr
  numYears <- length(YearsRaw) - 2  # Get number of years (drop VIRG and INIT)
  Years <- fyear + 0:(numYears - 1)/4  # Convert from quarters (EPO models) to year values
  BioSmry <- TSdat$Bio_smry
  x <- Years
  y <- BioSmry[3:length(BioSmry)]
  # Get the yearly values
  x2 <- unique(floor(x))
  y2 <- y[x %in% x2]
  BioSmryYr.Out <- cbind(x2, y2)
  BioSmryYr.Out <- as.data.frame(BioSmryYr.Out)
  names(BioSmryYr.Out) <- c("Year", "BioSmry")
  
  # Get SBR series
  
  SPBdat <- BaseCase.rep$timeseries
  SPBdat$Yr2 <- 1975 + (SPBdat$Y/4) - 0.25
  x <- SPBdat$Yr2[3:(length(SPBdat$Yr2))]
  y <- SPBdat$SpawnBio[3:(length(SPBdat$SpawnBio))]
  x2 <- unique(floor(x))
  y2 <- y[x %in% x2]
  SBR <- y2/SPBdat$SpawnBio[1]
  SpawnBioYr.Out <- cbind(x2, y2, SBR)
  SpawnBioYr.Out <- as.data.frame(SpawnBioYr.Out)
  names(SpawnBioYr.Out) <- c("Year", "SB", "SBR")
  
  # Get dyanmic SBR_msy series
  dSPBdat <- Dynamic.rep$timeseries
  dSPBdat$Yr2 <- 1975 + (dSPBdat$Y/4) - 0.25
  x <- dSPBdat$Yr2[(3+(lyear-fyear+1)*4):(length(dSPBdat$Yr2))]
  y <- dSPBdat$SpawnBio[(3+(lyear-fyear+1)*4):(length(dSPBdat$SpawnBio))]
  x2 <- unique(floor(x))
  y2 <- y[x %in% x2]
  # for (yy in 1:length(x2)) y2[yy] <- mean(y[floor(x) %in% (x2[yy]-1)])
  dSBR <- y2/dSPBdat$SpawnBio[1]
  dSpawnBioYr.Out <- cbind(x2, y2, dSBR)
  dSpawnBioYr.Out <- as.data.frame(dSpawnBioYr.Out)
  names(dSpawnBioYr.Out) <- c("Year", "SB", "SBR")
  
  # Get the std vales
  if(STD_only==TRUE) {
    if(newSS==FALSE) {
      Table <- makeManagTable(Path, FFleets = FFleets)
      print("************do not use the new ss************")
      Fmult_scale <- Table$FmultScale
      STD_Table <- data.frame(read.table(file = paste0(Path,"ss.std"),header = TRUE))
      f_index <- which(STD_Table$name=="Mgmt_quant"&STD_Table$value>0)
      F_last <- STD_Table$value[f_index[14]]
      F_last_SD <- STD_Table$std.dev[f_index[14]]
      F_mult_recentSD <- Fmult_scale*F_last_SD/F_last^2
      F_mult_last <- as.numeric(Table$ManagTable$val[which(Table$ManagTable$quant=="Fmultiplier")])
      F_mult_low <- F_mult_last-1.96*F_mult_recentSD
      F_mult_high <- F_mult_last+1.96*F_mult_recentSD
    }
    else {
      Table <- makeManagTable.new(Path, FFleets = FFleets, FstdPath, FstdPath)
      STD_Table <- data.frame(read.table(file = paste0(Path,"ss.std"),header = TRUE))
      print("************do use the new ss************")
      FrecentFmsy <- as.numeric(Table$ManagTable$val[which(Table$ManagTable$quant=="FrecentFmsy")])
      FrecentFmsy_std <- as.numeric(Table$ManagTable$val[which(Table$ManagTable$quant=="FrecentFmsy_std")])
      FrecentFmsy_low <- FrecentFmsy-1.96*FrecentFmsy_std
      FrecentFmsy_high <- FrecentFmsy+1.96*FrecentFmsy_std
    }
    
    SBR_last <- as.numeric(Table$ManagTable$val[which(Table$ManagTable$quant=="Srecent/Smsy")])
    SBR_CV <- STD_Table$std.dev[which(STD_Table$name=="depletion")[(lyear-fyear+1)*4]]/STD_Table$value[which(STD_Table$name=="depletion")[(lyear-fyear+1)*4]]
    SBR_recent_low <- SBR_last-1.96*SBR_last*SBR_CV
    SBR_recent_high <- SBR_last+1.96*SBR_last*SBR_CV
    
    if(newSS==FALSE) STD <- data.frame("Fmultiplier"=c(F_mult_low,F_mult_last,F_mult_high),
                                       "SB"=c(SBR_recent_low,SBR_last,SBR_recent_high))
    else STD <- data.frame("FrecentFmsy"=c(FrecentFmsy_low,FrecentFmsy,FrecentFmsy_high),
                           "SB"=c(SBR_recent_low,SBR_last,SBR_recent_high))
    
    Kobe.Out <- list(STD=STD)
  }
  
  ################################################################################################# STEP 2 - Do the interative Kobe runs
  else { # run the kobe trajectory code
    ## 2.1 - Get replist from the Kobe run and create the forecast file qrt definition tables
    
    # Get the replist to extract some quantities
    Kobe.rep <- r4ss::SS_output(dir = KobePath, ncols = 400, forecast = F, covar = F, verbose = F, printstats = F)  # Need base case replist to extract endyr
    QrtsMat <- matrix(0, 0, 3)  # Output table
    vecTemp <- rep(0, 3)
    
    for (i in 1:1000) {
      # Fill in the elements of the current vector in the table
      vecTemp[1] <- i
      if (i == 1) {
        vecTemp[2] <- 1
      }
      if (i != 1) {
        vecTemp[2] <- QrtsMat[i - 1, 2] + 4
      }
      vecTemp[3] <- vecTemp[2] - 1 + 12
      
      # Bind the current vector to the output table
      QrtsMat <- rbind(QrtsMat, as.numeric(vecTemp))
      # print(i)
      
      if (vecTemp[3] == Kobe.rep$endyr) {
        break
      }
    }
    
    QrtsMat <- QrtsMat[(fyear-1975+1):nrow(QrtsMat),]
    QrtsMat[,1] <- QrtsMat[,1] - fyear + 1975
    
    
    # Create the two qrt forecaset definition tables that will actually be used to define the forecast file
    BmarkTable <- cbind(matrix(QrtsMat[, 3], dim(QrtsMat)[1], 4), QrtsMat[, 2:3], 1, QrtsMat[, 3], 1, QrtsMat[, 
                                                                                                              3])
    FcastTable <- cbind(matrix(0, dim(QrtsMat)[1], 2), QrtsMat[, 2:3], -999, 0)
    
    
    ## 2.2 - Loop over the qrt forecast definition tables, change forecast, run model and get management quantities
    
    # Define the forecast file KobePath
    ForeKobePath <- paste(KobePath, "Forecast.SS", sep = "")
    # Define the output table for management quantities
    MSYtableOut <- matrix(0, 9, 0)
    MSYtableOut <- as.data.frame(MSYtableOut)
    
    # Loop over the elments of the qrt definition tables (BmarkTable,FcastTable)
    for (i in 1:dim(QrtsMat)[1]) # for(i in 1:3)
    {
      print(paste("run", i, "out of", dim(QrtsMat)[1], "runs"))
      
      # Get the forecast indices to paste into the forecast file
      QrtsTemp.Bmark <- BmarkTable[i, ]  # for Bmark line
      QrtsTemp.Fcast <- FcastTable[i, ]  # for Fcast line
      QrtsTemp.Bmark
      QrtsTemp.Fcast
      # Read in the forecast files
      LinesFore <- readLines(ForeKobePath, warn = F)
      
      # Find the Bmark line
      BmarkLine <- match("#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF, beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr)", 
                         LinesFore)
      # Paste forecast indices into Bmark line
      LinesFore[BmarkLine + 1] <- paste(QrtsTemp.Bmark, collapse = " ")
      
      # Find the Fcast line
      FcastLine <- match("#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF, beg_recruits, end_recruits  (enter actual year, or values of 0 or -integer to be rel. endyr)", 
                         LinesFore)
      # Paste forecast indices into Fcast line
      LinesFore[FcastLine + 1] <- paste(QrtsTemp.Fcast, collapse = " ")
      
      # Write the forecast file with updated qrt forecast indices
      writeLines(LinesFore, ForeKobePath)
      
      # Run current model
      command <- paste("cd", KobePath, "& go_noHess.bat", sep = " ")
      shell(cmd = command, intern = T, wait = T)
      
      # Read in the management quantities
      # Kobe.rep <- r4ss::SS_output(dir = KobePath, ncols = 215, covar = F, verbose = F, printstats = F)
      MSYtableTemp <- makeManagTable(Path = KobePath, FFleets = FFleets)
      dS0 <- as.numeric(MSYtableTemp$ManagTable[10,2])
      MSYtableTemp <- as.numeric(MSYtableTemp$ManagTable[1:9,2])
      # cbind the management table
      MSYtableOut <- cbind(MSYtableOut, MSYtableTemp)
      names(MSYtableOut)[i] <- paste("run", i)
      
      MSYtableOut[8,i] <- SpawnBioYr.Out[i + 2, 2]/dSpawnBioYr.Out[i + 2, 2] # new SB/SBmsy late
      MSYtableOut[5,i] <- (SpawnBioYr.Out[i + 2, 2]/dS0)/MSYtableOut[5,i] # new SB/SBmsy early: S/Smsy = (S/dS0)/(Smsy/S0)
      p <- (i - 1) / (dim(QrtsMat)[1] - 1)
      MSYtableOut[4,i] <- p
      MSYtableOut[3,i] <- (1 - p) * MSYtableOut[5,i] + p * MSYtableOut[8,i]
      }
    
    # NEED TO GET A VECTOR OF YEAR LabelS Get the years corresponding to the 3-yr averages YearsAvg <-
    # seq(fyear+2,1,dim(MSYtableOut)[2]-1) for(i in 1:length(YearsAvg)){YearsAvg[i]<-yearStart+4 fyear
    
    # Add row labes to MSYtableOut Make table with management quantities
    RowNames <- c("msy", "Bmsy", "Srecent/dSmsy", "p", "Srecent/dSmsy1", "Crecent/msy", "Brecent/Bmsy", "Srecent/dSmsy2", 
                  "Fmultiplier")
    MSYtableOut <- cbind(RowNames, MSYtableOut)
    write.csv(MSYtableOut, paste0(KobePath, "KobePlotOut.csv"), row.names = FALSE)
    # For debugging MSYtableOut <-
    # read.table('C:/Users/alexdasilva/Documents/IATTC/IATTC_2012/MEETINGS/SAC3/SS_work/BET/KOBE_PLOT/BET_KobePlotOut.csv',
    # sep=',')
    
    ################################################################################################# STEP 3 - Compute the time series to make the Kobe plots
    
    SoverSmsy <- rep(0, dim(MSYtableOut)[2] - 1)
    for (i in 1:length(SoverSmsy)) {
      SoverSmsy[i] <- SpawnBioYr.Out[i + 3, 3]/MSYtableOut[5, i + 1]
    }
    
    BoverBmsy <- rep(0, dim(MSYtableOut)[2] - 1)
    for (i in 1:length(BoverBmsy)) {
      BoverBmsy[i] <- BioSmryYr.Out[i + 3, 2]/MSYtableOut[2, i + 1]
    }
    
    FmultInv <- 1/as.numeric(MSYtableOut[9, 2:dim(MSYtableOut)[2]])
    
    Kobe.Out <- list(BioSmryYr.Out = BioSmryYr.Out, SpawnBioYr.Out = SpawnBioYr.Out, MSYtableOut = MSYtableOut, 
                     SoverSmsy = SoverSmsy, BoverBmsy = SoverSmsy, FmultInv = FmultInv)
  }
  
  return(Kobe.Out)
}