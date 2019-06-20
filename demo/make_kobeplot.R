
############################################
## RUN
############################################

# Define the directory
# SAC4
BasePath <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/stock assessment/BET Assessment/SS_work/BET_SAC9/Final Models/BET_base/"

KobePath <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/stock assessment/BET Assessment/SS_work/BET_SAC9/Final Models/KOBE_PLOT/BC Run change Starter/"

# Arguments to pass function
year1 <- 1975

MakeKobeTbls <- function(year1, BasePath, KobePath)
{
	#####################################################################################################################
	# STEP 1 - Get time series of BioSmr and SBR from the base case run
	#####################################################################################################################
	
	# Get the base case rep list
	print(c("Getting the base case rep list"))
	BaseCase.rep <- SS_output(dir=BasePath,ncols=215,covar=F, verbose=F, printstats=F)   # Need base case BaseCase.rep to extract endyr
	print(c("Base case rep list was read"))

	# Get BioSmr series
	startYr <- BaseCase.rep$startyr
	endYr <- BaseCase.rep$endyr
	numSeasons <- BaseCase.rep$nseasons
	numAreas <- BaseCase.rep$nareas
	TSraw <- BaseCase.rep$timeseries
	TSraw$Yr <- TSraw$Yr + (TSraw$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat <- TSraw[TSraw$Yr <= endYr+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat <- TSdat[TSdat$Seas==1 & TSdat$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	BioSmry <- TSdat$Bio_smry
	x <- Years
	y <- BioSmry[3:length(BioSmry)]
	# Get the yearly values
	x2 <- unique(floor(x))
	y2 <- y[x%in%x2]
	BioSmryYr.Out <- cbind(x2,y2)
	BioSmryYr.Out <- as.data.frame(BioSmryYr.Out)
	names(BioSmryYr.Out) <- c("Year","BioSmry")

	# Get SBR series
	DerivedQuants <- BaseCase.rep$derived_quants
    # Make dataframe with SpanBio, years and 95CI for all eras
    #SBRsd <- DerivedQuants
    Labels <- substring(DerivedQuants$Label,1,3)    
    SPBdat <- DerivedQuants[Labels==c("SSB"),]    
    SPBdat$Yr <- substring(SPBdat$Label,5,max(nchar(SPBdat$Label)))
    SPBdat$Yr <- as.numeric(SPBdat$Yr)		# Make column with years
    # Convert from quarters (EPO models) to year values
	# SPBdat$Yr2 <- SPBdat$Yr 
    SPBdat$Yr2 <- year1 + (SPBdat$Yr/4)-0.25
    # Get the yearly values
	x <- SPBdat$Yr2[3:(length(SPBdat$Yr2))]
	y <- SPBdat$Value[3:(length(SPBdat$Value))]
    x2 <- unique(floor(x))
	y2 <- y[x%in%x2]
	SBR <- y2/SPBdat$Value[1]
	SpawnBioYr.Out <- cbind(x2,y2,SBR)
	SpawnBioYr.Out <- as.data.frame(SpawnBioYr.Out)
	names(SpawnBioYr.Out) <- c("Year","SB","SBR")
	
	# Get the std vales
	
	#################################################################################################
	# STEP 2 - Do the interative Kobe runs
	#################################################################################################
		
	## 2.1 - Get replist from the Kobe run and create the forecast file qrt definition tables
	
	# Get the replist to extract some quantities
	Kobe.rep <- SS_output(dir=KobePath,ncols=215,forecast=F,covar=F, verbose=F, printstats=F)   # Need base case replist to extract endyr
	QrtsMat <- matrix(0,0,3) # Output table
	vecTemp <- rep(0,3)

	for(i in 1:1000)  # Made it 1000, but will never reach it
	{
		# Fill in the elements of the current vector in the table
		vecTemp[1] <- i
		if(i==1){vecTemp[2] <- 1}
		if(i!=1){vecTemp[2] <- QrtsMat[i-1,2]+4}
		vecTemp[3] <- vecTemp[2]-1+12
	
		# Bind the current vector to the output table
		QrtsMat <- rbind(QrtsMat,as.numeric(vecTemp))
		#print(i)
	
		if(vecTemp[3]==Kobe.rep$endyr){break}
	}

	# Create the two qrt forecaset definition tables that will actually be used to define the forecast file
	BmarkTable <- cbind(matrix(0,dim(QrtsMat)[1],4),QrtsMat[,2:3])
	FcastTable <- cbind(matrix(0,dim(QrtsMat)[1],2),QrtsMat[,2:3])

	
	## 2.2 - Loop over the qrt forecast definition tables, change forecast, run model and get management quantities
	
	# Define the forecast file KobePath
	ForeKobePath <- paste(KobePath,"Forecast.SS",sep="")
	# Define the output table for management quantities
	MSYtableOut <- matrix(0,9,0)
	MSYtableOut <- as.data.frame(MSYtableOut)
	
	# Loop over the elments of the qrt definition tables (BmarkTable,FcastTable)
	for(i in 1:dim(QrtsMat)[1])
	# for(i in 1:2)
	{
		print(paste("run", i,"out of",dim(QrtsMat)[1],"runs"))
		
		# Get the forecast indices to paste into the forecast file
		QrtsTemp.Bmark <- BmarkTable[i,]	# for Bmark line
		QrtsTemp.Fcast <- FcastTable[i,]	# for Fcast line
    QrtsTemp.Bmark
    QrtsTemp.Fcast
		# Read in the forecast files
		LinesFore <- readLines(ForeKobePath, warn=F)
		
		# Find the Bmark line
		BmarkLine <- match("#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF (enter actual year, or values of 0 or -integer to be rel. endyr)", LinesFore)
		# Paste forecast indices into Bmark line
		LinesFore[BmarkLine+1] <- paste(QrtsTemp.Bmark,collapse=" ")

		# Find the Fcast line
		FcastLine <- match("#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF  (enter actual year, or values of 0 or -integer to be rel. endyr)", LinesFore)
		# Paste forecast indices into Fcast line
		LinesFore[FcastLine+1] <- paste(QrtsTemp.Fcast,collapse=" ")
	
		# Write the forecast file with updated qrt forecast indices	
		writeLines(LinesFore,ForeKobePath)
		
		# Run current model
		command <- paste("cd",KobePath,"& go_noHess.bat", sep=" ")
		x <- shell(cmd=command, intern=T, wait=T)

		# Read in the management quantities
		Kobe.rep <- SS_output(dir=KobePath, ncols=215, covar=F, verbose=F, printstats=F)                                                          
		MSYtableTemp <- makeManagTable(replist=Kobe.rep, Path=KobePath)		
		MSYtableTemp <- as.numeric(MSYtableTemp$ManagTable[,2])
		# cbind the management table
		MSYtableOut <- cbind(MSYtableOut,MSYtableTemp)
		names(MSYtableOut)[i] <- paste("run",i)
	}
	
	# NEED TO GET A VECTOR OF YEAR LabelS
	# Get the years corresponding to the 3-yr averages
	#YearsAvg <- seq(year1+2,1,dim(MSYtableOut)[2]-1)
	#for(i in 1:length(YearsAvg)){YearsAvg[i]<-yearStart+4
	#year1
	
	# Add row labes to MSYtableOut
	# Make table with management quantities
	RowNames <- c("msy","Bmsy","Smsy","Bmsy/Bzero","Smsy/Szero","Crecent/msy",
					"Brecent/Bmsy", "Srecent/Smsy", "Fmultiplier")
	MSYtableOut <- cbind(RowNames,MSYtableOut)
	write.table(MSYtableOut,"YFT_KobePlotOut.csv", sep=",")
	# For debugging
	#MSYtableOut <- read.table("C:/Users/alexdasilva/Documents/IATTC/IATTC_2012/MEETINGS/SAC3/SS_work/BET/KOBE_PLOT/BET_KobePlotOut.csv", sep=",")

	#################################################################################################
	# STEP 3 - Compute the time series to make the Kobe plots
	#################################################################################################
		
	SoverSmsy <- rep(0,dim(MSYtableOut)[2]-1)
	for(i in 1:length(SoverSmsy)) {SoverSmsy[i] <- SpawnBioYr.Out[i+3,3]/MSYtableOut[5,i+1]}
	
	BoverBmsy <- rep(0,dim(MSYtableOut)[2]-1)
	for(i in 1:length(BoverBmsy)) {BoverBmsy[i] <- BioSmryYr.Out[i+3,2]/MSYtableOut[2,i+1]}
	
	FmultInv <- 1/as.numeric(MSYtableOut[9,2:dim(MSYtableOut)[2]])
 
	Kobe.Out <- list(BioSmryYr.Out=BioSmryYr.Out, SpawnBioYr.Out=SpawnBioYr.Out, MSYtableOut=MSYtableOut, SoverSmsy=SoverSmsy, BoverBmsy=BoverBmsy, FmultInv=FmultInv)
	return(Kobe.Out) 
}	
Kobe.Out <- MakeKobeTbls(year1=1975, BasePath=BasePath, KobePath=KobePath)