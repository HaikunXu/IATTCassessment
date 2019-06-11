

##### NEED R4 SS   
#install.packages("r4ss")
#require(r4ss)
#require(devtools)
#devtools::install_github("r4ss/r4ss")  # March 2016 repository for r4SS updates
#require(r4ss)


# BC SAC2
#Path <- 'W:/2011/SAC2_RUNS/BET/BASE_CASE/BET_SAC2_BC_whess/'

# Make myreplist
#myreplist = SS_output(dir=Path,ncols=215,covar=F)                                                          
#replist=myreplist
# Make plot
#SS_plots(replist=myreplist, forecastplot=F, uncertainty=F, datplot=F)


####################################################################################
### 	Useful functions needed
####################################################################################

# Defining internal functions: matchfun and matchfun2
matchfun <- function(string, obj=rawrep[,1], substr1=TRUE)
{
  # return a line number from the report file (or other file)
  # sstr controls whether to compare subsets or the whole line
  match(string, if(substr1){substring(obj,1,nchar(string))}else{obj} )
}

matchfun2 <- function(string1,adjust1,string2,adjust2,cols=NA,matchcol1=1,matchcol2=1,
  objmatch=rawrep,objsubset=rawrep,substr1=TRUE,substr2=TRUE)
{
  # return a subset of values from the report file (or other file)
  # subset is defined by character strings at the start and end, with integer
  # adjustments of the number of lines to above/below the two strings
  line1 <- match(string1,if(substr1){substring(objmatch[,matchcol1],1,nchar(string1))}else{objmatch[,matchcol1]})
  line2 <- match(string2,if(substr2){substring(objmatch[,matchcol2],1,nchar(string2))}else{objmatch[,matchcol2]})
  if(!is.na(cols[1])){ out <- objsubset[(line1+adjust1):(line2+adjust2),cols]
     }else{            out <- objsubset[(line1+adjust1):(line2+adjust2), ]}
  return(out)
}



####################################################################################
### 	Get likelihood components
####################################################################################

GetLikelihoods<-function(replist)
{
	## C.V. Minte-Vera March 15 2016
	n<-length(replist$likelihoods_by_fleet[3,])
	lambdas<-replist$likelihoods_by_fleet[3,3:n]
	Nll<-replist$likelihoods_by_fleet[4,3:n]
	Nll.survey<-lambdas*Nll
	
	lambdas<-replist$likelihoods_by_fleet[5,3:n]
	Nll<-replist$likelihoods_by_fleet[6,3:n]
	Nll.LenComps<-lambdas*Nll

	lambdas<-replist$likelihoods_by_fleet[9,3:n]
	Nll<-replist$likelihoods_by_fleet[10,3:n]
	Nll.SizeComps<-lambdas*Nll

	Overall<-rbind(Nll.survey,Nll.LenComps,Nll.SizeComps)
	row.names(Overall)<-c("survey","LenComps","SizeComps")
	return(Overall)

}
#GetLikelihoods(myreplist)




####################################################################################
### 	Plot Lenght-Weight relationship (plot1)
####################################################################################

plotLWrel <- function(replist, twoSexes)
{
	# Get quantities from replist
	biology <- replist$biology
	nsexes <- replist$nsexes

	# Prepare quantities for plot
	x <- biology$Mean_Size
	y <- biology$Wt_len_F

	# Make plot
	windows(8,5)
	par(mar=c(4.5,4.5,2,1))
	xlim <- range(pretty(x))
	ylim <- range(0,pretty(y))		
	plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25, las=1)
			
	# Two sex model
	if(nsexes > 1 & twoSexes>1)
	{
		y2 <- biology$Wt_len_M
		lines(x,y2, col="black", type="l",lty=2, lwd=1.25)
		legend("topleft",bty="n", c("Females","Males"), lty=c(1,2), col=c("black","black"))
	}
	
	# Plot Labels
	title(xlab="Length (cm)", cex.lab=1.6, line=3)
	title(ylab="Mean weight (kg)", cex.lab=1.6, line=3)
}
#plotLWrel(replist=myreplist, twoSexes=1)
   

####################################################################################
### 	Maturity ogive
####################################################################################

plotMatAtAge <- function(replist)
{
	# Get quantities from replist
	biology <- replist$biology
	nsexes <- replist$nsexes
	growdat <- replist$endgrowth
    growdat2 <- subset(growdat,growdat$Gender==1)		# Subset growdat for females only

	# Prepare quantities for plot
	if(nsexes==1)
	{
		x <- growdat$Age
		y <- growdat$Age_Mat
	}
	if(nsexes==2)
	{
		x <- growdat2$Age
		y <- growdat2$Age_Mat
	}
	# Make plot
	windows(8,5)
	par(mar=c(4.5,4.5,2,1))
	xlim <- range(pretty(x))
	ylim <- range(0,pretty(y))		
	plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25, las=1)
	
	# Plot Labels
	title(xlab="Age in quarters - Edad en trimestres", cex.lab=1.3, line=3)
	title(ylab="Proportion mature - Proporcion madura", cex.lab=1.3, line=3)

}
#plotMatAtAge(replist=myreplist)


####################################################################################
### 	Growth
####################################################################################

#PathOtolithDat <- "C:/Documents and Settings/alexdasilva/My Documents/IATTC/IATTC_2009/MEETINGS/SARM10/ASSESSMENTS_2009/BET_EPO/SS_V302D_Feb09/SS3_runs_SARM10/R_output/input/otolith_data.csv"
#PathOtolithDat <- 'C:/Users/alexdasilva/Documents/IATTC/IATTC_2011/MEETINGS/SAC2/SS_prep_work/SAC2_RUNS/YFT/YFT_otolith_data.csv'

#replist=myreplist
#PlotOtoliths=1

#myreplist
#PlotOtoliths=1
#PathOtolithDat=PathOtolithDat

plotGrowth <- function(replist, PlotOtoliths, PathOtolithDat)
{
	# Get quantities from replist
	morph_indexing <- replist$morph_indexing
	growdat <- replist$endgrowth
	nseasons <- replist$nseasons
	nsexes <- replist$nsexes

	# Read the otolith data
	if(PlotOtoliths==1){OtolithDat <- read.csv(PathOtolithDat,header=T)}
	
	# Prepare quantities for plot
	# Mid year mean length at age with 95% range of lengths (by sex if applicable)
	mainmorphs <- morph_indexing$Index[morph_indexing$Bseas==1]
	growdatF <- growdat[growdat$Morph==mainmorphs[1],]
	growdatF$Sd_Size <- growdatF$SD_Mid
	growdatF$high <- growdatF$Len_Mid + 1.96*growdatF$Sd_Size
	growdatF$low <- growdatF$Len_Mid - 1.96*growdatF$Sd_Size
	#x <- growdatF$Age 			# This was wrong
	x <- growdatF$Age_Mid 			# This was wrong

	y <- growdatF$Len_Mid
	
	# Prepare CI quantities for plot
	yCIup <- growdatF$high
	yCIlo <- growdatF$low

	# Make plot
	windows(8,5)
	par(mar=c(4.5,4.5,2,1))
	xlim <- range(pretty(x))
	ylim <- range(0,pretty(yCIup))		
	plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25, las=1)
	# Plot CIs
	polygon(c(x,rev(x)), c(yCIlo,rev(yCIup)), col='gray', border="NA")
	lines(x,y,col="black",lty=1, lwd=1.25)
	lines(x,yCIup,col="black",lty=3, lwd=1.25)
	lines(x,yCIlo,col="black",lty=3, lwd=1.25)
	# BET
	#if(PlotOtoliths==1){points(OtolithDat[,1],OtolithDat[,2], pch=19, col="black", cex=.75)}
	# YFT
	if(PlotOtoliths==1){points(OtolithDat[,1],OtolithDat[,3], pch=19, col="black", cex=.75)}
	if(PlotOtoliths==2){points(OtolithDat[,2],OtolithDat[,3], pch=19, col="black", cex=.75)}

	#if(nsexes > 1)
	#{
	#	growdatM <- growdat[growdat$Morph==mainmorphs[2],]
	#	xm <- growdatM$Age
	#	growdatM$Sd_Size <- growdatM$SD_Mid
	#	growdatM$high <- growdatM$Len_Mid + 1.96*growdatM$Sd_Size
	#	growdatM$low <- growdatM$Len_Mid - 1.96*growdatM$Sd_Size
	#	lines(xm,growdatM$Len_Mid,col="blue",lwd=2,type="l")
	#	lines(xm,growdatM$high,col="blue",lwd=1,lty="dashed")
	#	lines(xm,growdatM$low,col="blue",lwd=1,lty="dashed")
	#	grid()
	#	legend("topleft",bty="n", c("Females","Males"), lty=1, col = c("red","blue"))
    #}
    
    # PLOT OTOLITH DATA
    
    # Plot Labels
	title(xlab="Age in quarters - Edad en trimestres", cex.lab=1.3, line=3)
	title(ylab="Length (cm) - Talla (cm)", cex.lab=1.3, line=3)
}
#plotGrowth(myreplist,PlotOtoliths=1, PathOtolithDat=PathOtolithDat)


####################################################################################
### 	Natural mortality schedulle
####################################################################################

plotM <- function(replist)
{
	# Get quantities from replist
	morph_indexing <- replist$morph_indexing
	growdat <- replist$endgrowth
	nseasons <- replist$nseasons
	nsexes <- replist$nsexes
	
	# Natural mortality (if time or sex varying)
	mainmorphs <- morph_indexing$Index[morph_indexing$Bseas==1]
	growdatF <- growdat[growdat$Morph==mainmorphs[1],]
	M <- growdatF$M
	x <- growdatF$Age
	y <- M

	if(min(M)!=max(M))
	{
		ymax <- max(M)
		#ylab <- "Natural mortality"
		# Make plot
		windows(8,5)
		par(mar=c(4.5,5,2,1))
		xlim <- range(pretty(x))
		ylim <- range(0,pretty(y))
		plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="o",col="red", ylab="",xlab="", lwd=2, las=1, pch=19)	
		if(nsexes > 1)
		{
			growdatM <- growdat[growdat$Morph==mainmorphs[2],]
			x2 <- growdatM$Age
			y2 <- growdatM$M
			lines(x2,y2,col="blue",lwd=2,type="o",pch=25, cex=0.75)
			legend("topright",bty="n", c("Females-Hembras","Males-Machos"), lty=c(1,1), lwd=2, pch=c(19,25), col=c("red","blue"))
		}
	}
	# Plot Labels
	title(xlab="Age in quarters - Edad en trimestres", cex.lab=1.3, line=3)
	title(ylab="Natural mortality - Mortalidad natural", cex.lab=1.3, line=3.5)
}
#plotM(myreplist)


####################################################################################
### 	Time series - total biomass
####################################################################################

plotBioTot <- function(replist, year1, convertTon, xlim)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	TSraw <- replist$timeseries
	
	# Prepare quantities for plot
	TSraw$Yr <- TSraw$Yr + (TSraw$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat <- TSraw[TSraw$Yr <= endYr+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat <- TSdat[TSdat$Seas==1 & TSdat$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	if(convertTon==0) {BioAll <- TSdat$Bio_all}
	if(convertTon==1) {BioAll <- TSdat$Bio_all/1000}
	x <- Years
	y <- BioAll[3:length(BioAll)]

	# Make plot
	windows(8,5)
	par(mar=c(4,8,2,1))
	if(length(xlim)==1){xlim <- range(pretty(x))}
	if(length(xlim)==2){xlim <- xlim}
	ylim <- range(0,pretty(y))		
	options(scipen=2) 				# Do not use scientific notation in plotting
	plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
	# Plot yearly values
	x2 <- unique(floor(x))
	y2 <- y[x%in%x2]
	points(x2,y2, pch=19, col="black", cex=.75)
	# Plot Labels
	title(xlab="Year-Ano", cex.lab=1.6, line=3)
	title(ylab="Total biomass (t)", cex.lab=1.6, line=7)
	title(ylab="Biomassa total (t)", cex.lab=1.6, line=5)
 
    #abline(h=0,col="grey")
}
#plotBioTot(replist=myreplist, year1=1975, convertTon=1, xlim=c(1970,2015))


####################################################################################
### 	Time series - summary biomass
####################################################################################

plotBioSmry  <- function(replist, year1, convertTon, xlim)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	TSraw <- replist$timeseries
	
	# Prepare quantities for plot
	TSraw$Yr <- TSraw$Yr + (TSraw$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat <- TSraw[TSraw$Yr <= endYr+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat <- TSdat[TSdat$Seas==1 & TSdat$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	if(convertTon==0) {BioSmry <- TSdat$Bio_smry}
	if(convertTon==1) {BioSmry <- TSdat$Bio_smry/1000}
	x <- Years
	y <- BioSmry[3:length(BioSmry)]

	# Make plot
	windows(8,5)
	par(mar=c(4,8,2,1))
	if(length(xlim)==1){xlim <- range(pretty(x))}
	if(length(xlim)==2){xlim <- xlim}
	ylim <- range(0,pretty(y))		
	options(scipen=1) 				# Do not use scientific notation in plotting
	plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
	# Plot yearly values
	x2 <- unique(floor(x))
	y2 <- y[x%in%x2]
	points(x2,y2, pch=19, col="black", cex=.75)
	# Plot Labels
	title(xlab="Year-Ano", cex.lab=1.6, line=3)
	title(ylab="Summary biomass (t)", cex.lab=1.6, line=7)
	title(ylab="Biomasa sumaria (t)", cex.lab=1.6, line=5)
	 
    #abline(h=0,col="grey")
}
#plotBioSmry(myreplist, year1=1975, convertTon=0, xlim=c(1970,2025))


####################################################################################
### 	Time series - spawning biomass
####################################################################################


plotSpawnBio <- function(replist, year1, convertTon, forecast, uncertainty, xlim)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	DerivedQuants <- replist$derived_quants
    bioScale <- 1	# Scaling factor for single sex models
    if(numSexes==1) bioScale <- 0.5
       
    # Make dataframe with SpanBio, years and 95CI for all eras
    SpawnBioSD <- matchfun2("SPB_Virgin",0,"Recr_Virgin",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants,objsubset=DerivedQuants,substr1=TRUE,substr2=TRUE)
    SpawnBioSD$Yr <- substring(SpawnBioSD$Label,5,nchar(SpawnBioSD$Label[1])-1)
    SpawnBioSD$Yr[2] <- as.numeric(SpawnBioSD$Yr[3])-1
    SpawnBioSD$Yr[1] <- as.numeric(SpawnBioSD$Yr[2])-1
    SpawnBioSD$Yr <- as.numeric(SpawnBioSD$Yr)
    SpawnBio <- SpawnBioSD$Value*bioScale								# Scale for biosex (if nsexes = 1, divide SB by 2)
    # Make columns with 95%CI
    SpawnBioSD$upper <- SpawnBio + 1.96*SpawnBioSD$StdDev*bioScale
    SpawnBioSD$lower <- SpawnBio - 1.96*SpawnBioSD$StdDev*bioScale
    SpawnBioSD$lower[SpawnBioSD$lower < 0] <- 0							# If <0 , make it 0
    # Convert to tons
    if(convertTon==1)
    {
	    SpawnBio <- SpawnBio/1000
	    SpawnBioSD$upper <- SpawnBioSD$upper/1000
	    SpawnBioSD$lower <- SpawnBioSD$lower/1000
    }
    # Convert from quarters (EPO models) to year values
	SpawnBioSD$Yr2 <- SpawnBioSD$Yr 
    SpawnBioSD$Yr2[3:length(SpawnBioSD$Yr)] <- year1 + (SpawnBioSD$Yr[3:length(SpawnBioSD$Yr)]/4)-0.25
        
    # Do not plot FORECAST
    if(forecast==F)
    {
	    # Prepare quantities for plot
	    Yrs <- SpawnBioSD$Yr[SpawnBioSD$Yr<=(endYr+1)]
	    SpawnBioVals <- SpawnBio[SpawnBioSD$Yr<=(endYr+1)]
	    SpawnBioCIup <- SpawnBioSD$upper[SpawnBioSD$Yr<=(endYr+1)]
	    SpawnBioCIlo <- SpawnBioSD$lower[SpawnBioSD$Yr<=(endYr+1)]
	    x <- SpawnBioSD$Yr2[SpawnBioSD$Yr%in%Yrs[3:length(Yrs)]]
	    y <- SpawnBioVals[3:length(SpawnBioVals)]

	    # Do not plot 95CI
	    if(uncertainty==F)
	    {
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,8,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(y))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass (t)", cex.lab=1.6, line=7)
		    title(ylab="Biomasa reproductora (t)", cex.lab=1.6, line=5)
	    }
	    
	    # Plot 95CI
	    if(uncertainty==T)
	    {
		    # Prepare CI quantities for plot
		    yCIup <- SpawnBioCIup[3:length(SpawnBioCIup)]
		    yCIlo <- SpawnBioCIlo[3:length(SpawnBioCIlo)]
	    
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,8,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(yCIup))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot CIs
		    polygon(c(x,rev(x)), c(yCIlo,rev(yCIup)), col='grey', border="NA")
		    lines(x,y,col="black",lty=1, lwd=1.25)
		    lines(x,yCIup,col="black",lty=3, lwd=1.25)
		    lines(x,yCIlo,col="black",lty=3, lwd=1.25)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass (t)", cex.lab=1.6, line=7)
		    title(ylab="Biomasa reproductora (t)", cex.lab=1.6, line=5)
	    }
    }
    
    # Plot FORECAST
    if(forecast==T)
    {
	    # Prepare quantities for plot
	    Yrs <- SpawnBioSD$Yr
	    SpawnBioVals <- SpawnBio
	    SpawnBioCIup <- SpawnBioSD$upper
	    SpawnBioCIlo <- SpawnBioSD$lower
	    x <- SpawnBioSD$Yr2[SpawnBioSD$Yr%in%Yrs[3:length(Yrs)]]
	    y <- SpawnBioVals[3:length(SpawnBioVals)]
	    xLast <- SpawnBioSD$Yr2[SpawnBioSD$Yr==(endYr+1)] # Get last year of historical Era 
	    yLast <- SpawnBio[SpawnBioSD$Yr==(endYr+1)] # Get SpawnBio last in last year of historical Era

	    # Do not plot 95CI
	    if(uncertainty==F)
	    {
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,8,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(y))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(xLast,yLast, pch=19, col="black", cex=1.5)
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass (t)", cex.lab=1.6, line=7)
		    title(ylab="Biomasa reproductora (t)", cex.lab=1.6, line=5)
	    }
	    # Plot 95CI
	    if(uncertainty==T)
	    {
		    # Prepare CI quantities for plot
		    yCIup <- SpawnBioCIup[3:length(SpawnBioCIup)]
		    yCIlo <- SpawnBioCIlo[3:length(SpawnBioCIlo)]

		    # Make plot
		    windows(8,5)
		    par(mar=c(4,8,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(yCIup))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot CIs
		    polygon(c(x,rev(x)), c(yCIlo,rev(yCIup)), col='grey', border="NA")
		    lines(x,y,col="black",lty=1, lwd=1.25)
		    lines(x,yCIup,col="black",lty=3, lwd=1.25)
		    lines(x,yCIlo,col="black",lty=3, lwd=1.25)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(xLast,yLast, pch=19, col="black", cex=1.5)
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass (t)", cex.lab=1.6, line=7)
		    title(ylab="Biomasa reproductora (t)", cex.lab=1.6, line=5)
	    }
    }
    
 
}
#plotSpawnBio(replist=myreplist, year1=1975, convertTon=0, forecast=F, uncertainty=T, xlim=c(1970,2025))
   

####################################################################################
### 	Time series - depletion  (SBR)
####################################################################################
### THIS function now returns the values. 

#replist=myreplist
#year1=1975
#forecast=T
#uncertainty=T
#Path
#xlim=c(1970,2025)

plotSBR <- function(replist, year1, forecast, uncertainty, Path, xlim)
{
    # Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	DerivedQuants <- replist$derived_quants
	bioScale <- 1 	# Scaling factor for single sex models
    if(numSexes==1) bioScale <- 0.5

    # Make dataframe with SpanBio, years and 95CI for all eras
    SBRsd <- DerivedQuants
    Labels <- substring(DerivedQuants$Label,1,6)    
    SBRsd <- SBRsd[Labels==c("Bratio"),]    
    SBRsd$Yr <- substring(SBRsd$Label,8,max(nchar(SBRsd$Label)))
    SBRsd$Yr <- as.numeric(SBRsd$Yr)		# Make column with years
    SBRvals <- SBRsd$Value*bioScale		# Scale for biosex (if nsexes = 1, divide SB by 2)
    # Make columns with 95%CI
    SBRsd$upper <- SBRvals + 1.96*SBRsd$StdDev*bioScale
    SBRsd$lower <- SBRvals - 1.96*SBRsd$StdDev*bioScale
    SBRsd$lower[SBRsd$lower < 0] <- 0		# If <0 , make it 0
    # Convert from quarters (EPO models) to year values
	SBRsd$Yr2 <- SBRsd$Yr 
    SBRsd$Yr2 <- year1 + (SBRsd$Yr/4)-0.25

    # Make forecast management report name
	#ForeRepName <- paste(Path, "Forecast-report.SSO" ,sep="")
	# Get management report
	#ForeRepStart <- grep("Management_report", readLines(ForeRepName))
	#ForeRepEnd <- grep("Forecast_using_Fspr", readLines(ForeRepName))[1]
	#ForeDat <- read.table(file=ForeRepName,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=45, skip = ForeRepStart-1)
	#ForeDat <- as.data.frame(ForeDat)
	# Get Smsy/S0
	#SmsySzero <- as.numeric(ForeDat[ForeDat[,1]==c("SPBmsy/SPBzero(using_S0)"),3])

	
	# Make forecast management report name
	ForeRepName <- paste(Path, "Forecast-report.SSO" ,sep="")
	# Get management report
	ForeRepStart <- grep("Management_report", readLines(ForeRepName))
	ForeRepEnd <- grep("#_note when there is time-varying biology", readLines(ForeRepName))[1]
	
	#ForeDat <- read.table(file=ForeRepName,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=45, skip = ForeRepStart-1)
	ForeDat <- read.table(file=ForeRepName,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=ForeRepEnd-ForeRepStart, skip = ForeRepStart-1)
	ForeDat <- as.data.frame(ForeDat)
	# Get Smsy/S0
	SmsySzero <- as.numeric(ForeDat[ForeDat[,1]==c("SPBmsy/SPBzero(using_SPB_virgin)"),2])

    # Do not plot FORECAST
    if(forecast==F)
    {
	    # Prepare quantities for plot
	    Yrs <- SBRsd$Yr[SBRsd$Yr<=(endYr+1)]
	    SBRvals <- SBRsd$Value[SBRsd$Yr<=(endYr+1)]
	    SBR.CIup <- SBRsd$upper[SBRsd$Yr<=(endYr+1)]
	    SBR.CIlo <- SBRsd$lower[SBRsd$Yr<=(endYr+1)]
	    x <- SBRsd$Yr2[SBRsd$Yr%in%Yrs[1:length(Yrs)]]
	    y <- SBRvals[1:length(SBRvals)]
	    
	    # Do not plot 95CI
	    if(uncertainty==F)
	    {		    
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(y))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    x2 <- x2[2:length(x2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
			lines(xlim,c(SmsySzero,SmsySzero), lty="dashed", lwd=1.25)		# Plot horizontal ref line for SmsySzero

		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass ratio", cex.lab=1.6, line=5)
		    title(ylab="Cociente de biomasa reproductora", cex.lab=1.6, line=3)
	    }
	    
	    # Plot 95CI
	    if(uncertainty==T)
	    {
		    # Prepare CI quantities for plot
		    yCIup <- SBRsd$upper[1:length(SBR.CIup)]
		    yCIlo <- SBRsd$lower[1:length(SBR.CIlo)]
	    
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(yCIup))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot CIs
		    polygon(c(x,rev(x)), c(yCIlo,rev(yCIup)), col='grey', border="NA")
		    lines(x,y,col="black",lty=1, lwd=1.25)
		    lines(x,yCIup,col="black",lty=3, lwd=1.25)
		    lines(x,yCIlo,col="black",lty=3, lwd=1.25)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    x2 <- x2[2:length(x2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
			lines(xlim,c(SmsySzero,SmsySzero), lty="dashed", lwd=1.25)		# Plot horizontal ref line for SmsySzero

		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass ratio", cex.lab=1.6, line=5)
		    title(ylab="Cociente de biomasa reproductora", cex.lab=1.6, line=3)
	    }
    }
        
    # Plot FORECAST
    if(forecast==T)
    {
	    # Prepare quantities for plot
		Yrs <- SBRsd$Yr
		SBRvals <- SBRsd$Value
		SBR.CIup <- SBRsd$upper
		SBR.CIlo <- SBRsd$lower
		x <- SBRsd$Yr2[SBRsd$Yr%in%Yrs[1:length(Yrs)]]		    
		y <- SBRvals[1:length(SBRvals)]
		# print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
		# print(y)
		# print(length(y))
		xLast <- SBRsd$Yr2[SBRsd$Yr==(endYr+1)] 	# Get last year of historical Era 
    	yLast <- SBRvals[SBRsd$Yr==(endYr+1)] 	# Get SpawnBio last in last year of historical Era
    	    	
	    # Do not plot 95CI
	    if(uncertainty==F)
	    {
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(y))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    x2 <- x2[2:length(x2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(xLast,yLast, pch=19, col="black", cex=1.5)
			lines(xlim,c(SmsySzero,SmsySzero), lty="dashed", lwd=1.25)		# Plot horizontal ref line for SmsySzero

		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass ratio", cex.lab=1.6, line=5)
		    title(ylab="Cociente de biomasa reproductora", cex.lab=1.6, line=3)
	    }
	    # Plot 95CI
	    if(uncertainty==T)
	    {
		    # Prepare CI quantities for plot
		    yCIup <- SBRsd$upper[1:length(SBR.CIup)]
		    yCIlo <- SBRsd$lower[1:length(SBR.CIlo)]
		        		
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(yCIup))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot CIs		    
		    polygon(c(x,rev(x)), c(yCIlo,rev(yCIup)), col='grey', border="NA")
		    lines(x,y,col="black",lty=1, lwd=1.25)
		    lines(x,yCIup,col="black",lty=3, lwd=1.25)
		    lines(x,yCIlo,col="black",lty=3, lwd=1.25)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    x2 <- x2[2:length(x2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(xLast,yLast, pch=19, col="black", cex=1.5)
			lines(xlim,c(SmsySzero,SmsySzero), lty="dashed", lwd=1.25)		# Plot horizontal ref line for SmsySzero

		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass ratio", cex.lab=1.6, line=5)
		    title(ylab="Cociente de biomasa reproductora", cex.lab=1.6, line=3)
		    
		    x2 <- unique(floor(x))
		    x2 <- x2[1:length(x2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    y2 <- y[x%in%x2]
		    
		    # print(length(x2))
		    # print(length(y2))

	    }
    }

    xy<-cbind(x,y)
    colnames(xy)<-c("years","SBR")
    return(xy)
    
    }

#plotSBR(replist=myreplist, year1=1975, forecast=T, uncertainty=T, Path, xlim=c(1970,2025))
   

####################################################################################
### 	Time series - RECRUITMENT
####################################################################################

plotRec <- function(replist, year1, recUnits, relRec, forecast, uncertainty, xlim)
{
    # Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	DerivedQuants <- replist$derived_quants
	bioScale <- 1 	# Scaling factor for single sex models
    if(numSexes==1) bioScale <- 0.5

    # Make dataframe with Rec, years and 95CI for all eras    
    RecSD <- matchfun2("Recr_Virgin",0,"SPRratio_1",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants,objsubset=DerivedQuants,substr1=TRUE,substr2=TRUE)
    RecSD$Yr <- substring(RecSD$Label,6,nchar(RecSD$Label[1])-1)
    RecSD$Yr[2] <- as.numeric(RecSD$Yr[3])-1
    RecSD$Yr[1] <- as.numeric(RecSD$Yr[2])-1
    RecSD$Yr <- as.numeric(RecSD$Yr)
    # Make columns with 95%CI
    RecSD$upper <- RecSD$Value + 1.96*RecSD$StdDev
    RecSD$lower <- RecSD$Value - 1.96*RecSD$StdDev
    RecSD$lower[RecSD$lower < 0] <- 0		# If <0 , make it 0
    # Convert to desired units
    RecSD$Value <- RecSD$Value/recUnits
    RecSD$upper <- RecSD$upper/recUnits
    RecSD$lower <- RecSD$lower/recUnits
    # Convert from quarters (EPO models) to year values
	RecSD$Yr2 <- RecSD$Yr
	RecSD$Yr2 <- year1 + (RecSD$Yr/4)-0.25
	# Drop first two rows (recVir and recInit) from table (better for general plotting head)
	RecSD2 <- RecSD[3:dim(RecSD)[1],]

    # Make dataframe with relative Rec, years and 95CI for all eras
    recVirg <- RecSD$Value[1]  
	RelRecSD <- RecSD2
	RelRecSD$Value <- RelRecSD$Value/recVirg
	RelRecSD$upper <- RelRecSD$upper/recVirg
    RelRecSD$lower <- RelRecSD$lower/recVirg

    # Define recrutiment table for plot (absolute or relative recs)
	if(relRec==0){RecTable <- RecSD2}
	if(relRec==1){RecTable <- RelRecSD}

    # Do not plot FORECAST
    if(forecast==F)
    {
	    # Prepare quantities for plot
	    Yrs <- RecTable$Yr[RecTable$Yr<=(endYr+1)]
	    RecVals <- RecTable$Value[RecTable$Yr<=(endYr+1)]
	    RecCIup <- RecTable$upper[RecTable$Yr<=(endYr+1)]
	    RecCIlo <- RecTable$lower[RecTable$Yr<=(endYr+1)]
	    x <- RecTable$Yr2[RecTable$Yr%in%Yrs[1:length(Yrs)]]
	    y <- RecVals[1:length(RecVals)]

	    # Do not plot 95CI
	    if(uncertainty==F)
	    {    
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(y))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    if(relRec==0)
		    {
			    title(ylab="Recruitment (millions of fish)", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento (milliones de peces)", cex.lab=1.6, line=3)
		    }
		    if(relRec==1)
		    {
			    title(ylab="Relative recruitment", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento relativo", cex.lab=1.6, line=3)
			    abline(h=1, lty="dashed", lwd=1.25)
		    }
	    }
	    
	    # Plot 95CI
	    if(uncertainty==T)
	    {
		    # Prepare CI quantities for plot
		    yCIup <- RecTable$upper[1:length(RecCIup)]
		    yCIlo <- RecTable$lower[1:length(RecCIlo)]
	    		    
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(yCIup))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot CIs
		    polygon(c(x,rev(x)), c(yCIlo,rev(yCIup)), col='grey', border="NA")
		    lines(x,y,col="black",lty=1, lwd=1.25)
		    lines(x,yCIup,col="black",lty=3, lwd=1.25)
		    lines(x,yCIlo,col="black",lty=3, lwd=1.25)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    if(relRec==0)
		    {
			    title(ylab="Recruitment (millions of fish)", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento (milliones de peces)", cex.lab=1.6, line=3)
		    }
		    if(relRec==1)
		    {
			    title(ylab="Relative recruitment", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento relativo", cex.lab=1.6, line=3)
			    abline(h=1, lty="dashed", lwd=1.25)
		    }	
	    }
    }
        
    # Plot FORECAST
    if(forecast==T)
    {
	    # Prepare quantities for plot
	    Yrs <- RecTable$Yr
	    RecVals <- RecTable$Value
	    RecCIup <- RecTable$upper
	    RecCIlo <- RecTable$lower
	    x <- RecTable$Yr2[RecTable$Yr%in%Yrs[1:length(Yrs)]]
	    y <- RecVals[1:length(RecVals)]
	    xLast <- RecTable$Yr2[RecTable$Yr==(endYr+1)] 	# Get last year of historical Era
	    yLast <- RecVals[RecTable$Yr==(endYr+1)] 	# Get SpawnBio last in last year of historical Era

	    # Do not plot 95CI
	    if(uncertainty==F)
	    {		
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(y))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(xLast,yLast, pch=19, col="black", cex=1.5)
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    if(relRec==0)
		    {
			    title(ylab="Recruitment (millions of fish)", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento (milliones de peces)", cex.lab=1.6, line=3)
		    }
		    if(relRec==1)
		    {
			    title(ylab="Relative recruitment", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento relativo", cex.lab=1.6, line=3)
			    abline(h=1, lty="dashed", lwd=1.25)
		    }	
	    }
	    
	    # Plot 95CI
	    if(uncertainty==T)
	    {    
		    # Prepare CI quantities for plot
		    yCIup <- RecTable$upper[1:length(RecCIup)]
		    yCIlo <- RecTable$lower[1:length(RecCIlo)]
		        		
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    if(length(xlim)==1){xlim <- range(pretty(x))}
		    if(length(xlim)==2){xlim <- xlim}
		    ylim <- range(0,pretty(yCIup))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot CIs
		    polygon(c(x,rev(x)), c(yCIlo,rev(yCIup)), col='grey', border="NA")
		    lines(x,y,col="black",lty=1, lwd=1.25)
		    lines(x,yCIup,col="black",lty=3, lwd=1.25)
		    lines(x,yCIlo,col="black",lty=3, lwd=1.25)
		    # Plot yearly values
		    x2 <- unique(floor(x))
		    y2 <- y[x%in%x2]
		    points(x2,y2, pch=19, col="black", cex=.75)
		    points(xLast,yLast, pch=19, col="black", cex=1.5)
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    if(relRec==0)
		    {
			    title(ylab="Recruitment (millions of fish)", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento (milliones de peces)", cex.lab=1.6, line=3)
		    }
		    if(relRec==1)
		    {
			    title(ylab="Relative recruitment", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento relativo", cex.lab=1.6, line=3)
			    abline(h=1, lty="dashed", lwd=1.25)
		    }	
	    }
    }
}
#plotRec(replist=myreplist, year1=1975, recUnits=1000000, relRec=1, forecast=F, uncertainty=T, xlim=c(1970,2025))


####################################################################################
###		CATCH TIME SERIES - ALL FISHERIES (Fig 2.2)
####################################################################################

plotCatch <- function(replist, year1, convertTon=1, fore, FleetNums, LLfleetNums, numRows, numCols, catchAnnual, shadedOn, printOut, xlim=c(1970,2015))
{
    # Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	#numFleets <- replist$nfleets
			numFleets <- replist$nfishfleets # only fisheries fleets <>< Change 15 March 2016


	TimeSeries <- replist$timeseries
	FleetNames <- replist$FleetNames

	# Make table with catch time series in weight
	# make headers to subset
	HeadersC <- rep(NA,numFleets)
	for(ifleet in 1:numFleets)
	{
		headerTemp <- paste("sel(B):_", ifleet, sep="")
		HeadersC[ifleet] <- headerTemp
	}
	CatchW <- subset(TimeSeries,select=c("Yr", "Era", HeadersC))
	CatchW <- CatchW[3:dim(CatchW)[1],] 	# Drop virg and init rows	
	CatchW$Yr2 <- year1 + (CatchW$Yr/4)-0.25
	CatchW$YrFloor <- floor(CatchW$Yr2)
	if(fore==0) {CatchW <- subset(CatchW, Era=="TIME")}  	# Subset for historical period (drop forecast years)
	
	# Convert to tons
	if(convertTon==1){CatchW[,3:(dim(CatchW)[2]-2)]=CatchW[,3:(dim(CatchW)[2]-2)]/1000}

	# If plot of annual catch desired
	# Make table of annual catch 
	if(catchAnnual==1)
	{
		# Make table annual catches
		Years <- unique(CatchW$YrFloor) 
		CatchAnnual <- data.frame(matrix(NA,length(Years),numFleets))
		for(ifleet in 1:numFleets)
		{
			CatchAnnual[,ifleet] <- tapply(CatchW[,ifleet+2],CatchW$YrFloor,sum)
		}
		Era <- CatchW[CatchW$Yr2%in%Years,2]
		CatchAnnual <- cbind(CatchAnnual,Years,Era)		
		if(fore==1){CatchAnnual <- subset(CatchAnnual,Years!=Years[length(Years)])}
		CatchAnnual <- CatchAnnual[,1:dim(CatchAnnual)[2]-1]	  
	}
			
	# Plot
	# Prepare quantities for plot
	if(catchAnnual==0){CatchToPlot=CatchW[,3:(dim(CatchW)[2]-1)]} 	# If plot of quarterly catch desired
	if(catchAnnual==1){CatchToPlot=CatchAnnual}						# If plot of annual catch is desired

	#x <- CatchToPlot[,dim(CatchToPlot)[2]-1]
	x <- CatchToPlot[,dim(CatchToPlot)[2]]

	#xlim <- range(pretty(x))
	if(length(xlim)==1){xlim <- range(pretty(x))}
	if(length(xlim)==2){xlim <- xlim}

	ymax <- 1.25* max(CatchToPlot[,1:(dim(CatchToPlot)[2]-1)])
	ylim <- range(0,pretty(ymax))
	
	windows(8,6)
	par(mfrow=c(numRows,numCols), mar=c(2,2,0.5,1), omi=c(.5,.5,.2,0))
	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			# Set color of polygons
			color=c("green")
			if(ifleet%in%LLfleetNums){color=c("blue")}
			
			plot(x, CatchToPlot[,ifleet], type="l", xlim=xlim, ylim=ylim, ylab="",xlab="", col="black", lwd=1.25, cex.axis=.75, mgp=c(2,0.5,0), tcl=-.3, las=1)
			# If polygons desired
			if(shadedOn==1){polygon(c(x,rev(x)), c(rep(0,length(x)),rev(CatchToPlot[,ifleet])), col=color, border="black")}
			#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
			leg <- FleetNames[ifleet]
			# title(main=leg, cex.main=1)
			#text(x[5],ymax,leg, adj=c(0,1), cex=1)
			text(x[5],ymax,leg, adj=c(0,1), cex=0.9)
		}
	}
	mtext(side=1, outer=T, "Year-Ano", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Total catch (t) - Captura total (t)", line=1.5, cex=1.25)
	#return(CatchToPlot)
	if(printOut==1)
	{
		print(CatchToPlot)
		CatchBioTable <- CatchToPlot
		return(CatchBioTable)
	}
}
#plotCatch(replist=myreplist, year1=1975, convertTon=0, fore=1, FleetNums=seq(1,16,1), LLfleetNums=c(8,9,14,15), numRows=4, numCols=4, catchAnnual=0, shadedOn=1, printOut=1, xlim=c(1975,2015))


####################################################################################
###		CATCH TIME SERIES - SF and LL
####################################################################################

#replist=myreplist
#year1=1975
#convertTon=0
#fore=0
#SFfleetNums=c(seq(1,11,1),seq(20,23,1))
#LLfleetNums=seq(12,23,1)
#catchAnnual=1
#printOut=1

plotCpooled <- function(replist, year1, convertTon=1, fore, SFfleetNums, LLfleetNums, catchAnnual, printOut)
{
    # Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	#numFleets <- replist$nfleets
	numFleets <- replist$nfishfleets # only fisheries fleets <>< Change 15 March 2016

	TimeSeries <- replist$timeseries
	FleetNames <- replist$FleetNames

	# Make table with catch time series in weight
	# make headers to subset
	HeadersC <- rep(NA,numFleets)
	for(ifleet in 1:numFleets)
	{
		headerTemp <- paste("sel(B):_", ifleet, sep="")
		HeadersC[ifleet] <- headerTemp
	}
	CatchW <- subset(TimeSeries,select=c("Yr", "Era", HeadersC))
	CatchW <- CatchW[3:dim(CatchW)[1],] 	# Drop virg and init rows	
	CatchW$Yr2 <- year1 + (CatchW$Yr/4)-0.25
	CatchW$YrFloor <- floor(CatchW$Yr2)
	if(fore==0) {CatchW <- subset(CatchW, Era=="TIME")}  	# Subset for historical period (drop forecast years)
	# Convert to tons
	if(convertTon==1){CatchW[,3:(dim(CatchW)[2]-2)]=CatchW[,3:(dim(CatchW)[2]-2)]/1000}
	CatchW <- CatchW[,3:(dim(CatchW)[2])]			# NEW

	# Prepare quantities for plot
	# If plot of annual catch desired
	# Make table of annual catch 
	if(catchAnnual==1)
	{
		# Make table annual catches
		Years <- unique(CatchW$YrFloor) 
		CatchAnnual <- data.frame(matrix(NA,length(Years),numFleets))
		for(ifleet in 1:numFleets)
		{
			CatchAnnual[,ifleet] <- tapply(CatchW[,ifleet],CatchW$YrFloor,sum)
		}
		#Era <- CatchW[CatchW$Yr2%in%Years,2]
		#CatchAnnual <- cbind(CatchAnnual,Years,Era)		
		#CatchAnnual <- subset(CatchAnnual,Years!=Years[length(Years)])  

		Era <- CatchW[CatchW$Yr2%in%Years,2]
		CatchAnnual <- cbind(CatchAnnual,Years,Era)		
		CatchAnnual <- subset(CatchAnnual,Years!=Years[length(Years)])
		CatchAnnual <- CatchAnnual[,1:dim(CatchAnnual)[2]-1]	  

		
		
		#CatchAnnual <- cbind(CatchAnnual,Years)
		#CatchAnnual <- subset(CatchAnnual,Years!=Years[length(Years)])  		
	}
	# If plot of quarterly catch desired
	if(catchAnnual==0)
	{
		CatchToPlot=CatchW
		YearsToPlot <- CatchW[,dim(CatchW)[2]-1]
	}
	# If plot of annual catch is desired
	if(catchAnnual==1)
	{
		CatchToPlot=CatchAnnual
		YearsToPlot <- CatchAnnual[,dim(CatchAnnual)[2]]
	}		
	# Compute pooled catches for SF and LL fisheries
	CatchSF <- apply(as.matrix(CatchToPlot[,SFfleetNums]), MARGIN=1, FUN=sum)
	CatchLL <- apply(as.matrix(CatchToPlot[,LLfleetNums]), MARGIN=1, FUN=sum)
	CatchTot <- CatchSF + CatchLL
	CatchToPlot <- cbind(CatchSF,CatchLL,CatchTot,YearsToPlot)
		
	# Plot
	# Prepare quantities for plot
	x <- YearsToPlot
	xlim <- range(pretty(x))
	ymax <- 1.25* max(CatchToPlot[,3])
	ylim <- range(0,pretty(ymax))
	
	windows(8,5)
	par(mar=c(2,2,1,1), omi=c(.5,.5,.2,0))
	# Set color of polygons
	colorSF=c("green")
	colorLL=c("blue")
			
	plot(x, CatchToPlot[,3], type="l", xlim=xlim, ylim=ylim, ylab="",xlab="", col="black", lwd=1.25, cex.axis=.75, mgp=c(2,0.5,0), tcl=-.3, las=1)
	# If polygons desired
	polygon(c(x,rev(x)), c(rep(0,length(x)),rev(CatchToPlot[,1])), col=colorSF, border="black")
	polygon(c(x,rev(x)), c(CatchToPlot[,1],rev(CatchToPlot[,3])), col=colorLL, border="black")
	
	#leg <- FleetNames[ifleet]
	# title(main=leg, cex.main=1)
	#text(x[5],ymax,leg, adj=c(0,1), cex=1)
	#text(x[5],ymax,leg, adj=c(0,1), cex=0.9)
	
	mtext(side=1, outer=T, "Year-Ano", line=1, cex=1.25)
	mtext(side=2, outer=T, "Total catch (t) - Captura total (t)", line=1.5, cex=1.25)
	#return(CatchToPlot)
	if(printOut==1)
	{
		print(CatchToPlot)
		CatchBioTable <- CatchToPlot
		return(CatchBioTable)
	}	
}
#plotCpooled(replist=myreplist, year1=1975, convertTon=1, fore=0, SFfleetNums=c(1,2,3,4,5,6,7,10,11,12,13), LLfleetNums=c(8,9,14,15), catchAnnual=1, printOut=1)


####################################################################################
###		DISCARDS TIME SERIES (Fig 2.3)
####################################################################################

plotDiscards <- function(replist, year1, yearCut, convertTon, FleetNums, RealFleetNums, DiscFleetNums, numRows, numCols, plotAnnual, printOut)
{
    # Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	#numFleets <- replist$nfleets
		numFleets <- replist$nfishfleets # only fisheries fleets <>< Change 15 March 2016

	TimeSeries <- replist$timeseries
	FleetNames <- replist$FleetNames

	# CATCH (quarterly)
	# Make table with quarterly catch time series (weight)
	# make headers to subset
	HeadersC <- rep(NA,numFleets)
	for(ifleet in 1:numFleets)
	{
		headerTemp <- paste("sel(B):_", ifleet, sep="")
		HeadersC[ifleet] <- headerTemp
	}
	CatchW <- subset(TimeSeries,select=c("Yr", "Era", HeadersC))
	CatchW <- CatchW[3:dim(CatchW)[1],] 	# Drop virg and init rows	
	CatchW <- subset(CatchW, Era=="TIME")  	# Subset for historical period (drop forecast years)
	# Convert to tons
	if(convertTon==1){CatchW[,3:dim(CatchW)[2]]=CatchW[,3:dim(CatchW)[2]]/1000}
	# DISCARD RATIOS (quarterly)
	# Make table with quarterly discard ratio time series
	DiscRatioTable <- data.frame(matrix(NA,dim(CatchW)[1],length(DiscFleetNums)))
	for(ifleet in 1:length(DiscFleetNums))	# Loop over discard fisheries
	{
		# Get discard and real fishery
		realFleetNum <- RealFleetNums[ifleet]
		discFleetNum <- DiscFleetNums[ifleet]
		# Compute the dicard ratios
		for(irow in 1:dim(CatchW)[1])	# Loop over quarters
		{
			DiscRatioTable[irow,ifleet] <- CatchW[irow,discFleetNum+2]/(CatchW[irow,realFleetNum+2] + CatchW[irow,discFleetNum+2])
			#DiscRatioTable[irow,ifleet] <- CatchW[irow,discFleetNum+2]/CatchW[irow,realFleetNum+2]
		}
	}
	names(DiscRatioTable) <-  paste("discRatio.F",DiscFleetNums,sep="")
	CatchW <- cbind(CatchW,DiscRatioTable)
	CatchW$Yr2 <- year1 + (CatchW$Yr/4)-0.25
	CatchW$YrFloor <- floor(CatchW$Yr2)

	# If annual quantities desired
	if(plotAnnual==1)
	{
		# Make table of annual catch 
		Years <- unique(CatchW$YrFloor) 
		CatchAnnual <- data.frame(matrix(NA,length(Years),numFleets))
		for(ifleet in 1:numFleets)
		{
			CatchAnnual[,ifleet] <- tapply(CatchW[,ifleet+2],CatchW$YrFloor,sum)
		}		
		# Compute annual discard ratios and make discards table
		DiscRatioTable <- data.frame(matrix(NA,dim(CatchAnnual)[1],length(DiscFleetNums)))
		for(ifleet in 1:length(DiscFleetNums))	# Loop over discard fisheries
		{
			# Get discard and real fishery
			realFleetNum <- RealFleetNums[ifleet]
			discFleetNum <- DiscFleetNums[ifleet]
			# Compute the dicard ratios
			for(irow in 1:dim(CatchAnnual)[1])	# Loop over years
			{
				DiscRatioTable[irow,ifleet] <- CatchAnnual[irow,discFleetNum]/(CatchAnnual[irow,realFleetNum] + CatchAnnual[irow,discFleetNum])
			}
		}
		names(DiscRatioTable) <-  paste("discRatio.F",DiscFleetNums,sep="")
		CatchAnnual <- cbind(CatchAnnual,DiscRatioTable,Years)	
	}

	# Prepare quantities for plot
	if(plotAnnual==0){CatchToPlot=CatchW[,3:(dim(CatchW)[2]-1)]} 	# If plot of quarterly catch desired
	if(plotAnnual==1){CatchToPlot=CatchAnnual}						# If plot of annual catch is desired


	##################
	# PLOT DISCARDS
	##################

	numDiscFleets <- length(DiscFleetNums)	
	x <- CatchToPlot[CatchToPlot[,dim(CatchToPlot)[2]]>=yearCut,dim(CatchToPlot)[2]]
	xlim <- range(pretty(x))
	
	DiscTable <- CatchToPlot[CatchToPlot[,dim(CatchToPlot)[2]]>=yearCut,(numFleets+1):(numFleets+numDiscFleets)]
	ymax <- 1.25*max(DiscTable,na.rm=T)
	#ymax <- 0.30
	#ylim <- range(0,pretty(ymax))
	ylim <- range(0,ymax)
	
		
	windows(8,6)
	par(mfrow=c(numRows,numCols), mar=c(2,2,0.5,1), omi=c(.5,.5,.2,0))
	for(ifleet in 1:length(DiscFleetNums))
	{
		discFleetNum <- DiscFleetNums[ifleet]
		realFleetNum <- RealFleetNums[ifleet]
		y <- DiscTable[,ifleet]
			
		plot(x, y, type="l", xlim=xlim, ylim=ylim, ylab="",xlab="", col="blue", lwd=2, cex.axis=1, mgp=c(2,0.5,0), tcl=-.3, las=1)
		leg <- FleetNames[DiscFleetNums[ifleet]]
		#leg <- paste("Fisheries ", discFleetNum,"/",realFleetNum," - Pesquerias ",discFleetNum,"/",realFleetNum, sep="")
		text(x[4],ymax,leg, adj=c(0,1), cex=1)
	}
	mtext(side=1, outer=T, "Year-Ano", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Discard proportions/Proporcion de descartes", line=1.5, cex=1.25)
	
	#return(CatchToPlot)
	#print(CatchToPlot)

}
#plotDiscards(replist=myreplist, year1=1975, yearCut=1990, convertTon=1,  FleetNums=seq(1,15,1), RealFleetNums=c(2,3,4,5), DiscFleetNums=c(10,11,12,13), numRows=2, numCols=2, plotAnnual=1, printOut=0)


####################################################################################
###		EFFORT TIME SERIES (Fig 2.4) - CLASS 6 VESSELS
####################################################################################

plotEffort <- function(replist, year1, convertTon, FleetNums, LLfleets, numRows, numCols, effortAnnual, printOut, xlim)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	#numFleets <- replist$nfleets
	numFleets <- replist$nfishfleets # only fisheries fleets <>< Change 15 March 2016

	TimeSeries <- replist$timeseries
	CPUEdata <- replist$cpue
	FleetNames <- replist$FleetNames

	# Make table with catch time series in weight
	# make headers to subset
	HeadersC <- rep(NA,numFleets)
	for(ifleet in 1:numFleets)
	{
		headerTemp <- paste("sel(B):_", ifleet, sep="")
		HeadersC[ifleet] <- headerTemp
	}
	CatchW <- subset(TimeSeries,select=c("Yr", "Era", HeadersC))
	CatchW <- CatchW[3:dim(CatchW)[1],] 	# Drop virg and init rows	
	CatchW$Yr2 <- year1 + (CatchW$Yr/4)-0.25
	CatchW$YrFloor <- floor(CatchW$Yr2)
	CatchW <- subset(CatchW, Era=="TIME")  	# Subset for historical period (drop forecast years)
	# Convert to tons
	if(convertTon==1){CatchW[,3:(dim(CatchW)[2]-2)]=CatchW[,3:(dim(CatchW)[2]-2)]/1000}

	# Make table with CPUE time series   
	# Convert from quarters (EPO models) to year values
	CPUEdata$Yr2 <-  year1 + (CPUEdata$Yr/4)-0.25
	CPUEdata$YrFloor <- floor(CPUEdata$Yr2)

	#CPUEdata[CPUEdata[,]=="NA"]=0
	#PSdat[PSdat$DOLdf=="NaN",]$DOLdf=0 

	# Prepare table for output effort data (quarterly or annual)
	if(effortAnnual==0)
	{
		EffortTable <- data.frame(matrix(0,length(unique(as.numeric(CPUEdata$Yr2))),numFleets+1))
		EffortTable[,dim(EffortTable)[2]] <- sort(unique(as.numeric(CPUEdata$Yr2)))
	}
	if(effortAnnual==1)
	{
		EffortTable <- data.frame(matrix(0,length(unique(floor(CPUEdata$Yr2))),numFleets+1))
		EffortTable[,dim(EffortTable)[2]] <- sort(unique(floor(CPUEdata$Yr2)))
	}

	# Plot
	# Prepare quantities for plot
	windows(8,6)
	par(mfrow=c(numRows,numCols), mar=c(2,2,0.5,1), omi=c(.5,1,.2,0))
	#xlim <- range(pretty(CatchW$Yr2))
	if(length(xlim)==1){xlim <- range(pretty(CatchW$Yr2))}
	if(length(xlim)==2){xlim <- xlim}

	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			CPUEdataTemp <- subset(CPUEdata,FleetNum==ifleet)
			if(dim(CPUEdataTemp)[1]!=0)
			{
				CPUEdataTemp$Obs=abs(CPUEdataTemp$Obs)	# Get rid of negative sign (used not to fit to CPUE)
				YearsTemp <- CPUEdataTemp$Yr2	# Select years for which there is CPUE data
				YearsFloorTemp <- floor(YearsTemp)
				CatchTemp <- subset(CatchW[,c(ifleet+2,(dim(CatchW)[2]-1))], Yr2%in%YearsTemp)  # Select catch data for which there is CPUE data
				EffortTemp <- CatchTemp[,1]/CPUEdataTemp$Obs
				if(ifleet%in%LLfleets){EffortTemp=EffortTemp/mean(EffortTemp)} # Scale std numbers of hooks to mean
								
				# If plot of annual effort desired
				# Make table of annual effort 
				if(effortAnnual==1)
				{
					# Make table annual effort
					YearsTemp <- unique(YearsFloorTemp)
					EffortTemp <- tapply(EffortTemp,YearsFloorTemp,sum)
				}
	
				y <- EffortTemp[EffortTemp!=c("Inf")]	
				x <- YearsTemp[EffortTemp!=c("Inf")]
				ymax <- 1.25 * max(y, na.rm=T)
				#ylim <- range(0,pretty(ymax))
				ylim <- range(0,ymax)

				plot(x, y, type="l", xlim=xlim, ylim=ylim, ylab="",xlab="", col="blue", lwd=2, cex.axis=.75, mgp=c(2,0.5,0), tcl=-.3, las=1)
				leg <- FleetNames[ifleet]
				#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
				text(xlim[1]+2, ymax, leg, adj=c(0,1), cex=1)
				
				# Populate effort table
				#if(printOut==1)
				#{
				#	for(irow in 1:dim(EffortTable)[1])
				#	{
				#		yearTemp <- EffortTable[irow,dim(EffortTable)[2]]
				#		test <- yearTemp%in%x
				#		if(test==TRUE)
				#		{
				#			effortTemp <- y[x==yearTemp]
				#			EffortTable[irow,ifleet]=effortTemp
				#		}
				#	}
				#}
			}
		}
	}
	mtext(side=1, outer=T, "Year-Ano", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Thousands of days and standardized numbers of hooks", line=4, cex=1.25)
	mtext(side=2, outer=T, "Miles de dias e numero de anzuelos estandarizados", line=1, cex=1.25)
	

}
#plotEffort(replist=myreplist, year1=1975, convertTon=1, FleetNums=seq(1,15,1), LLfleets=c(8,9), numRows=3, numCols=3, effortAnnual=1, printOut=0, xlim=c(1975,2010))


####################################################################################
###		CPUE DATA (Fig 2.5)
####################################################################################

plotCPUEobs <- function(replist, year1, FleetNums, numRows, numCols, lowessOn, loessBin, xlim)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	numFleets <- replist$nfleets
	CPUEdata <- replist$cpue
	FleetNames <- replist$FleetNames

	# Make table with CPUE time series    
	# Convert from quarters (EPO models) to year values
	CPUEdata$Yr2 <-  year1 + (CPUEdata$Yr/4)-0.25

	# Plot
	# Prepare quantities for plot
	windows(8,6)
	par(mfrow=c(numRows,numCols), mar=c(2,2,0.5,1), omi=c(.5,.5,.2,0))
	#xlim <- range(pretty(CPUEdata$Yr2))
	if(length(xlim)==1){xlim <- range(pretty(CPUEdata$Yr2))}
	if(length(xlim)==2){xlim <- xlim}

	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			CPUEdataTemp <- subset(CPUEdata,FleetNum==ifleet)
			if(dim(CPUEdataTemp)[1]!=0)
			{
				x <- CPUEdataTemp$Yr2
				CPUEobs <- CPUEdataTemp$Obs
				# Mark negative cpue vals (outliers) as NA
				for(i in 1:length(CPUEobs))
				{
					if(CPUEobs[i]<0) {CPUEobs[i]=NA}
				}
				# Compute normalized cpue
				CPUEobsAvg <- mean(CPUEobs, na.rm=T)
				y <- CPUEobs/(CPUEobsAvg)
				ymax <- 1.25 * max(y, na.rm=T)
				ylim <- range(0,pretty(ymax))
				ylim <- range(0,ymax)

				plot(x, y, type="l", xlim=xlim, ylim=ylim, ylab="",xlab="", col="black", lwd=1.25, cex.axis=.75, mgp=c(2,0.5,0), tcl=-.3, las=1)
				if(lowessOn ==1){lines(x,predict(loess(y ~ x, span=loessBin, na.action=na.exclude)), col="blue", lwd=2)}

				abline(h=1, col="black",lty="dashed", lwd=1.25)
				leg <- FleetNames[ifleet]
				#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
				text(xlim[1]+2, ymax, leg, adj=c(0,1), cex=1)
			}
		}
	}
	mtext(side=1, outer=T, "Year-Ano", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Scaled CPUE-CPUE escalada", line=1.5, cex=1.25)
}
#plotCPUEobs(replist=myreplist, year1=1975, FleetNums= seq(1,16,1), numRows=4, numCols=2, lowessOn=1, loessBin=1/4, xlim=c(1975,2010))


####################################################################################
### 	SIZE-SELECTIVITY CURVES (Fig 4.2)
####################################################################################

plotSizeSelex <- function(replist, FleetNums=c(1,2,3,4,5,6,7,8,9,14,15), numRows, numCols)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSexes <- replist$nsexes
	numFleets <- replist$nfleets
	SizeSelexDat <- replist$sizeselex
	numSizeBins <- replist$nlbins
	SizeBins <- replist$lbinspop
	# Subset SizeSelexDat for endYr and gender=1
	SizeSelex <- subset(SizeSelexDat, Yr==endYr & Sex==1)
	FleetNames <- replist$FleetNames

	# Plot
	windows(8,6)
	par(mfrow=c(numRows,numCols), mar=c(2,2,2,1), omi=c(.5,.5,.2,0))
	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			plot(SizeBins, SizeSelex[ifleet,6:dim(SizeSelex)[2]], type="l", ylim=c(0,1), ylab="",xlab="", col="black", lwd=1.25, cex.axis=.75, mgp=c(2,0.5,0), tcl=-.3, las=1)
			leg <- FleetNames[ifleet]
			#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
			title(main=leg, cex.main=1)
		}
	}

	#legend(5,0.95,lwd=2, col=c("black", "slate grey"), legend=c("SS3"))
	mtext(side=1, outer=T, "Length (cm)-Talla (cm)", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Selectivity and retention - Selectividad e retencion", line=1.5, cex=1.25)
}
#plotSizeSelex(replist=myreplist, FleetNums=seq(1,9), numRows=3, numCols=3)

	
####################################################################################
### 	STOCK-RECRUITMENT RELATIONSHIP (Fig 4.4)
####################################################################################

plotSR <- function(replist, year1, convertTon, recUnits, relVirg)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	TSraw <- replist$timeseries
	
	# Prepare quantities for plot
	TSraw$Yr <- TSraw$Yr + (TSraw$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat <- TSraw[TSraw$Yr <= endYr+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat <- TSdat[TSdat$Seas==1 & TSdat$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	
	# Covert units of biomass (kg to ton) and recruitment (to millions of fish)
	if(convertTon==0)
	{
		SpawnBio <- TSdat$SpawnBio
		Rec <- TSdat$Recruit_0
	}
	if(convertTon==1)
	{
		SpawnBio <- TSdat$SpawnBio/1000
		Rec <- TSdat$Recruit_0/recUnits
	}
	
	# ABSOLUTE or RELATIVE quantities (to virgin)
	if(relVirg==1)
	{
		SpawnBio <- SpawnBio/SpawnBio[1]
		Rec <- Rec/Rec[1]
	}
	
	x <- SpawnBio[3:length(SpawnBio)]
	y <- Rec[3:length(Rec)]

	# Make plot
	windows(8,5)
	par(mar=c(4,6,2,1))
	if(relVirg==0){xlim <- range(0,pretty(x))}
	if(relVirg==1){xlim <- c(0,1)}
	ylim <- range(0,pretty(y))
		
	options(scipen=1) 				# Do not use scientific notation in plotting
	plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="p", col="black", ylab="",xlab="", lwd=1.25,las=1, pch=19)
	#if(relVirg==1) {abline(h=1, lty="solid", lwd=1.25)}
	if(relVirg==1) {lines(c(0,1),c(1,1), lty="solid", lwd=1.25)}


	
	# Plot Labels
	if(relVirg==0)
	{
		title(xlab="Spawning biomass (t)-Biomasa reproductora (t)", cex.lab=1.25, line=2.5)
		title(ylab="Recruitment (millions of fish)", cex.lab=1.25, line=4.5)
		title(ylab="Reclutamiento (millones de peces)", cex.lab=1.25, line=3)
	}
	if(relVirg==1)
	{
		title(xlab="Relative spawning biomass-Biomasa reproductora relativa", cex.lab=1.25, line=2.5)
		title(ylab="Relative recruitment-Reclutamiento relativo", cex.lab=1.25, line=2.5)
	}
 
}
#plotSR(myreplist, year1=1975, convertTon=1, recUnits=1000000, relVirg=1)


	
####################################################################################
### 	STOCK-RECRUITMENT RELATIONSHIP (Fig 4.4)
####################################################################################

plotSRwSteep <- function(replist, year1, convertTon, recUnits, relRec)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	SRraw <- replist$recruit
	
	# Prepare quantities for plot
	SRdat <- SRraw[SRraw$era == c("Main"),]  	# Subset TimeSeries for historical era (include first forecast yr)	
	SRdat <- SRdat[order(SRdat$spawn_bio),]
			
	# Covert units of biomass (kg to ton) and recruitment (to millions of fish)
	if(convertTon==0)
	{
		SpawnBio <- SRdat$spawn_bio
		RecObs <- SRdat$pred_recr
		RecExp <- SRdat$exp_recr
	}
	if(convertTon==1)
	{
		SpawnBio <- SRdat$spawn_bio/1000
		RecObs <- SRdat$pred_recr/recUnits
		RecExp <- SRdat$exp_recr/recUnits
	}
	
	# ABSOLUTE or RELATIVE quantities (to average rec)
	if(relRec==1)
	{
		SpawnBio <- SpawnBio/mean(SpawnBio)
		RecObs <- SRdat$pred_recr/mean(SRdat$exp_recr)
		RecExp <- SRdat$exp_recr/mean(SRdat$exp_recr)
	}
	
	# Make plot
	# Prepare quantitie for plot
	x <- SpawnBio
	yObs <- RecObs
	yExp <- RecExp

	windows(8,5)
	par(mar=c(4,6,2,1))
	if(relRec==0){xlim <- range(0,pretty(x))}
	xlim <- range(0,pretty(x))
	#if(relRec==1){xlim <- c(0,1)}
	ylim <- range(0,pretty(yObs))
		
	options(scipen=1) 				# Do not use scientific notation in plotting
	plot(x, yObs, xlim=xlim, ylim=ylim, axes=T, type="p", col="black", ylab="",xlab="", lwd=1.25,las=1, pch=19)
	lines(x,yExp, lty="solid", lwd=2, col="blue")

	# Plot Labels
	if(relRec==0)
	{
		title(xlab="Spawning biomass (t) - Biomasa reproductora (t)", cex.lab=1.25, line=2.5)
		title(ylab="Recruitment (millions of fish)", cex.lab=1.25, line=4.5)
		title(ylab="Reclutamiento (millones de peces)", cex.lab=1.25, line=3)
	}
	if(relRec==1)
	{
		title(xlab="Relative spawning biomass-Biomasa reproductora relativa", cex.lab=1.25, line=2.5)
		title(ylab="Relative recruitment-Reclutamiento relativo", cex.lab=1.25, line=3)
	}
 
}
#plotSRwSteep(myreplist, year1=1975, convertTon=1, recUnits=1000000, relRec=0)


####################################################################################
###		CPUE MODEL FIT (Fig 4.10)
####################################################################################

plotCPUEfit <- function(replist, year1, convertTon, FleetNums, FOfleetNums, LLfleetNums, numRows, numCols, xlim)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSeasons <- replist$nseasons
	numAreas <- replist$nareas
	numSexes <- replist$nsexes
	numFleets <- replist$nfleets
	CPUEdata <- replist$cpue
	FleetNames <- replist$FleetNames

	# Make table with CPUE time series    
	# Convert from quarters (EPO models) to year values
	CPUEdata$Yr2 <-  year1 + (CPUEdata$Yr/4)-0.25
	#xlim <- range(pretty(CPUEdata$Yr2))
	if(length(xlim)==1){xlim <- range(pretty(CPUEdata$Yr2))}
	if(length(xlim)==2){xlim <- xlim}

	# Plot
	if(numRows==1 & numCols==1){windows(8,4)}
	if(numRows!=1 & numCols!=1) {windows(8,6)}
	if(numRows==2 & numCols==1) {windows(8,6)}
	if(numRows>2 & numCols==1) {windows(6,8)}
	par(mfrow=c(numRows,numCols), mar=c(2,2,0.5,1), omi=c(.5,1,.2,0))

	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			# Prepare quantities for plot
			CPUEdataTemp <- subset(CPUEdata,FleetNum==ifleet)
			if(dim(CPUEdataTemp)[1]!=0)
			{
				#mark change start			
				# Prepare quantities for FO fisheries
				Wfactor <-1
				if(convertTon==1){Wfactor <-1000}
				if(ifleet%in%FOfleetNums)
				{
					x <- CPUEdataTemp$Yr2
					yCIup <- exp(log(CPUEdataTemp$Obs) + (1.96*CPUEdataTemp$SE))
					yCIlo <- exp(log(CPUEdataTemp$Obs) - (1.96*CPUEdataTemp$SE))
					CPUEobs <- CPUEdataTemp$Obs/Wfactor
					CPUEexp <- CPUEdataTemp$Exp/Wfactor
					yCIup<-yCIup/Wfactor
					yCIlo<-yCIlo/Wfactor
				}

				# Prepare quantities for LL fisheries
				if(ifleet%in%LLfleetNums)
				{
					x <- CPUEdataTemp$Yr2
					yCIup <- exp(log(CPUEdataTemp$Obs) + (1.96*CPUEdataTemp$SE))
					yCIlo <- exp(log(CPUEdataTemp$Obs) - (1.96*CPUEdataTemp$SE))
					CPUEmean <- mean(CPUEdataTemp$Obs)
					CPUEobs <- CPUEdataTemp$Obs/CPUEmean
					CPUEexp <- CPUEdataTemp$Exp/CPUEmean
					yCIup <- yCIup/CPUEmean
					yCIlo <- yCIlo/CPUEmean

				}
				#mark change end

				# Make plot
				ymax <- 1.25 * max(yCIup, na.rm=T)
				ylim <- range(0,pretty(ymax))
				ylim <- range(0,ymax)

				plot(x,CPUEexp, type="l", xlim=xlim, ylim=ylim, ylab="",xlab="", col="blue", lwd=2, cex.axis=.75, mgp=c(2,0.5,0), tcl=-.3, las=1)
				points(x,CPUEobs, col="black", pch=19,cex=.75)
				for (irow in 1:length(x)) {lines(c(x[irow],x[irow]),c(yCIup[irow],yCIlo[irow]))}				

				leg <- FleetNames[ifleet]
				#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
				text(xlim[1]+2, ymax, leg, adj=c(0,1), cex=1)
			}
		}
	}
	
	# Make axis Labels
	#mtext(side=1, outer=T, "Year-Ano", line=1.5, cex=1.25)
	if(numRows==1) {mtext(side=1, outer=T, "Year-Ano", line=0.5, cex=1.0)}
	if(numRows!=1) {mtext(side=1, outer=T, "Year-Ano", line=0.5, cex=1.25)}

	if(numRows==1 & numCols==1)
	{
		mtext(side=2, outer=T, "Catch per day (t) - Captura por dia (t)", line=2.5, cex=1.0)
		mtext(side=2, outer=T, "Standardized CPUE - CPUE estandarizada", line=0.5, cex=1.0)
	}

	if(numRows==2 & numCols==1)
	{
		mtext(side=2, outer=T, "Catch per day (t) - Captura por dia (t)", line=2.5, cex=1.25)
		mtext(side=2, outer=T, "Standardized CPUE - CPUE estandarizada", line=0.5, cex=1.25)
	}

	if(numRows>2)
	{
		mtext(side=2, outer=T, "Catch per day (t) - Captura por dia (t)", line=5, cex=1.25)
		mtext(side=2, outer=T, "Standardized CPUE - CPUE estandarizada", line=1.5, cex=1.25)
	}

}
#plotCPUEfit(replist=myreplist, year1=1975, convertTon=1, FleetNums=c(2,3,5,8,9), FOfleetNums=c(1,2,3,4,5,6,7), LLfleetNums=c(8,9), numRows=5, numCols=1, xlim=c(1975,2011))


####################################################################################
###		SIZE COMPOSITIONS - BUBBLE PLOTS (Fig 2.6)
####################################################################################

#replist=myreplist
#year1=1975
#FleetNums= seq(1,3,1)
#numRows=3
#numCols=1
#pcex1=.075

plotLFobsBub <- function(replist, year1, FleetNums, numRows, numCols, pcex1, xlim)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSizeBins <- replist$nlbins
	SizeBins <- replist$lbinspop
	numFleets <- replist$nfleets
	LFdata <- replist$lendbase
	FleetNames <- replist$FleetNames
	# Convert from quarters (EPO models) to year values
	LFdata$Yr2 <-  year1 + (LFdata$Yr/4)-0.25

	# Plot
	if(numRows==1 & numCols==1){windows(8,4)}
	if(numRows!=1) {windows(6,8)}
	par(mfrow=c(numRows,numCols), mar=c(2,2,1,1), omi=c(.5,.5,.2,0))
	#xlim <- range(pretty(LFdata$Yr2))
	if(length(xlim)==1){xlim <- range(pretty(LFdata$Yr2))}
	if(length(xlim)==2){xlim <- xlim}

	ylim <- range(0,max(SizeBins))
	#ylim <- range(0,180)

	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			# Prepare quantities for plot
			LFdataTemp <- subset(LFdata, Kind=="LEN" & Fleet==ifleet)
			ObsVals <- LFdataTemp$Obs
			x <- LFdataTemp$Yr2
			y <- LFdataTemp$Bin
			z <- ObsVals

			# Bubble are should increase proportionaly with value 
			# Set cex = 1 when p = pcex1 (passed as argument)
			# p is proportional to area of bubble (p=alpha*pi*radius^2)
			# cex = r = sqrt(p/(alpha*pi))
			# alpha = p/((cex^2)*pi), since cex=1 then alpha = p/pi 
			alpha <-  pcex1/pi
			cex <- sqrt(z/(alpha*pi))
						
			plot(x, y, xlab="Year", ylab="Length (cm)", xlim=xlim, ylim=ylim, cex=cex, pch=16, col="grey40")
			abline(h=75,col="red",lty="dashed",lwd=3)
			abline(h=125, col="green",lty="dashed",lwd=3)

			leg <- FleetNames[ifleet]
			#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
			
			text(xlim[1]+2, .1* max(SizeBins), leg, adj=c(0,1), cex=1.5)
			#text(xlim[1]+2, max(SizeBins), leg, adj=c(0,1), cex=1.5)

		}
	}
	mtext(side=1, outer=T, "Year-Ano", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Length (cm)- Talla (cm)", line=1.5, cex=1.25)
}
#plotLFobsBub(replist=myreplist, year1=1975, FleetNums= seq(1,3,1), numRows=3, numCols=1, pcex1=.075, xlim=c(1975,2010))
#plotLFobsBub(replist=myreplist, year1=1975, FleetNums= seq(4,6,1), numRows=3, numCols=1, pcex1=.075, xlim=c(1975,2010))
#plotLFobsBub(replist=myreplist, year1=1975, FleetNums= seq(7,9,1), numRows=3, numCols=1, pcex1=.05, xlim=c(1975,2010))


####################################################################################
###		PEARSON RESIDUAL PLOTS FOR LENGTH COMPS (Fig 4.11)
####################################################################################

plotLFresidsBub <- function(replist, year1, FleetNums, numRows, numCols, pcex1, xlim)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSizeBins <- replist$nlbins
	SizeBins <- replist$lbinspop
	numFleets <- replist$nfleets
	
	
	LFdata <- replist$lendbase
	FleetNames <- replist$FleetNames
	# Convert from quarters (EPO models) to year values
	LFdata$Yr2 <-  year1 + (LFdata$Yr/4)-0.25

	# Plot
	if(numRows==1 & numCols==1){windows(8,4)}
	if(numRows!=1) {windows(6,8)}
	par(mfrow=c(numRows,numCols), mar=c(2,2,1,1), omi=c(.5,.5,.2,0))
	#xlim <- range(pretty(LFdata$Yr2))
	if(length(xlim)==1){xlim <- range(pretty(LFdata$Yr2))}
	if(length(xlim)==2){xlim <- xlim}

	
	ylim <- range(0,max(SizeBins))

	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			# Prepare quantities for plot
			NegResids <- subset(LFdata,  Kind=="LEN" & Fleet==ifleet & LFdata$Pearson<0)
			PosResids <- subset(LFdata,  Kind=="LEN" & Fleet==ifleet & LFdata$Pearson>0)			
			cexNeg <- abs(NegResids$Pearson)
			cexPos <- PosResids$Pearson
			
			alpha <-  pcex1/pi
			cexNeg <- sqrt(cexNeg/(alpha*pi))
			cexPos <- sqrt(cexPos/(alpha*pi))

			plot(NegResids$Yr2, NegResids$Bin, xlab="Year", ylab="Length (cm)", xlim=xlim, ylim=ylim, cex=cexNeg, pch=16, col=gray(0.6), las=1)
			points(PosResids$Yr2, PosResids$Bin, cex=cexPos, pch=16, col="black")
			
			leg <- FleetNames[ifleet]
			#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
			#title(main=leg, cex.main=1)
			#text(xlim[1]+2, max(SizeBins), leg, adj=c(0,1), cex=1.5)
			text(xlim[1]+2, .1* max(SizeBins), leg, adj=c(0,1), cex=1.5)

		}
	}
	mtext(side=1, outer=T, "Year-Ano", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Lenght (cm)- Talla (cm)", line=1.5, cex=1.25)
}
#plotLFresidsBub(replist=myreplist, year1=1975, FleetNums= seq(1,3,1), numRows=3, numCols=1, pcex1=.5,  xlim=c(1975,2010))
#plotLFresidsBub(replist=myreplist, year1=1975, FleetNums= seq(4,6,1), numRows=3, numCols=1, pcex1=1, xlim=c(1975,2010))
#plotLFresidsBub(replist=myreplist, year1=1975, FleetNums= seq(7,9,1), numRows=3, numCols=1, pcex1=1, xlim=c(1975,2010))


####################################################################################
###		PEARSON RESIDUAL PLOTS FOR GENERALIZED SIZE COMPS ####################################################################################

plotGenSizeresidsBub <- function(replist, year1, FleetNums, numRows, numCols, pcex1, xlim,type=1,method=1)
{
	# For generalized size option (weight data, etc)
	## if type =1  then is weight (for the lables)
	##  otherwise is length
	
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSizeBins <- replist$nlbins
			#SizeBins <- replist$lbinspop
	SizeBins <-as.vector(replist$sizebinlist[[method]])
	#numFleets <- replist$nfleets
		numFleets <- replist$nfishfleets # only fisheries fleets <>< Change 15 March 2016

			#LFdata <- replist$lendbase
	LFdata <- replist$sizedbase
	FleetNames <- replist$FleetNames
	# Convert from quarters (EPO models) to year values
	LFdata$Yr2 <-  year1 + (LFdata$Yr/4)-0.25

	# Plot
	if(numRows==1 & numCols==1){windows(8,4)}
	if(numRows!=1) {windows(6,8)}
	par(mfrow=c(numRows,numCols), mar=c(2,2,1,1), omi=c(.5,.5,.2,0))
	#xlim <- range(pretty(LFdata$Yr2))
	if(length(xlim)==1){xlim <- range(pretty(LFdata$Yr2))}
	if(length(xlim)==2){xlim <- xlim}

	
	ylim <- range(0,max(SizeBins))

	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			# Prepare quantities for plot
			NegResids <- subset(LFdata,  Kind=="SIZE" & Fleet==ifleet & LFdata$Pearson<0)
			PosResids <- subset(LFdata,  Kind=="SIZE" & Fleet==ifleet & LFdata$Pearson>0)			
			cexNeg <- abs(NegResids$Pearson)
			cexPos <- PosResids$Pearson
			
			alpha <-  pcex1/pi
			cexNeg <- sqrt(cexNeg/(alpha*pi))
			cexPos <- sqrt(cexPos/(alpha*pi))

			if(type) plot(NegResids$Yr2, NegResids$Bin, xlab="Year", ylab="Weight (Kg)", xlim=xlim, ylim=ylim, cex=cexNeg, pch=16, col=gray(0.6), las=1)
			else plot(NegResids$Yr2, NegResids$Bin, xlab="Year", ylab="Length (cm)", xlim=xlim, ylim=ylim, cex=cexNeg, pch=16, col=gray(0.6), las=1)

			points(PosResids$Yr2, PosResids$Bin, cex=cexPos, pch=16, col="black")
			
			leg <- FleetNames[ifleet]
			#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
			#title(main=leg, cex.main=1)
			#text(xlim[1]+2, max(SizeBins), leg, adj=c(0,1), cex=1.5)
			text(xlim[1]+0.5, .97* max(SizeBins), leg, adj=c(0,1), cex=1.5)

		}
	}
	mtext(side=1, outer=T, "Year-Ano", line=1.5, cex=1.25)
	if(type) mtext(side=2, outer=T, "Weight (kg) - Peso (kg)", line=1.5, cex=1.25)
	else mtext(side=2, outer=T, "Lenght (cm)- Talla (cm)", line=1.5, cex=1.25)
}

#plotGenSizeresidsBub(replist=myreplist, year1=1975, FleetNums= c(17,18), numRows=2, numCols=1, pcex1=1, xlim=c(1975,2000))


####################################################################################
###		MEAN OBS AND EXPECTED LF
####################################################################################

#replist=myreplist
#FleetNums=seq(1,7,1)
#numRows=3
#numCols=3
#yMaxFix=.08

plotLFmean <- function(replist, FleetNums, numRows, numCols, yMaxFix)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSizeBins <- replist$nlbins
	SizeBins <- replist$lbinspop
#	numFleets <- replist$nfleets
		numFleets <- replist$nfishfleets # only fisheries fleets <>< Change 15 March 2016

	LFdata <- replist$lendbase
	FleetNames <- replist$FleetNames

	# Convert from quarters (EPO models) to year values
	#LFdata$Yr2 <-  year1 + (LFdata$Yr/4)-0.25

	# Plot
	windows(8,6)
	par(mfrow=c(numRows,numCols), mar=c(2,2,2,2), omi=c(.5,.75,.2,0))	
	xlim <- range(0,max(SizeBins))
	ymax <- 1.4*(max(tapply(LFdata$Obs,list(LFdata$Bin,LFdata$Fleet),mean),tapply(LFdata$Exp,list(LFdata$Bin,LFdata$Fleet),mean),na.rm=T))
	if(yMaxFix!=0) {ymax=yMaxFix}
	ylim <- range(0,ymax)

	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{

			# Prepare quantities for plot
			LFdataTemp <- subset(LFdata, Kind=="LEN" & Fleet==ifleet)
			#ObsMean <- tapply(LFdataTemp$Obs,LFdataTemp$Bin,mean)	# Compute Obs mean
			ObsMean<-tapply((LFdataTemp$Obs*LFdataTemp$effN),LFdataTemp$Bin,sum)/tapply(LFdataTemp$effN,LFdataTemp$Bin,sum)
			#ExpMean <- tapply(LFdataTemp$Exp,LFdataTemp$Bin,mean)	# Compute Exp mean
			ExpMean <- tapply((LFdataTemp$Exp*LFdataTemp$effN),LFdataTemp$Bin,sum)/tapply(LFdataTemp$effN,LFdataTemp$Bin,sum)	# Compute Exp mean
			
			Bins <- tapply(LFdataTemp$Bin,LFdataTemp$Bin,mean)
			#ObsMean <- tapply(LFdataTemp$Obs,LFdataTemp$Bin,weighted.mean)	# Compute Obs mean
			#ExpMean <- tapply(LFdataTemp$Exp,LFdataTemp$Bin,weighted.mean)	# Compute Exp mean
			#Bins <- tapply(LFdataTemp$Bin,LFdataTemp$Bin,weighted.mean)

			#plot(Bins, ExpMean, xlim=xlim, ylim=ylim, type="l",las=1, lwd=2, col="blue")
			#points(Bins, ObsMean, cex=.75, pch=16, col="black")
			
			#plot(Bins, ObsMean, xlim=xlim, ylim=ylim, type="p",las=1, col="black", cex=.75, pch=16)
		    plot(0,0,xlim=xlim, ylim=ylim,las=1, type="n")
			polygon(c(Bins,rev(Bins)), c(rep(0,length(Bins)),rev(ObsMean)), col='grey', border="black", lty=1)
			lines(Bins, ExpMean, lwd=2, col="red")

			leg <- FleetNames[ifleet]
			#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
			#text(xlim[1]+5, ymax, leg, adj=c(0,1), cex=1.0)
			text(xlim[1]+5, ymax, leg, adj=c(0,1), cex=.75) # for sensitivity
		}
	}
	mtext(side=1, outer=T, "Lenght (cm)- Talla (cm)", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Proportion of the catch - Proporcion de la captura", line=2.5, cex=1.25)
}
#plotLFmean(replist=myreplist, FleetNums=seq(1,9,1), numRows=3, numCols=3, yMaxFix=0.5)

#plotLFmean(replist=myreplist, FleetNums=seq(1,7,1), numRows=3, numCols=3, yMaxFix=.08)

	
####################################################################################
###		LF FIT to individual size comps   (TO FINISH)
####################################################################################


#replist=myreplist
#year1=1975
#fleetNum=1
#Years=c(2006,2007)

plotLFfits <- function(replist, year1, fleetNum, Years, yMaxFix)
{
	# Get quantities from replist
	startYr <- replist$startyr
	endYr <- replist$endyr
	numSizeBins <- replist$nlbins
	SizeBins <- replist$lbinspop
 #	numFleets <- replist$nfleets
	numFleets <- replist$nfishfleets # only fisheries fleets <>< Change 15 March 2016

	LFdata <- replist$lendbase
	# Convert from quarters (EPO models) to year values
	LFdata$Yr2 <-  year1 + (LFdata$Yr/4)-0.25
	LFdata$YrFloor <- floor(LFdata$Yr2) 
	LFdata$Qrt <- (LFdata$Yr2 - LFdata$YrFloor + 0.25) / 0.25 
	FleetNames <- replist$FleetNames

	# Subset LF data for fishery and period of interest
	LFdataTemp <- subset(LFdata, Fleet==fleetNum & Kind=="LEN" & YrFloor%in%Years)
	fleetName <- FleetNames[fleetNum]

	# Plot
	numRows <- length(Years)*4

	windows(4,8)
	par(mfrow=c(numRows,1), mar=c(2,2,0,2), omi=c(.5,.75,.5,0))	
	xlim <- range(0,max(SizeBins))
	ymax <- 1.4*(max(LFdataTemp$Obs, LFdataTemp$Exp, na.rm=T))
	if(yMaxFix!=0) {ymax=yMaxFix}
	ylim <- range(0,ymax)

	# Loop over years
	for(iyear in 1:length(Years))
	{
		yearTemp <- Years[iyear]
		# Loop over quarters
		Quarters <- seq(1:4)
		for(iqrt in 1:length(Quarters))
		{
			qrtTemp <- Quarters[iqrt]
			LFdataTemp <- subset(LFdata, Fleet==fleetNum & Kind=="LEN" & YrFloor==yearTemp & Qrt==qrtTemp)
			Obs <- LFdataTemp$Obs
			Exp <- LFdataTemp$Exp
			Bins <- LFdataTemp$Bin
			
			#plot(LFdataTemp$Bin, LFdataTemp$Exp, xlim=xlim, ylim=ylim, type="l", lwd=3, col="blue", mgp=c(2,0.5,0), tcl=-.3, las=1)
			#points(LFdataTemp$Bin, LFdataTemp$Obs, cex=.75, pch=16, col="black")
			
			#plot(Bins, ObsMean, xlim=xlim, ylim=ylim, type="p",las=1, col="black", cex=.75, pch=16)
		    plot(0,0,xlim=xlim, ylim=ylim,las=1, type="n")
			polygon(c(Bins,rev(Bins)), c(rep(0,length(Bins)),rev(Obs)), col='grey',  border="black", lty=1)
			lines(Bins, Exp, lwd=2, col="red")

			leg <- paste(yearTemp," Quarter ",qrtTemp," - Trimestre ", qrtTemp, sep="")
			text(xlim[1]+5, ymax, leg, adj=c(0,1), cex=1.25)
		}
	}
	mtext(side=1, outer=T, "Lenght (cm)- Talla (cm)", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Proportion of the catch - Proporcion de la captura", line=2, cex=1.25)
	mtext(side=3, outer=T, paste("Fishery ", fleetName, sep=""), line=1.5, cex=1.25)

}
#plotLFfits(replist=myreplist, year1=1975, fleetNum=2, Years=c(2007,2008),  yMaxFix=0)


####################################################################################
###		Make table of management quantities
####################################################################################


makeManagTable <- function(replist, Path)
{
  # replist <- myreplist0
  # Path <- Path0
	# Get quantities from replist
	TimeSeries <- replist$timeseries
	#numFleets <- replist$nfleets # all fleets including surveys
	numFleets <- replist$nfishfleets # only fisheries fleets <>< Change 15 March 2016
	endYr <- replist$endyr

	# Make forecast management report name
	ForeRepName <- paste(Path, "Forecast-report.SSO" ,sep="")
	# Get management report
	ForeRepStart <- grep("Management_report", readLines(ForeRepName))
	ForeRepEnd <- grep("THIS FORECAST FOR PURPOSES", readLines(ForeRepName))[1]

	#ForeDat <- read.table(file=ForeRepName,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=45, skip = ForeRepStart-1)
	ForeDat <- read.table(file=ForeRepName,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=ForeRepEnd-ForeRepStart, skip = ForeRepStart-1)
	ForeDat <- as.data.frame(ForeDat)

	# Make catch headers to subset
	HeadersC <- rep(NA,numFleets)
	for(ifleet in 1:numFleets)
	{
		headerTemp <- paste("sel(B):_", ifleet, sep="")
		HeadersC[ifleet] <- headerTemp
	}
	# Make table with forecast time series
	ForeTS <- subset(TimeSeries, select=c("Yr", "Era", "Bio_smry", "SpawnBio", HeadersC))
	#Brecent <- ForeTS[ForeTS$Yr==endYr+1,3]/1000
	#Srecent <- ForeTS[ForeTS$Yr==endYr+1,4]/1000
	#Crecent <- sum(ForeTS[ForeTS$Yr%in%seq(endYr-3,endYr,1),5:dim(ForeTS)[2]])/1000				
	Brecent <- ForeTS[ForeTS$Yr==endYr+1,3]
	Srecent <- ForeTS[ForeTS$Yr==endYr+1,4]

	Crecent <- sum(ForeTS[ForeTS$Yr%in%seq(endYr-3,endYr,1),5:dim(ForeTS)[2]])				
	
	options(scipen=2) 				# Do not use scientific notation in plotting
	# Get management quantities
	# msy
	#msy <- as.numeric(ForeDat[ForeDat[,1]==c("MSY_for_optimize"),5])*4/1000
	msy <- as.numeric(ForeDat[ForeDat[,1]==c("MSY_for_optimize"),2])*4
	# Bmsy
	#Bmsy <- as.numeric(ForeDat[ForeDat[,1]==c("Biomass_Smry"),5])/1000
	Bmsy <- as.numeric(ForeDat[ForeDat[,1]==c("Biomass_Smry"),2])
	Bmsy <- Bmsy[length(Bmsy)]
	# Smsy
	#Smsy <- as.numeric(ForeDat[ForeDat[,1]==c("SPBio"),5])/1000
	Smsy <- as.numeric(ForeDat[ForeDat[,1]==c("SPBio"),2])
	Smsy <- Smsy[length(Smsy)]
	# Bmsy/B0
	#Bzero <- as.numeric(ForeDat[ForeDat[,1]==c("BIO_Smry_unfished"),5])/1000
	Bzero <- as.numeric(ForeDat[ForeDat[,1]==c("BIO_Smry_unfished(Bmark)"),2])
	BmsyBzero <-Bmsy/Bzero
	# Smsy/S0
	SmsySzero <- as.numeric(ForeDat[ForeDat[,1]==c("SPBmsy/SPB_virgin"),2])
	CrecentMsy <- Crecent/msy
	# Brecent/Bmsy
	BrecentBmsy <- Brecent/Bmsy
	# S recent/Smsy
	SrecentSmsy <- Srecent/Smsy
	
	# Methot takes the F by fishery averaged over the given years and makes it sum to 1
	# So the Fmult to use is not the one given in the output but Fmult/sum(F1,F2,...)
	 # Compute the average F vector in absolute (rather than scaled to 1 terms) 
	FvectorRepStart <- grep("Seasonal_apicalF=Fmult", readLines(ForeRepName))
	Fvector <- read.table(file=ForeRepName, nrows=1, skip = FvectorRepStart[1]+1)
	Fvector <- Fvector[3:length(Fvector)]
	FmultScale <- sum(Fvector)
	# Fmultiplier
	Fmult <- as.numeric(ForeDat[ForeDat[,1]==c("Fmult"),2])[3]
	Fmult <- Fmult/FmultScale

	# Make table with management quantities
	RowNames <- c("msy","Bmsy","Smsy","Bmsy/Bzero","Smsy/Szero","Crecent/msy",
					"Brecent/Bmsy", "Srecent/Smsy", "Fmultiplier")
	ManagTable <- matrix(NA, length(RowNames),2)
	ManagTable <- data.frame(ManagTable)
	names(ManagTable) <- c("quant", "val")
	# Populate table with quantities
	ManagTable[,1] <- RowNames
	ManagTable[1,2] <- format(msy,digits=1)
	ManagTable[2,2] <- format(Bmsy,digits=1)
	ManagTable[3,2] <- format(Smsy, digits=1)
	ManagTable[4,2] <- format(BmsyBzero, digits=2, nsmall=2)
	ManagTable[5,2] <- format(SmsySzero, digits=2, nsmall=2)
	ManagTable[6,2] <- format(CrecentMsy, digits=2, nsmall=2)
	ManagTable[7,2] <- format(BrecentBmsy, digits=2, nsmall=2)
	ManagTable[8,2] <- format(SrecentSmsy, digits=2, nsmall=2)
	ManagTable[9,2] <- format(Fmult, digits=2, nsmall=2)
	
	Out <- list(Fvector=Fvector, FmultScale=FmultScale, ManagTable=ManagTable)
	#print(Out)
	return(Out)
}
#ManagTable <- makeManagTable(replist=myreplist, Path=Path)


##########################################
##	FISHING MORTALITY RATES (To FINISH)
##########################################


#myreplist$startyr
#myreplist$endyr

#Times <- seq(myreplist$startyr, myreplist$endyr)  
#Year <- yearStart+(Times-1)/4

# Arguments needed for function
ageMax <- 40 		# Improve - get this from the repfile, not available at the moment
yearStart <- 1975  # Defined by used
TimeBins  <- rbind(c(1975,1992),c(1993,2011))
AgeBins <- rbind(c(1,4),c(5,8),c(9,12),c(13,19),c(20,ageMax-1))

makeFplots <- function(replist, ageMax, yearStart, TimeBins)
{
	###################################
	##	STEP 1: Compute FatAge matrix
	###################################
	
	FatAge <- myreplist$Z_at_age - myreplist$M_at_age
	FatAge[,1:3] <- myreplist$Z_at_age[,1:3]
	Yr <- yearStart+(FatAge$Year-1)/4
	# Add real years from starting year
	YrFloor <- floor(Yr)
	FatAge <- cbind(Yr,YrFloor,FatAge)
	
		
	########################################
	##	STEP 2: Compute Faverage table
	########################################
	
	# Make Faverage output table
	# If two gender model, need to compute Favg for each sex within each time peridod
	#  the final Favg for each specified time peridod will be the mean between the respective Favg for females and males
	FavgOutput <- as.data.frame(matrix(0, myreplist$nsexes*dim(TimeBins)[1]+2 ,ageMax+1))
	names(FavgOutput) <- paste("age",seq(0,ageMax,1), sep="")
	FavgOutput <- as.data.frame <- FavgOutput
	
	# Initialize the F output matrix (with no rows)
	FavgOut <- matrix(0,0,41)

	icount=1	# set counter to 1 before begging loop
	# Loop over sexes
	for(i in 1:myreplist$nsexes)
	{
		sexTemp <- i
	
		# Loop over specified periods to calculate Favg
		for(i in 1:dim(TimeBins)[1])
		{
			# Get start and end years
			yrStartTemp <- TimeBins[i,1]
			yrEndTemp <- TimeBins[i,2]

			# Make Favg output row name
			nameVecFavgOut <- paste("sex",sexTemp,".",yrStartTemp,".",yrEndTemp, sep="")
		
			# Favg calcs
			FatAgeTemp <- subset(FatAge, Gender==sexTemp & YrFloor%in%seq(yrStartTemp,yrEndTemp,1))
			FavgTemp <- colMeans(FatAgeTemp[,6:dim(FatAgeTemp)[2]], na.rm=T)
		
			# Bind output Favg vector to output table
			FavgOut <- rbind(FavgOut,as.numeric(FavgTemp))
			row.names(FavgOut)[icount] <- nameVecFavgOut
		
			icount = icount +1
		}
	}
	
	# THIS IS NOT WORKING NEED TO THINK OF A BETTER WAY TO DO IT. THE AVG SHOULD ACTUALLY COME RIGHT AFTER THE TWO SEX ROWS FOR EACH PERIOD
	# If two gender model, compute Favg between females and males for each specified perido
	# DOING THIS MANUALLY HERE, NEED TO IMPROVE!!
	if(myreplist$nsexes==2)
	{
		sex1.Favg <- colMeans(FavgOut[c(1,3),])
		sex2.Favg <- colMeans(FavgOut[c(2,4),])
	
		FavgOut <- rbind(FavgOut,sex1.Favg,sex2.Favg)
	}

	#######################################################
	##	STEP 2: Compute Faverage table by age groups
	#######################################################
	
	# Compute quarterly table
	FatAgeGrps <- FatAge[,1:5]	# Get the first 5 cols with gender and qrt info
	# Prepare output table
	FatAgeGrpsOut <- matrix(0,dim(FatAge)[1],0)
	FatAgeGrpsOut <- as.data.frame(FatAgeGrpsOut)
	
	for(i in 1:dim(AgeBins)[1])
	{
		# Get start and end years
		AgeBinLoTemp <- AgeBins[i,1]
		AgeBinHiTemp <- AgeBins[i,2]
		ageGrpName <- paste('ages',AgeBinLoTemp,'.',AgeBinHiTemp, sep="")
		
		FatAgeGrpTemp <- rowMeans(FatAge[6+AgeBinLoTemp:AgeBinHiTemp])
		FatAgeGrpsOut <- cbind(FatAgeGrpsOut,FatAgeGrpTemp)
		names(FatAgeGrpsOut)[i] <- ageGrpName	
	}
	FatAgeGrps <- cbind(FatAgeGrps,FatAgeGrpsOut)

	# I'M HERE
	
	# Compute yearly table
	# use tapply for genders separately
	# Loop over gender
	
	# Lover over age groups
	tapply(FatAgeGrps$ages1.4,list(FatAgeGrps$YrFloor), sum)
	
	# Compute avg between males and females
		
	
	YFT.DOL.DF <- as.data.frame(tapply(PSdat$DOLdf, list(PSdat$time,PSdat$YFT.DOL.fish), sum))


		GridOv.Sel <- subset(EnvDatGrid, year==yearSel & month==monthSel)
	EnvMat <- tapply(GridOv.Sel$val, list(GridOv.Sel$lat,GridOv.Sel$lon),mean, na.omit=T)


	
}	
	


####################################################################################
### 	Time series (MULTIPLE CASES) - summary biomass
####################################################################################

plotBioSmryXcases <- function(Case1, Case2, Case3, Case4, Case5, Legends, year1, convertTon, retro, EndYrCases, legendOption, xlim)
{
	# Get quantities from replist
	startYr <- Case1$startyr
	numSeasons <- Case1$nseasons
	numAreas <- Case1$nareas
	TSraw.C1 <- Case1$timeseries
	TSraw.C2 <- Case2$timeseries
	if(class(Case3)=="list"){TSraw.C3 <- Case3$timeseries}
	if(class(Case4)=="list"){TSraw.C4 <- Case4$timeseries}
	if(class(Case5)=="list"){TSraw.C5 <- Case5$timeseries}

	# Define endYr
	# Standard analysis
	if(retro!=1)
	{
		endYr.C1 <- Case1$endyr
		endYr.C2 <- Case2$endyr
		if(class(Case3)=="list"){endYr.C3 <- Case3$endyr}
		if(class(Case4)=="list"){endYr.C4 <- Case4$endyr}
		if(class(Case5)=="list"){endYr.C5 <- Case5$endyr}
	}
	# Retrospective analysis
	if(retro==1)
	{
		endYr.C1 <- EndYrCases[1]
		endYr.C2 <- EndYrCases[2]
		endYr.C3 <- EndYrCases[3]
		endYr.C4 <- EndYrCases[4]
		endYr.C5 <- EndYrCases[5]
	}
	
	#####################################
    #   CASE 1 - prepare quantities
    #####################################

	# Prepare quantities for plot
	TSraw.C1$Yr <- TSraw.C1$Yr + (TSraw.C1$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat.C1 <- TSraw.C1[TSraw.C1$Yr <= endYr.C1+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat.C1 <- TSdat.C1[TSdat.C1$Seas==1 & TSdat.C1$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat.C1$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat.C1$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	if(convertTon==0) {BioSmry.C1 <- TSdat.C1$Bio_smry}
	if(convertTon==1) {BioSmry.C1 <- TSdat.C1$Bio_smry/1000}
	x.C1 <- Years
	y.C1 <- BioSmry.C1[3:length(BioSmry.C1)]
		
	######################################
    #   CASE 2 - prepare quantities
    ######################################

	# Prepare quantities for plot
	TSraw.C2$Yr <- TSraw.C2$Yr + (TSraw.C2$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat.C2 <- TSraw.C2[TSraw.C2$Yr <= endYr.C2+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat.C2 <- TSdat.C2[TSdat.C2$Seas==1 & TSdat.C2$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat.C2$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat.C2$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	if(convertTon==0) {BioSmry.C2 <- TSdat.C2$Bio_smry}
	if(convertTon==1) {BioSmry.C2 <- TSdat.C2$Bio_smry/1000}
	x.C2 <- Years
	y.C2 <- BioSmry.C2[3:length(BioSmry.C2)]

	######################################
    #   CASE 3 - prepare quantities
    ######################################

    if(class(Case3)=="list")
    {
	    # Prepare quantities for plot
	    TSraw.C3$Yr <- TSraw.C3$Yr + (TSraw.C3$Seas-1)/numSeasons
	    # Subset for Era of interest - Option 1 (HISTORICAL ERA)
	    TSdat.C3 <- TSraw.C3[TSraw.C3$Yr <= endYr.C3+1,]  	# Subset TimeSeries for historical era (include first forecast yr)
	    TSdat.C3 <- TSdat.C3[TSdat.C3$Seas==1 & TSdat.C3$Area==1,]
	    # Subset Years and Total Biomass quantities for plot
	    YearsRaw <- TSdat.C3$Yr
	    numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	    if(year1==1) {Years<- TSdat.C3$Yr}
	    if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	    if(convertTon==0) {BioSmry.C3 <- TSdat.C3$Bio_smry}
	    if(convertTon==1) {BioSmry.C3 <- TSdat.C3$Bio_smry/1000}
	    x.C3 <- Years
	    y.C3 <- BioSmry.C3[3:length(BioSmry.C3)]
    }

	######################################
    #   CASE 4 - prepare quantities
    ######################################

    if(class(Case4)=="list")
    {
	    # Prepare quantities for plot
	    TSraw.C4$Yr <- TSraw.C4$Yr + (TSraw.C4$Seas-1)/numSeasons
	    # Subset for Era of interest - Option 1 (HISTORICAL ERA)
	    TSdat.C4 <- TSraw.C4[TSraw.C4$Yr <= endYr.C4+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	    TSdat.C4 <- TSdat.C4[TSdat.C4$Seas==1 & TSdat.C4$Area==1,]
	    # Subset Years and Total Biomass quantities for plot
	    YearsRaw <- TSdat.C4$Yr
	    numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	    if(year1==1) {Years<- TSdat.C4$Yr}
	    if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	    if(convertTon==0) {BioSmry.C4 <- TSdat.C4$Bio_smry}
	    if(convertTon==1) {BioSmry.C4 <- TSdat.C4$Bio_smry/1000}
	    x.C4 <- Years
	    y.C4 <- BioSmry.C4[3:length(BioSmry.C4)]
    }

	######################################
    #   CASE 5 - prepare quantities
    ######################################

    if(class(Case5)=="list")
    {
	    # Prepare quantities for plot
	    TSraw.C5$Yr <- TSraw.C5$Yr + (TSraw.C5$Seas-1)/numSeasons
	    # Subset for Era of interest - Option 1 (HISTORICAL ERA)
	    TSdat.C5 <- TSraw.C5[TSraw.C5$Yr <= endYr.C5+1,]  	# Subset TimeSeries for historical era (include first forecast yr)
	    TSdat.C5 <- TSdat.C5[TSdat.C5$Seas==1 & TSdat.C5$Area==1,]
	    # Subset Years and Total Biomass quantities for plot
	    YearsRaw <- TSdat.C5$Yr
	    numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	    if(year1==1) {Years<- TSdat.C5$Yr}
	    if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	    if(convertTon==0) {BioSmry.C5 <- TSdat.C5$Bio_smry}
	    if(convertTon==1) {BioSmry.C5 <- TSdat.C5$Bio_smry/1000}
	    x.C5 <- Years
	    y.C5 <- BioSmry.C5[3:length(BioSmry.C5)]
    }

	#################
    #   Make plot 
    #################

	# Make plot
	windows(8,5)
	par(mar=c(4,8,2,1))
	#xlim <- range(pretty(x.C1))
	if(length(xlim)==1){xlim <- range(pretty(x.C1))}
	if(length(xlim)==2){xlim <- xlim}

	ylim <- range(0,max(y.C1,y.C2))
	if(class(Case3)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3))}
	if(class(Case4)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3,y.C4))}
	if(class(Case5)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3,y.C4,y.C5))}

	options(scipen=1) 				# Do not use scientific notation in plotting
	
	# Plot CASE1
	plot(x.C1, y.C1, xlim=xlim, ylim=ylim, axes=T, type="l",col="blue", ylab="",xlab="", lwd=2,las=1)
	# Plot yearly values
	x2.C1 <- unique(floor(x.C1))
	y2.C1 <- y.C1[x.C1%in%x2.C1]
	points(x2.C1,y2.C1, pch=19, col="blue", cex=1)
	
	# Plot CASE2
	lines(x.C2, y.C2, lty=2, pch=21, col="red", lwd=2)
	# Plot yearly values
	x2.C2 <- unique(floor(x.C2))
	y2.C2 <- y.C2[x.C2%in%x2.C2]
	points(x2.C2,y2.C2, pch=24, col="red", lwd=1.25, cex=1)

	# Plot CASE3
	if(class(Case3)=="list")
	{
		lines(x.C3, y.C3, lty=1, pch=21, col="green", lwd=2)
		# Plot yearly values
		x2.C3 <- unique(floor(x.C3))
		y2.C3 <- y.C3[x.C3%in%x2.C3]
		points(x2.C3,y2.C3, pch=22, col="green", lwd=1.25, cex=1)
	}

	# Plot CASE4
	if(class(Case4)=="list")
	{
		lines(x.C4, y.C4, lty=2, pch=21, col="black", lwd=2)
		# Plot yearly values
		x2.C4 <- unique(floor(x.C4))
		y2.C4 <- y.C4[x.C4%in%x2.C4]
		points(x2.C4,y2.C4, pch=23, col="black", lwd=1.25, cex=1)
	}

	# Plot CASE5
	if(class(Case5)=="list")
	{
		lines(x.C5, y.C5, lty=3, pch=21, col="purple", lwd=2)
		# Plot yearly values
		x2.C5 <- unique(floor(x.C5))
		y2.C5 <- y.C5[x.C5%in%x2.C5]
		points(x2.C5,y2.C5, pch=25, col="purple", lwd=1.25, cex=1)
	}

	# Make legend
	leg.C1 <- Legends[1]
	leg.C2 <- Legends[2]
	if(class(Case3)=="list"){leg.C3 <- Legends[3]}
	if(class(Case4)=="list"){leg.C4 <- Legends[4]}
	if(class(Case5)=="list"){leg.C5 <- Legends[5]}

	
	# Plot legend
	if(legendOption==0)
	{
		legend(x=x.C1[1]+2, y=.5*ylim[2], legend = leg.C1, lty=1, pch=19, cex=1, col="blue", lwd=2, bty="n")
		legend(x=x.C2[1]+2, y=.4*ylim[2], legend = leg.C2, lty=1, pch=24, cex=1, col="red", lwd=2, bty="n")
		if(class(Case3)=="list"){legend(x=x.C3[1]+2, y=.3*ylim[2], legend = leg.C3, lty=1, pch=22, cex=1, col="green", lwd=2, bty="n")}
		if(class(Case4)=="list"){legend(x=x.C4[1]+2, y=.2*ylim[2], legend = leg.C4, lty=2, pch=23, cex=1, col="black", lwd=2, bty="n")}
		if(class(Case5)=="list"){legend(x=x.C5[1]+2, y=.1*ylim[2], legend = leg.C5, lty=3, pch=25, cex=1, col="purple", lwd=2, bty="n")}
	}
	if(legendOption==1)
	{
		legend(x=x.C1[1]+20, y=1*ylim[2], legend = leg.C1, lty=1, pch=19, cex=.75, col="blue", lwd=2, bty="n")
		legend(x=x.C2[1]+20, y=.9*ylim[2], legend = leg.C2, lty=1, pch=24, cex=.75, col="red", lwd=2, bty="n")
		if(class(Case3)=="list"){legend(x=x.C3[1]+20, y=.8*ylim[2], legend = leg.C3, lty=1, pch=22, cex=.75, col="green", lwd=2, bty="n")}
		if(class(Case4)=="list"){legend(x=x.C4[1]+20, y=.7*ylim[2], legend = leg.C4, lty=2, pch=23, cex=.75, col="black", lwd=2, bty="n")}
		if(class(Case5)=="list"){legend(x=x.C5[1]+20, y=.6*ylim[2], legend = leg.C5, lty=3, pch=25, cex=.75, col="purple", lwd=2, bty="n")}
	}
	if(legendOption==2)
	{
		legend(x=x.C1[1]+2, y=1*ylim[2], legend = leg.C1, lty=1, pch=19, cex=.75, col="blue", lwd=2, bty="n")
		legend(x=x.C2[1]+2, y=.9*ylim[2], legend = leg.C2, lty=1, pch=24, cex=.75, col="red", lwd=2, bty="n")
		if(class(Case3)=="list"){legend(x=x.C3[1]+2, y=.8*ylim[2], legend = leg.C3, lty=1, pch=22, cex=.75, col="green", lwd=2, bty="n")}
		if(class(Case4)=="list"){legend(x=x.C4[1]+2, y=.7*ylim[2], legend = leg.C4, lty=2, pch=23, cex=.75, col="black", lwd=2, bty="n")}
		if(class(Case5)=="list"){legend(x=x.C5[1]+2, y=.6*ylim[2], legend = leg.C5, lty=3, pch=25, cex=.75, col="purple", lwd=2, bty="n")}
	}

	# Plot Labels
	title(xlab="Year-Ano", cex.lab=1.6, line=3)
	title(ylab="Summary biomass (t)", cex.lab=1.6, line=7)
	title(ylab="Biomasa sumaria (t)", cex.lab=1.6, line=5)
 
    #abline(h=0,col="grey")
}
#plotBioSmryXcases(Case1=BaseRep, Case2=Retro2008Rep, Case3=Retro2007Rep, Case4=Retro2006Rep, Case5=Retro2005Rep, Legends=c("2009","2008","2007","2006","2005"), year1=1975, convertTon=1, retro=1, EndYrCases=seq(136,120,-4), legendOption=1, xlim=c(1970, 2015))


####################################################################################
### 	Time series (MULTIPLE CASES) - spawning biomass
####################################################################################


plotSpawnBioXcases <- function(Case1, Case2, Legends, year1, convertTon, forecast, uncertainty, xlim)
{	
	# Get quantities from replist
	startYr <- Case1$startyr
	endYr.C1 <- Case1$endyr
	endYr.C2 <- Case2$endyr

	numSeasons <- Case1$nseasons
	numAreas <- Case1$nareas
	numSexes <- Case1$nsexes
	DerivedQuants.C1 <- Case1$derived_quants		# Get derived quants for case1
	DerivedQuants.C2 <- Case2$derived_quants		# Get derived quants for case2
    bioScale <- 1	# Scaling factor for single sex models
    if(numSexes==1) bioScale <- 0.5
    
    ##############
    #   CASE 1 
    ##############
   
    # Make dataframe with SpanBio, years and 95CI for all eras - CASE 1
    SpawnBioSD.C1 <- matchfun2("SPB_Virgin",0,"Recr_Virgin",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants.C1,objsubset=DerivedQuants.C1,substr1=TRUE,substr2=TRUE)
    SpawnBioSD.C1$Yr <- substring(SpawnBioSD.C1$Label,5,nchar(SpawnBioSD.C1$Label[1])-1)
    SpawnBioSD.C1$Yr[2] <- as.numeric(SpawnBioSD.C1$Yr[3])-1
    SpawnBioSD.C1$Yr[1] <- as.numeric(SpawnBioSD.C1$Yr[2])-1
    SpawnBioSD.C1$Yr <- as.numeric(SpawnBioSD.C1$Yr)
    SpawnBio.C1 <- SpawnBioSD.C1$Value*bioScale								# Scale for biosex (if nsexes = 1, divide SB by 2)
    # Make columns with 95%CI
    SpawnBioSD.C1$upper <- SpawnBio.C1 + 1.96*SpawnBioSD.C1$StdDev*bioScale
    SpawnBioSD.C1$lower <- SpawnBio.C1 - 1.96*SpawnBioSD.C1$StdDev*bioScale
    SpawnBioSD.C1$lower[SpawnBioSD.C1$lower < 0] <- 0							# If <0 , make it 0
    # Convert to tons
    if(convertTon==1)
    {
	    SpawnBio.C1 <- SpawnBio.C1/1000
	    SpawnBioSD.C1$upper <- SpawnBioSD.C1$upper/1000
	    SpawnBioSD.C1$lower <- SpawnBioSD.C1$lower/1000
    }
    # Convert from quarters (EPO models) to year values
	SpawnBioSD.C1$Yr2 <- SpawnBioSD.C1$Yr 
    SpawnBioSD.C1$Yr2[3:length(SpawnBioSD.C1$Yr)] <- year1 + (SpawnBioSD.C1$Yr[3:length(SpawnBioSD.C1$Yr)]/4)-0.25
        
    ##############
    #   CASE 2 
    ##############
   
    # Make dataframe with SpanBio, years and 95CI for all eras - CASE 1
    SpawnBioSD.C2 <- matchfun2("SPB_Virgin",0,"Recr_Virgin",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants.C2,objsubset=DerivedQuants.C2,substr1=TRUE,substr2=TRUE)
    SpawnBioSD.C2$Yr <- substring(SpawnBioSD.C2$Label,5,nchar(SpawnBioSD.C2$Label[1])-1)
    SpawnBioSD.C2$Yr[2] <- as.numeric(SpawnBioSD.C2$Yr[3])-1
    SpawnBioSD.C2$Yr[1] <- as.numeric(SpawnBioSD.C2$Yr[2])-1
    SpawnBioSD.C2$Yr <- as.numeric(SpawnBioSD.C2$Yr)
    SpawnBio.C2 <- SpawnBioSD.C2$Value*bioScale								# Scale for biosex (if nsexes = 1, divide SB by 2)
    # Make columns with 95%CI
    SpawnBioSD.C2$upper <- SpawnBio.C2 + 1.96*SpawnBioSD.C2$StdDev*bioScale
    SpawnBioSD.C2$lower <- SpawnBio.C2 - 1.96*SpawnBioSD.C2$StdDev*bioScale
    SpawnBioSD.C2$lower[SpawnBioSD.C2$lower < 0] <- 0							# If <0 , make it 0
    # Convert to tons
    if(convertTon==1)
    {
	    SpawnBio.C2 <- SpawnBio.C2/1000
	    SpawnBioSD.C2$upper <- SpawnBioSD.C2$upper/1000
	    SpawnBioSD.C2$lower <- SpawnBioSD.C2$lower/1000
    }
    # Convert from quarters (EPO models) to year values
	SpawnBioSD.C2$Yr2 <- SpawnBioSD.C2$Yr 
    SpawnBioSD.C2$Yr2[3:length(SpawnBioSD.C2$Yr)] <- year1 + (SpawnBioSD.C2$Yr[3:length(SpawnBioSD.C2$Yr)]/4)-0.25

    
    # Do not plot FORECAST
    if(forecast==F)
    {
	    # Prepare quantities for plot - CASE 1
	    Yrs <- SpawnBioSD.C1$Yr[SpawnBioSD.C1$Yr<=(endYr.C1+1)]
	    SpawnBioVals.C1 <- SpawnBio.C1[SpawnBioSD.C1$Yr<=(endYr.C1+1)]
	    SpawnBioCIup.C1 <- SpawnBioSD.C1$upper[SpawnBioSD.C1$Yr<=(endYr.C1+1)]
	    SpawnBioCIlo.C1 <- SpawnBioSD.C1$lower[SpawnBioSD.C1$Yr<=(endYr.C1+1)]
	    x.C1 <- SpawnBioSD.C1$Yr2[SpawnBioSD.C1$Yr%in%Yrs[3:length(Yrs)]]
	    y.C1 <- SpawnBioVals.C1[3:length(SpawnBioVals.C1)]
	    
	    # Prepare quantities for plot - CASE 2
	    Yrs <- SpawnBioSD.C2$Yr[SpawnBioSD.C2$Yr<=(endYr.C2+1)]
	    SpawnBioVals.C2 <- SpawnBio.C2[SpawnBioSD.C2$Yr<=(endYr.C2+1)]
	    SpawnBioCIup.C2 <- SpawnBioSD.C2$upper[SpawnBioSD.C2$Yr<=(endYr.C2+1)]
	    SpawnBioCIlo.C2 <- SpawnBioSD.C2$lower[SpawnBioSD.C2$Yr<=(endYr.C2+1)]
	    x.C2 <- SpawnBioSD.C2$Yr2[SpawnBioSD.C2$Yr%in%Yrs[3:length(Yrs)]]
	    y.C2 <- SpawnBioVals.C2[3:length(SpawnBioVals.C2)]

	    # Do not plot 95CI
	    if(uncertainty==F)
	    {
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,8,2,1))
		    #xlim <- range(pretty(x.C1,x.C2))
		    if(length(xlim)==1){xlim <- range(pretty(x.C1,x.C2))}
		    if(length(xlim)==2){xlim <- xlim}

		    ylim <- range(0,max(y.C1,y.C2))
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    
		    # Plot CASE 1
		    plot(x.C1, y.C1, xlim=xlim, ylim=ylim, axes=T, type="l",col="blue", ylab="",xlab="", lwd=2,las=1)
		    # Plot yearly values
		    x2.C1 <- unique(floor(x.C1))
		    y2.C1 <- y.C1[x.C1%in%x2.C1]
		    points(x2.C1,y2.C1, pch=19, col="blue", cex=1)
		    
		    # Plot CASE 2
		    lines(x.C2, y.C2, lty=2, pch=21, col="red", lwd=2)
		   	# Plot yearly values
		    x2.C2 <- unique(floor(x.C2))
		    y2.C2 <- y.C2[x.C2%in%x2.C2]
		    points(x2.C2,y2.C2, pch=24, col="red", lwd=1.25, cex=1)

		    # Make legend
			leg.C1 <- Legends[1]
			leg.C2 <- Legends[2]
			# Plot legend
			legend(x=x.C1[1]+2, y=1*ylim[2], legend = leg.C1, lty=1, pch=19, cex=1, col="blue", lwd=2, bty="n")
			legend(x=x.C2[1]+2, y=.9*ylim[2], legend = leg.C2, lty=1, pch=24, cex=1, col="red", lwd=2, bty="n")
		    
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass (t)", cex.lab=1.6, line=7)
		    title(ylab="Biomasa reproductora (t)", cex.lab=1.6, line=5)
	    }
	    
	    ## Plot 95CI
	    #if(uncertainty==T)
	    #{
		    ## Prepare CI quantities for plot
		    #yCIup <- SpawnBioCIup[3:length(SpawnBioCIup)]
		    #yCIlo <- SpawnBioCIlo[3:length(SpawnBioCIlo)]
	    
		    ## Make plot
		    #windows(8,5)
		    #par(mar=c(4,8,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(yCIup))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    ## Plot CIs
		    #lines(x,yCIup,col="black",lty="dashed", lwd=1.25)
		    #lines(x,yCIlo,col="black",lty="dashed", lwd=1.25)
		    ## Plot yearly values
		    #x2 <- unique(floor(x))
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    ## Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #title(ylab="Spawning biomass (t)", cex.lab=1.6, line=7)
		    #title(ylab="Biomasa reproductora (t)", cex.lab=1.6, line=5)
	    #}
    }
    
    ## Plot FORECAST
    #if(forecast==T)
    #{
	    ## Prepare quantities for plot
	    #Yrs <- SpawnBioSD$Yr
	    #SpawnBioVals <- SpawnBio
	    #SpawnBioCIup <- SpawnBioSD$upper
	    #SpawnBioCIlo <- SpawnBioSD$lower
	    #x <- SpawnBioSD$Yr2[SpawnBioSD$Yr%in%Yrs[3:length(Yrs)]]
	    #y <- SpawnBioVals[3:length(SpawnBioVals)]
	    #xLast <- SpawnBioSD$Yr2[SpawnBioSD$Yr==(endYr+1)] # Get last year of historical Era 
	    #yLast <- SpawnBio[SpawnBioSD$Yr==(endYr+1)] # Get SpawnBio last in last year of historical Era

	    ## Do not plot 95CI
	    #if(uncertainty==F)
	    #{
		    ## Make plot
		    #windows(8,5)
		    #par(mar=c(4,8,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(y))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    ## Plot yearly values
		    #x2 <- unique(floor(x))
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(xLast,yLast, pch=19, col="black", cex=1.5)
		    ## Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #title(ylab="Spawning biomass (t)", cex.lab=1.6, line=7)
		    #title(ylab="Biomasa reproductora (t)", cex.lab=1.6, line=5)
	    #}
	    ## Plot 95CI
	    #if(uncertainty==T)
	    #{
		    ## Prepare CI quantities for plot
		    #yCIup <- SpawnBioCIup[3:length(SpawnBioCIup)]
		    #yCIlo <- SpawnBioCIlo[3:length(SpawnBioCIlo)]

		    ## Make plot
		    #windows(8,5)
		    #par(mar=c(4,8,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(yCIup))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    ## Plot CIs
		    #lines(x,yCIup,col="black",lty="dashed", lwd=1.25)
		    #lines(x,yCIlo,col="black",lty="dashed", lwd=1.25)
		    ## Plot yearly values
		    #x2 <- unique(floor(x))
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(xLast,yLast, pch=19, col="black", cex=1.5)
		    ## Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #title(ylab="Spawning biomass (t)", cex.lab=1.6, line=7)
		    #title(ylab="Biomasa reproductora (t)", cex.lab=1.6, line=5)
	    #}
    #}
}
#plotSpawnBioXcases(Case1=BaseRep, Case2=BaseRep08, Legends=c("Base case - Caso base", "h = 0.75"), year1=1975, convertTon=1, forecast=F, uncertainty=F, xlim=c(1875,2015))
   

####################################################################################
### 	Time series (MULTIPLE CASES) - depletion  (SBR)
####################################################################################

plotSBRXcases <- function(Case1, Case2, Case3, Case4, Case5, Path1, Path2, Path3, Path4, Path5, Legends, year1, convertTon, forecast, uncertainty, retro, EndYrCases, legendOption, xlim)
{
    # Get quantities from Case1
	startYr <- Case1$startyr
	numSeasons <- Case1$nseasons
	numAreas <- Case1$nareas
	numSexes <- Case1$nsexes
	bioScale <- 1 	# Scaling factor for single sex models
    if(numSexes==1) bioScale <- 0.5
	DerivedQuants.C1 <- Case1$derived_quants		# Get derived quants for case1
	DerivedQuants.C2 <- Case2$derived_quants		# Get derived quants for case2
	if(class(Case3)=="list"){DerivedQuants.C3 <- Case3$derived_quants}
    if(class(Case4)=="list"){DerivedQuants.C4 <- Case4$derived_quants}
    if(class(Case5)=="list"){DerivedQuants.C5 <- Case5$derived_quants}
    
	# Define endYr
	# Standard analysis
	if(retro!=1)
	{
		endYr.C1 <- Case1$endyr
		endYr.C2 <- Case2$endyr
		if(class(Case3)=="list"){endYr.C3 <- Case3$endyr}
		if(class(Case4)=="list"){endYr.C4 <- Case4$endyr}
		if(class(Case5)=="list"){endYr.C5 <- Case5$endyr}
	}
	# Retrospective analysis
	if(retro==1)
	{
		endYr.C1 <- EndYrCases[1]
		endYr.C2 <- EndYrCases[2]
		endYr.C3 <- EndYrCases[3]
		endYr.C4 <- EndYrCases[4]
		endYr.C5 <- EndYrCases[5]
	}
	    
    ##############
    #   CASE 1 
    ##############

    # Make dataframe with SpanBio, years and 95CI for all eras
    SBRsd.C1 <- DerivedQuants.C1
    Labels <- substring(DerivedQuants.C1$Label,1,6)    
    SBRsd.C1 <- SBRsd.C1[Labels==c("Bratio"),]    
    SBRsd.C1$Yr <- substring(SBRsd.C1$Label,8,max(nchar(SBRsd.C1$Label)))
    SBRsd.C1$Yr <- as.numeric(SBRsd.C1$Yr)		# Make column with years
    SBRvals.C1 <- SBRsd.C1$Value*bioScale		# Scale for biosex (if nsexes = 1, divide SB by 2)
    # Make columns with 95%CI
    SBRsd.C1$upper <- SBRvals.C1 + 1.96*SBRsd.C1$StdDev*bioScale
    SBRsd.C1$lower <- SBRvals.C1 - 1.96*SBRsd.C1$StdDev*bioScale
    SBRsd.C1$lower[SBRsd.C1$lower < 0] <- 0		# If <0 , make it 0
    # Convert from quarters (EPO models) to year values
	SBRsd.C1$Yr2 <- SBRsd.C1$Yr 
    SBRsd.C1$Yr2 <- year1 + (SBRsd.C1$Yr/4)-0.25

    # Make forecast management report name
	ForeRepName.C1 <- paste(Path1, "Forecast-report.SSO" ,sep="")
	# Get management report
	ForeRepStart.C1 <- grep("Management_report", readLines(ForeRepName.C1))
	ForeRepEnd.C1 <- grep("#_note when there is time-varying biology", readLines(ForeRepName.C1))[1]
	ForeDat.C1 <- read.table(file=ForeRepName.C1,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=ForeRepEnd.C1-ForeRepStart.C1, skip = ForeRepStart.C1-1)
	ForeDat.C1 <- as.data.frame(ForeDat.C1)
	# Get Smsy/S0
	SmsySzero.C1 <- as.numeric(ForeDat.C1[ForeDat.C1[,1]==c("SPBmsy/SPBzero(using_SPB_virgin)"),2])

		
	##############
    #   CASE 2 
    ##############

    # Make dataframe with SpanBio, years and 95CI for all eras
    SBRsd.C2 <- DerivedQuants.C2
    Labels <- substring(DerivedQuants.C2$Label,1,6)    
    SBRsd.C2 <- SBRsd.C2[Labels==c("Bratio"),]    
    SBRsd.C2$Yr <- substring(SBRsd.C2$Label,8,max(nchar(SBRsd.C2$Label)))
    SBRsd.C2$Yr <- as.numeric(SBRsd.C2$Yr)		# Make column with years
    SBRvals.C2 <- SBRsd.C2$Value*bioScale		# Scale for biosex (if nsexes = 1, divide SB by 2)
    # Make columns with 95%CI
    SBRsd.C2$upper <- SBRvals.C2 + 1.96*SBRsd.C2$StdDev*bioScale
    SBRsd.C2$lower <- SBRvals.C2 - 1.96*SBRsd.C2$StdDev*bioScale
    SBRsd.C2$lower[SBRsd.C2$lower < 0] <- 0		# If <0 , make it 0
    # Convert from quarters (EPO models) to year values
	SBRsd.C2$Yr2 <- SBRsd.C2$Yr 
    SBRsd.C2$Yr2 <- year1 + (SBRsd.C2$Yr/4)-0.25

	# Make forecast management report name
	ForeRepName.C2 <- paste(Path2, "Forecast-report.SSO" ,sep="")
	# Get management report
	ForeRepStart.C2 <- grep("Management_report", readLines(ForeRepName.C2))
	ForeRepEnd.C2 <- grep("#_note when there is time-varying biology", readLines(ForeRepName.C2))[1]
	ForeDat.C2 <- read.table(file=ForeRepName.C2,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=ForeRepEnd.C2-ForeRepStart.C2, skip = ForeRepStart.C2-1)
	ForeDat.C2 <- as.data.frame(ForeDat.C2)
	# Get Smsy/S0
	SmsySzero.C2 <- as.numeric(ForeDat.C2[ForeDat.C2[,1]==c("SPBmsy/SPBzero(using_SPB_virgin)"),2])

	
	##############
    #   CASE 3 
    ##############
    
    if(class(Case3)=="list")
    {
	    # Make dataframe with SpanBio, years and 95CI for all eras
	    SBRsd.C3 <- DerivedQuants.C3
	    Labels <- substring(DerivedQuants.C3$Label,1,6)
	    SBRsd.C3 <- SBRsd.C3[Labels==c("Bratio"),]
	    SBRsd.C3$Yr <- substring(SBRsd.C3$Label,8,max(nchar(SBRsd.C3$Label)))
	    SBRsd.C3$Yr <- as.numeric(SBRsd.C3$Yr)		# Make column with years
	    SBRvals.C3 <- SBRsd.C3$Value*bioScale		# Scale for biosex (if nsexes = 1, divide SB by 2)
	    # Make columns with 95%CI
	    SBRsd.C3$upper <- SBRvals.C3 + 1.96*SBRsd.C3$StdDev*bioScale
	    SBRsd.C3$lower <- SBRvals.C3 - 1.96*SBRsd.C3$StdDev*bioScale
	    SBRsd.C3$lower[SBRsd.C3$lower < 0] <- 0		# If <0 , make it 0
	    # Convert from quarters (EPO models) to year values
	    SBRsd.C3$Yr2 <- SBRsd.C3$Yr
	    SBRsd.C3$Yr2 <- year1 + (SBRsd.C3$Yr/4)-0.25
	    
	    # Make forecast management report name
	    ForeRepName.C3 <- paste(Path3, "Forecast-report.SSO" ,sep="")
	    # Get management report
	    ForeRepStart.C3 <- grep("Management_report", readLines(ForeRepName.C3))
	    ForeRepEnd.C3 <- grep("#_note when there is time-varying biology", readLines(ForeRepName.C3))[1]
	    ForeDat.C3 <- read.table(file=ForeRepName.C3,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=ForeRepEnd.C3-ForeRepStart.C3, skip = ForeRepStart.C3-1)
	    ForeDat.C3 <- as.data.frame(ForeDat.C3)
	    # Get Smsy/S0
	    SmsySzero.C3 <- as.numeric(ForeDat.C3[ForeDat.C3[,1]==c("SPBmsy/SPBzero(using_SPB_virgin)"),2])

    }

	##############
    #   CASE 4 
    ##############

    if(class(Case4)=="list")
    {
	    # Make dataframe with SpanBio, years and 95CI for all eras
	    SBRsd.C4 <- DerivedQuants.C4
	    Labels <- substring(DerivedQuants.C4$Label,1,6)
	    SBRsd.C4 <- SBRsd.C4[Labels==c("Bratio"),]
	    SBRsd.C4$Yr <- substring(SBRsd.C4$Label,8,max(nchar(SBRsd.C4$Label)))
	    SBRsd.C4$Yr <- as.numeric(SBRsd.C4$Yr)		# Make column with years
	    SBRvals.C4 <- SBRsd.C4$Value*bioScale		# Scale for biosex (if nsexes = 1, divide SB by 2)
	    # Make columns with 95%CI
	    SBRsd.C4$upper <- SBRvals.C4 + 1.96*SBRsd.C4$StdDev*bioScale
	    SBRsd.C4$lower <- SBRvals.C4 - 1.96*SBRsd.C4$StdDev*bioScale
	    SBRsd.C4$lower[SBRsd.C4$lower < 0] <- 0		# If <0 , make it 0
	    # Convert from quarters (EPO models) to year values
	    SBRsd.C4$Yr2 <- SBRsd.C4$Yr
	    SBRsd.C4$Yr2 <- year1 + (SBRsd.C4$Yr/4)-0.25
	    
	    # Make forecast management report name
	    ForeRepName.C4 <- paste(Path4, "Forecast-report.SSO" ,sep="")
	    # Get management report
	    ForeRepStart.C4 <- grep("Management_report", readLines(ForeRepName.C4))
	    ForeRepEnd.C4 <- grep("#_note when there is time-varying biology", readLines(ForeRepName.C4))[1]
	    ForeDat.C4 <- read.table(file=ForeRepName.C4,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=ForeRepEnd.C4-ForeRepStart.C4, skip = ForeRepStart.C4-1)
	    ForeDat.C4 <- as.data.frame(ForeDat.C4)
	    # Get Smsy/S0
	    SmsySzero.C4 <- as.numeric(ForeDat.C4[ForeDat.C4[,1]==c("SPBmsy/SPBzero(using_SPB_virgin)"),2])
	    
    }

	##############
    #   CASE 5 
    ##############
    
    if(class(Case5)=="list")
    {
	    # Make dataframe with SpanBio, years and 95CI for all eras
	    SBRsd.C5 <- DerivedQuants.C5
	    Labels <- substring(DerivedQuants.C5$Label,1,6)
	    SBRsd.C5 <- SBRsd.C5[Labels==c("Bratio"),]
	    SBRsd.C5$Yr <- substring(SBRsd.C5$Label,8,max(nchar(SBRsd.C5$Label)))
	    SBRsd.C5$Yr <- as.numeric(SBRsd.C5$Yr)		# Make column with years
	    SBRvals.C5 <- SBRsd.C5$Value*bioScale		# Scale for biosex (if nsexes = 1, divide SB by 2)
	    # Make columns with 95%CI
	    SBRsd.C5$upper <- SBRvals.C5 + 1.96*SBRsd.C5$StdDev*bioScale
	    SBRsd.C5$lower <- SBRvals.C5 - 1.96*SBRsd.C5$StdDev*bioScale
	    SBRsd.C5$lower[SBRsd.C5$lower < 0] <- 0		# If <0 , make it 0
	    # Convert from quarters (EPO models) to year values
	    SBRsd.C5$Yr2 <- SBRsd.C5$Yr
	    SBRsd.C5$Yr2 <- year1 + (SBRsd.C5$Yr/4)-0.25
	    
	    # Make forecast management report name
	    ForeRepName.C5 <- paste(Path5, "Forecast-report.SSO" ,sep="")
	    # Get management report
	    ForeRepStart.C5 <- grep("Management_report", readLines(ForeRepName.C5))
	    ForeRepEnd.C5 <- grep("#_note when there is time-varying biology", readLines(ForeRepName.C5))[1]
	    ForeDat.C5 <- read.table(file=ForeRepName.C5,col.names=c(seq(1,10,by=1)),fill=T,quote="",colClasses="character", nrows=ForeRepEnd.C5-ForeRepStart.C5, skip = ForeRepStart.C5-1)
	    ForeDat.C5 <- as.data.frame(ForeDat.C5)
	    # Get Smsy/S0
	    SmsySzero.C5 <- as.numeric(ForeDat.C5[ForeDat.C5[,1]==c("SPBmsy/SPBzero(using_SPB_virgin)"),2])
    }


	#################
	## PLOT SECTION
	#################
	
    # Do not plot FORECAST
    if(forecast==F)
    {
	   	# Prepare quantities for plot - CASE 1
	    Yrs.C1 <- SBRsd.C1$Yr[SBRsd.C1$Yr<=(endYr.C1+1)]
	    SBRvals.C1 <- SBRsd.C1$Value[SBRsd.C1$Yr<=(endYr.C1+1)]
	    #SBR.CIup.C1 <- SBRsd.C1$upper[SBRsd.C1$Yr<=(endYr.C1+1)]
	    #SBR.CIlo.C1 <- SBRsd.C1$lower[SBRsd.C1$Yr<=(endYr.C1+1)]
	    x.C1 <- SBRsd.C1$Yr2[SBRsd.C1$Yr%in%Yrs.C1[1:length(Yrs.C1)]]
	    y.C1 <- SBRvals.C1[1:length(SBRvals.C1)]
	    
	   	# Prepare quantities for plot - CASE 2
	    Yrs.C2 <- SBRsd.C2$Yr[SBRsd.C2$Yr<=(endYr.C2+1)]
	    SBRvals.C2 <- SBRsd.C2$Value[SBRsd.C2$Yr<=(endYr.C2+1)]
	    #SBR.CIup.C2 <- SBRsd.C2$upper[SBRsd.C2$Yr<=(endYr.C2+1)]
	    #SBR.CIlo.C2 <- SBRsd.C2$lower[SBRsd.C2$Yr<=(endYr.C2+1)]
	    x.C2 <- SBRsd.C2$Yr2[SBRsd.C2$Yr%in%Yrs.C2[1:length(Yrs.C2)]]
	    y.C2 <- SBRvals.C2[1:length(SBRvals.C2)]
	    
	    if(class(Case3)=="list")
	    {
		    # Prepare quantities for plot - CASE 3
		    Yrs.C3 <- SBRsd.C3$Yr[SBRsd.C3$Yr<=(endYr.C3+1)]
		    SBRvals.C3 <- SBRsd.C3$Value[SBRsd.C3$Yr<=(endYr.C3+1)]
		    #SBR.CIup.C3 <- SBRsd.C3$upper[SBRsd.C3$Yr<=(endYr.C3+1)]
		    #SBR.CIlo.C3 <- SBRsd.C3$lower[SBRsd.C3$Yr<=(endYr.C3+1)]
		    x.C3 <- SBRsd.C3$Yr2[SBRsd.C3$Yr%in%Yrs.C3[1:length(Yrs.C3)]]
		    y.C3 <- SBRvals.C3[1:length(SBRvals.C3)]
	    }

	    if(class(Case4)=="list")
	    {
		    # Prepare quantities for plot - CASE 4
		    Yrs.C4 <- SBRsd.C4$Yr[SBRsd.C4$Yr<=(endYr.C4+1)]
		    SBRvals.C4 <- SBRsd.C4$Value[SBRsd.C4$Yr<=(endYr.C4+1)]
		    #SBR.CIup.C4 <- SBRsd.C4$upper[SBRsd.C4$Yr<=(endYr.C4+1)]
		    #SBR.CIlo.C4 <- SBRsd.C4$lower[SBRsd.C4$Yr<=(endYr.C4+1)]
		    x.C4 <- SBRsd.C4$Yr2[SBRsd.C4$Yr%in%Yrs.C4[1:length(Yrs.C4)]]
		    y.C4 <- SBRvals.C4[1:length(SBRvals.C4)]
	    }

	    if(class(Case5)=="list")
	    {
		    # Prepare quantities for plot - CASE 5
		    Yrs.C5 <- SBRsd.C5$Yr[SBRsd.C5$Yr<=(endYr.C5+1)]
		    SBRvals.C5 <- SBRsd.C5$Value[SBRsd.C5$Yr<=(endYr.C5+1)]
		    #SBR.CIup.C5 <- SBRsd.C5$upper[SBRsd.C5$Yr<=(endYr.C5+1)]
		    #SBR.CIlo.C5 <- SBRsd.C5$lower[SBRsd.C5$Yr<=(endYr.C5+1)]
		    x.C5 <- SBRsd.C5$Yr2[SBRsd.C5$Yr%in%Yrs.C5[1:length(Yrs.C5)]]
		    y.C5 <- SBRvals.C5[1:length(SBRvals.C5)]
	    }

	    # Do not plot 95CI
	    if(uncertainty==F)
	    {		    
	    	# Make plot
		    windows(8,5)
		    par(mar=c(4,8,2,1))
		    #xlim <- range(pretty(x.C1,x.C2))
		    if(length(xlim)==1){xlim <- range(pretty(x.C1,x.C2))}
		    if(length(xlim)==2){xlim <- xlim}

		    ylim <- range(0,max(y.C1,y.C2))
		    if(class(Case3)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3))}
		    if(class(Case4)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3,y.C4))}
		    if(class(Case5)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3,y.C4,y.C5))}
		    options(scipen=1) 				# Do not use scientific notation in plotting
		    
		    # Plot CASE 1
		    plot(x.C1, y.C1, xlim=xlim, ylim=ylim, axes=T, type="l",col="blue", ylab="",xlab="", lwd=2,las=1)
		    # Plot yearly values
		    x2.C1 <- unique(floor(x.C1))
		    x2.C1 <- x2.C1[2:length(x2.C1)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    y2.C1 <- y.C1[x.C1%in%x2.C1]
		    points(x2.C1,y2.C1, pch=19, col="blue", cex=1)
			lines(xlim,c(SmsySzero.C1,SmsySzero.C1), lty="solid", lwd=2, col="blue")		# Plot horizontal ref line for SmsySzero

			# Plot CASE 2
		    lines(x.C2, y.C2, lty=2, pch=21, col="red", lwd=2)
		   	# Plot yearly values
		    x2.C2 <- unique(floor(x.C2))
		    x2.C2 <- x2.C2[2:length(x2.C2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    y2.C2 <- y.C2[x.C2%in%x2.C2]
		    points(x2.C2,y2.C2, pch=24, col="red", lwd=1.25, cex=1)
		    lines(xlim,c(SmsySzero.C2,SmsySzero.C2), lty="dashed", lwd=2,col="red")		# Plot horizontal ref line for SmsySzero

			# Plot CASE 3
			if(class(Case3)=="list")
		    {
			    lines(x.C3, y.C3, lty=1, pch=21, col="green", lwd=2)
			    # Plot yearly values
			    x2.C3 <- unique(floor(x.C3))
			    x2.C3 <- x2.C3[2:length(x2.C3)]		# the first year is not showing up in the derived quantities! FIX LATER...
			    y2.C3 <- y.C3[x.C3%in%x2.C3]
			    points(x2.C3,y2.C3, pch=22, col="green", lwd=1.25, cex=1)
			    lines(xlim,c(SmsySzero.C3,SmsySzero.C3), lty="solid", lwd=2,col="green")		# Plot horizontal ref line for SmsySzero
		    }
		    
		    # Plot CASE 4
		    if(class(Case4)=="list")
		    {
			    lines(x.C4, y.C4, lty=2, pch=21, col="black", lwd=2)
			    # Plot yearly values
			    x2.C4 <- unique(floor(x.C4))
			    x2.C4 <- x2.C4[2:length(x2.C4)]		# the first year is not showing up in the derived quantities! FIX LATER...
			    y2.C4 <- y.C4[x.C4%in%x2.C4]
			    points(x2.C4,y2.C4, pch=23, col="black", lwd=1.25, cex=1)
			    lines(xlim,c(SmsySzero.C4,SmsySzero.C4), lty="dashed", lwd=2,col="black")		# Plot horizontal ref line for SmsySzero
		    }

		    # Plot CASE 5
		    if(class(Case5)=="list")
		    {
			    lines(x.C5, y.C5, lty=3, pch=21, col="purple", lwd=2)
			    # Plot yearly values
			    x2.C5 <- unique(floor(x.C5))
			    x2.C5 <- x2.C5[2:length(x2.C5)]		# the first year is not showing up in the derived quantities! FIX LATER...
			    y2.C5 <- y.C5[x.C5%in%x2.C5]
			    points(x2.C5,y2.C5, pch=25, col="purple", lwd=1.25, cex=1)
			    lines(xlim,c(SmsySzero.C5,SmsySzero.C5), lty=3, lwd=2,col="purple")		# Plot horizontal ref line for SmsySzero
		    }

		    # Make legend
		    leg.C1 <- Legends[1]
		    leg.C2 <- Legends[2]
		    if(class(Case3)=="list"){leg.C3 <- Legends[3]}
		    if(class(Case4)=="list"){leg.C4 <- Legends[4]}
		    if(class(Case5)=="list"){leg.C5 <- Legends[5]}
		    
		    # Plot legend
		    if(legendOption==0)
		    {
			    legend(x=x.C1[1]+2, y=.3*ylim[2], legend = leg.C1, lty=1, pch=19, cex=1, col="blue", lwd=2, bty="n")
			    legend(x=x.C2[1]+2, y=.2*ylim[2], legend = leg.C2, lty=1, pch=24, cex=1, col="red", lwd=2, bty="n")
			    if(class(Case3)=="list"){legend(x=x.C3[1]+2, y=.1*ylim[2], legend = leg.C3, lty=1, pch=22, cex=1, col="green", lwd=2, bty="n")}
			    if(class(Case4)=="list"){legend(x=x.C4[1]+15, y=.3*ylim[2], legend = leg.C4, lty=2, pch=23, cex=1, col="black", lwd=2, bty="n")}
			    if(class(Case5)=="list"){legend(x=x.C5[1]+15, y=.2*ylim[2], legend = leg.C5, lty=3, pch=25, cex=1, col="purple", lwd=2, bty="n")}
		    }
		    
		    if(legendOption==1)
		    {
			    legend(x=x.C1[1]+20, y=1*ylim[2], legend = leg.C1, lty=1, pch=19, cex=.75, col="blue", lwd=2, bty="n")
			    legend(x=x.C2[1]+20, y=.9*ylim[2], legend = leg.C2, lty=1, pch=24, cex=.75, col="red", lwd=2, bty="n")
			    if(class(Case3)=="list"){legend(x=x.C3[1]+20, y=.8*ylim[2], legend = leg.C3, lty=1, pch=22, cex=.75, col="green", lwd=2, bty="n")}
			    if(class(Case4)=="list"){legend(x=x.C4[1]+20, y=.7*ylim[2], legend = leg.C4, lty=2, pch=23, cex=.75, col="black", lwd=2, bty="n")}
			    if(class(Case5)=="list"){legend(x=x.C5[1]+20, y=.6*ylim[2], legend = leg.C5, lty=3, pch=25, cex=.75, col="purple", lwd=2, bty="n")}
		    }

		    if(legendOption==2)
		    {
			    legend(x=x.C1[1]+2, y=1*ylim[2], legend = leg.C1, lty=1, pch=19, cex=.75, col="blue", lwd=2, bty="n")
			    legend(x=x.C2[1]+2, y=.9*ylim[2], legend = leg.C2, lty=1, pch=24, cex=.75, col="red", lwd=2, bty="n")
			    if(class(Case3)=="list"){legend(x=x.C3[1]+2, y=.8*ylim[2], legend = leg.C3, lty=1, pch=22, cex=.75, col="green", lwd=2, bty="n")}
			    if(class(Case4)=="list"){legend(x=x.C4[1]+2, y=.7*ylim[2], legend = leg.C4, lty=2, pch=23, cex=.75, col="black", lwd=2, bty="n")}
			    if(class(Case5)=="list"){legend(x=x.C5[1]+2, y=.6*ylim[2], legend = leg.C5, lty=3, pch=25, cex=.75, col="purple", lwd=2, bty="n")}
		    }

		    
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    title(ylab="Spawning biomass ratio", cex.lab=1.6, line=5)
		    title(ylab="Cociente de biomasa reproductora", cex.lab=1.6, line=3)
	    }
	    	    
	    # Plot 95CI
	    #if(uncertainty==T)
	    #{
		    ## Prepare CI quantities for plot
		    #yCIup <- SBRsd$upper[1:length(SBR.CIup)]
		    #yCIlo <- SBRsd$lower[1:length(SBR.CIlo)]
	    
		    # Make plot
		    #windows(8,5)
		    #par(mar=c(4,6,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(yCIup))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    ## Plot CIs
		    #lines(x,yCIup,col="black",lty="dashed", lwd=1.25)
		    #lines(x,yCIlo,col="black",lty="dashed", lwd=1.25)
		    ## Plot yearly values
		    #x2 <- unique(floor(x))
		    #x2 <- x2[2:length(x2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
			#lines(xlim,c(SmsySzero,SmsySzero), lty="dashed", lwd=1.25)		# Plot horizontal ref line for SmsySzero

		    # Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #title(ylab="Spawning biomass ratio", cex.lab=1.6, line=5)
		    #title(ylab="Cociente de biomasa reproductora", cex.lab=1.6, line=3)
	    #}
    }
        
    # Plot FORECAST
    #if(forecast==T)
    #{
	    ## Prepare quantities for plot
		#Yrs <- SBRsd$Yr
		#SBRvals <- SBRsd$Value
		#SBR.CIup <- SBRsd$upper
		#SBR.CIlo <- SBRsd$lower
		#x <- SBRsd$Yr2[SBRsd$Yr%in%Yrs[1:length(Yrs)]]		    
		#y <- SBRvals[1:length(SBRvals)]
		#xLast <- SBRsd$Yr2[SBRsd$Yr==(endYr+1)] 	# Get last year of historical Era 
    	#yLast <- SBRvals[SBRsd$Yr==(endYr+1)] 	# Get SpawnBio last in last year of historical Era
    	    	
	    ## Do not plot 95CI
	    #if(uncertainty==F)
	    #{
		    ## Make plot
		    #windows(8,5)
		    #par(mar=c(4,6,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(y))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    ## Plot yearly values
		    #x2 <- unique(floor(x))
		    #x2 <- x2[2:length(x2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(xLast,yLast, pch=19, col="black", cex=1.5)
			#lines(xlim,c(SmsySzero,SmsySzero), lty="dashed", lwd=1.25)		# Plot horizontal ref line for SmsySzero

		    # Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #title(ylab="Spawning biomass ratio", cex.lab=1.6, line=5)
		    #title(ylab="Cociente de biomasa reproductora", cex.lab=1.6, line=3)
	    #}
	    # Plot 95CI
	    #if(uncertainty==T)
	    #{
		    ## Prepare CI quantities for plot
		    #yCIup <- SBRsd$upper[1:length(SBR.CIup)]
		    #yCIlo <- SBRsd$lower[1:length(SBR.CIlo)]
		        		
		    ## Make plot
		    #windows(8,5)
		    #par(mar=c(4,6,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(yCIup))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot CIs
		    #lines(x,yCIup,col="black",lty="dashed", lwd=1.25)
		    #lines(x,yCIlo,col="black",lty="dashed", lwd=1.25)
		    ## Plot yearly values
		    #x2 <- unique(floor(x))
		    #x2 <- x2[2:length(x2)]		# the first year is not showing up in the derived quantities! FIX LATER...
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(xLast,yLast, pch=19, col="black", cex=1.5)
			#lines(xlim,c(SmsySzero,SmsySzero), lty="dashed", lwd=1.25)		# Plot horizontal ref line for SmsySzero

		    ## Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #title(ylab="Spawning biomass ratio", cex.lab=1.6, line=5)
		    #title(ylab="Cociente de biomasa reproductora", cex.lab=1.6, line=3)
	    #}
    #}
}
#plotSBRXcases(Case1=BaseRep, Case2=Retro2008Rep, Case3=Retro2007Rep, Case4=Retro2006Rep, Case5=Retro2005Rep, Path1=BasePath, Path2=Retro2008Path, Path3=Retro2007Path, Path4=Retro2006Path, Path5=Retro2005Path, Legends=c("2009","2008","2007","2006","2005"),year1=1975, convertTon=1, forecast=F, uncertainty=F, retro=1, EndYrCases=seq(136,120,-4), legendOption=1, xlim=c(1975,2015))


####################################################################################
### 	Time series (MULTIPLE CASES) - RECRUITMENT
####################################################################################

plotRecXcases <- function(Case1, Case2, Case3, Case4, Case5, Legends, year1, recUnits, relRec, scalar, forecast, uncertainty, retro, EndYrCases, legendOption=1, xlim)
{
    # Get quantities from Case1
	startYr <- Case1$startyr
	numSeasons <- Case1$nseasons
	numAreas <- Case1$nareas
	numSexes <- Case1$nsexes
	bioScale <- 1 	# Scaling factor for single sex models
    if(numSexes==1) bioScale <- 0.5

	DerivedQuants.C1 <- Case1$derived_quants		# Get derived quants for case1
	DerivedQuants.C2 <- Case2$derived_quants		# Get derived quants for case2
	if(class(Case3)=="list"){DerivedQuants.C3 <- Case3$derived_quants}
    if(class(Case4)=="list"){DerivedQuants.C4 <- Case4$derived_quants}
    if(class(Case5)=="list"){DerivedQuants.C5 <- Case5$derived_quants}
    
	# Define endYr
	# Standard analysis
	if(retro!=1)
	{
		endYr.C1 <- Case1$endyr
		endYr.C2 <- Case2$endyr
		if(class(Case3)=="list"){endYr.C3 <- Case3$endyr}
		if(class(Case4)=="list"){endYr.C4 <- Case4$endyr}
		if(class(Case5)=="list"){endYr.C5 <- Case5$endyr}
	}
	# Retrospective analysis
	if(retro==1)
	{
		endYr.C1 <- EndYrCases[1]
		endYr.C2 <- EndYrCases[2]
		endYr.C3 <- EndYrCases[3]
		endYr.C4 <- EndYrCases[4]
		endYr.C5 <- EndYrCases[5]
	}
	        
    ##############
    #   CASE 1 
    ##############
    
    # Make dataframe with Rec, years and 95CI for all eras    
    RecSD.C1 <- matchfun2("Recr_Virgin",0,"SPRratio_1",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants.C1,objsubset=DerivedQuants.C1,substr1=TRUE,substr2=TRUE)
    RecSD.C1$Yr <- substring(RecSD.C1$Label,6,nchar(RecSD.C1$Label[1])-1)
    RecSD.C1$Yr[2] <- as.numeric(RecSD.C1$Yr[3])-1
    RecSD.C1$Yr[1] <- as.numeric(RecSD.C1$Yr[2])-1
    RecSD.C1$Yr <- as.numeric(RecSD.C1$Yr)
    # Make columns with 95%CI
    RecSD.C1$upper <- RecSD.C1$Value + 1.96*RecSD.C1$StdDev
    RecSD.C1$lower <- RecSD.C1$Value - 1.96*RecSD.C1$StdDev
    RecSD.C1$lower[RecSD.C1$lower < 0] <- 0		# If <0 , make it 0
    # Convert to desired units
    RecSD.C1$Value <- RecSD.C1$Value/recUnits
    RecSD.C1$upper <- RecSD.C1$upper/recUnits
    RecSD.C1$lower <- RecSD.C1$lower/recUnits
    # Convert from quarters (EPO models) to year values
	RecSD.C1$Yr2 <- RecSD.C1$Yr
	RecSD.C1$Yr2 <- year1 + (RecSD.C1$Yr/4)-0.25
	# Drop first two rows (recVir and recInit) from table (better for general plotting head)
	RecSD2.C1 <- RecSD.C1[3:dim(RecSD.C1)[1],]

    # Make dataframe with relative Rec, years and 95CI for all eras
    recVirg.C1 <- RecSD.C1$Value[1]
    recMean.C1 <- mean(RecSD2.C1$Value[RecSD2.C1$Yr<=(endYr.C1+1)])
	RelRecSD.C1 <- RecSD2.C1
	if(scalar==0){scalar.C1=recVirg.C1} 	# scale to virgin rec
	if(scalar==1){scalar.C1=recMean.C1}	# scale to mean rec
	RelRecSD.C1$Value <- RelRecSD.C1$Value/scalar.C1
	RelRecSD.C1$upper <- RelRecSD.C1$upper/scalar.C1
    RelRecSD.C1$lower <- RelRecSD.C1$lower/scalar.C1

    # Define recrutiment table for plot (absolute or relative recs)
	if(relRec==0){RecTable.C1 <- RecSD2.C1}
	if(relRec==1){RecTable.C1 <- RelRecSD.C1}

	##############
    #   CASE 2 
    ##############
    
    # Make dataframe with Rec, years and 95CI for all eras    
    RecSD.C2 <- matchfun2("Recr_Virgin",0,"SPRratio_1",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants.C2,objsubset=DerivedQuants.C2,substr1=TRUE,substr2=TRUE)
    RecSD.C2$Yr <- substring(RecSD.C2$Label,6,nchar(RecSD.C2$Label[1])-1)
    RecSD.C2$Yr[2] <- as.numeric(RecSD.C2$Yr[3])-1
    RecSD.C2$Yr[1] <- as.numeric(RecSD.C2$Yr[2])-1
    RecSD.C2$Yr <- as.numeric(RecSD.C2$Yr)
    # Make columns with 95%CI
    RecSD.C2$upper <- RecSD.C2$Value + 1.96*RecSD.C2$StdDev
    RecSD.C2$lower <- RecSD.C2$Value - 1.96*RecSD.C2$StdDev
    RecSD.C2$lower[RecSD.C2$lower < 0] <- 0		# If <0 , make it 0
    # Convert to desired units
    RecSD.C2$Value <- RecSD.C2$Value/recUnits
    RecSD.C2$upper <- RecSD.C2$upper/recUnits
    RecSD.C2$lower <- RecSD.C2$lower/recUnits
    # Convert from quarters (EPO models) to year values
	RecSD.C2$Yr2 <- RecSD.C2$Yr
	RecSD.C2$Yr2 <- year1 + (RecSD.C2$Yr/4)-0.25
	# Drop first two rows (recVir and recInit) from table (better for general plotting head)
	RecSD2.C2 <- RecSD.C2[3:dim(RecSD.C2)[1],]

    # Make dataframe with relative Rec, years and 95CI for all eras
    recVirg.C2 <- RecSD.C2$Value[1]
    recMean.C2 <- mean(RecSD2.C2$Value[RecSD2.C2$Yr<=(endYr.C2+1)])
	RelRecSD.C2 <- RecSD2.C2
	if(scalar==0){scalar.C2=recVirg.C2} 	# scale to virgin rec
	if(scalar==1){scalar.C2=recMean.C2}	# scale to mean rec
	RelRecSD.C2$Value <- RelRecSD.C2$Value/scalar.C2
	RelRecSD.C2$upper <- RelRecSD.C2$upper/scalar.C2
    RelRecSD.C2$lower <- RelRecSD.C2$lower/scalar.C2

    # Define recrutiment table for plot (absolute or relative recs)
	if(relRec==0){RecTable.C2 <- RecSD2.C2}
	if(relRec==1){RecTable.C2 <- RelRecSD.C2}

	
	##############
    #   CASE 3 
    ##############
    
    if(class(Case3)=="list")
    {
    # Make dataframe with Rec, years and 95CI for all eras    
    RecSD.C3 <- matchfun2("Recr_Virgin",0,"SPRratio_1",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants.C3,objsubset=DerivedQuants.C3,substr1=TRUE,substr2=TRUE)
    RecSD.C3$Yr <- substring(RecSD.C3$Label,6,nchar(RecSD.C3$Label[1])-1)
    RecSD.C3$Yr[2] <- as.numeric(RecSD.C3$Yr[3])-1
    RecSD.C3$Yr[1] <- as.numeric(RecSD.C3$Yr[2])-1
    RecSD.C3$Yr <- as.numeric(RecSD.C3$Yr)
    # Make columns with 95%CI
    RecSD.C3$upper <- RecSD.C3$Value + 1.96*RecSD.C3$StdDev
    RecSD.C3$lower <- RecSD.C3$Value - 1.96*RecSD.C3$StdDev
    RecSD.C3$lower[RecSD.C3$lower < 0] <- 0		# If <0 , make it 0
    # Convert to desired units
    RecSD.C3$Value <- RecSD.C3$Value/recUnits
    RecSD.C3$upper <- RecSD.C3$upper/recUnits
    RecSD.C3$lower <- RecSD.C3$lower/recUnits
    # Convert from quarters (EPO models) to year values
	RecSD.C3$Yr2 <- RecSD.C3$Yr
	RecSD.C3$Yr2 <- year1 + (RecSD.C3$Yr/4)-0.25
	# Drop first two rows (recVir and recInit) from table (better for general plotting head)
	RecSD2.C3 <- RecSD.C3[3:dim(RecSD.C3)[1],]

    # Make dataframe with relative Rec, years and 95CI for all eras
    recVirg.C3 <- RecSD.C3$Value[1]
    recMean.C3 <- mean(RecSD2.C3$Value[RecSD2.C3$Yr<=(endYr.C3+1)])
	RelRecSD.C3 <- RecSD2.C3
	if(scalar==0){scalar.C3=recVirg.C3} 	# scale to virgin rec
	if(scalar==1){scalar.C3=recMean.C3}	# scale to mean rec
	RelRecSD.C3$Value <- RelRecSD.C3$Value/scalar.C3
	RelRecSD.C3$upper <- RelRecSD.C3$upper/scalar.C3
    RelRecSD.C3$lower <- RelRecSD.C3$lower/scalar.C3

    # Define recrutiment table for plot (absolute or relative recs)
	if(relRec==0){RecTable.C3 <- RecSD2.C3}
	if(relRec==1){RecTable.C3 <- RelRecSD.C3}
	}

	##############
    #   CASE 4 
    ##############
    
    if(class(Case4)=="list")
    {
    # Make dataframe with Rec, years and 95CI for all eras    
    RecSD.C4 <- matchfun2("Recr_Virgin",0,"SPRratio_1",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants.C4,objsubset=DerivedQuants.C4,substr1=TRUE,substr2=TRUE)
    RecSD.C4$Yr <- substring(RecSD.C4$Label,6,nchar(RecSD.C4$Label[1])-1)
    RecSD.C4$Yr[2] <- as.numeric(RecSD.C4$Yr[3])-1
    RecSD.C4$Yr[1] <- as.numeric(RecSD.C4$Yr[2])-1
    RecSD.C4$Yr <- as.numeric(RecSD.C4$Yr)
    # Make columns with 95%CI
    RecSD.C4$upper <- RecSD.C4$Value + 1.96*RecSD.C4$StdDev
    RecSD.C4$lower <- RecSD.C4$Value - 1.96*RecSD.C4$StdDev
    RecSD.C4$lower[RecSD.C4$lower < 0] <- 0		# If <0 , make it 0
    # Convert to desired units
    RecSD.C4$Value <- RecSD.C4$Value/recUnits
    RecSD.C4$upper <- RecSD.C4$upper/recUnits
    RecSD.C4$lower <- RecSD.C4$lower/recUnits
    # Convert from quarters (EPO models) to year values
	RecSD.C4$Yr2 <- RecSD.C4$Yr
	RecSD.C4$Yr2 <- year1 + (RecSD.C4$Yr/4)-0.25
	# Drop first two rows (recVir and recInit) from table (better for general plotting head)
	RecSD2.C4 <- RecSD.C4[3:dim(RecSD.C4)[1],]

    # Make dataframe with relative Rec, years and 95CI for all eras
    recVirg.C4 <- RecSD.C4$Value[1]
    recMean.C4 <- mean(RecSD2.C4$Value[RecSD2.C4$Yr<=(endYr.C4+1)])
	RelRecSD.C4 <- RecSD2.C4
	if(scalar==0){scalar.C4=recVirg.C4} 	# scale to virgin rec
	if(scalar==1){scalar.C4=recMean.C4}	# scale to mean rec
	RelRecSD.C4$Value <- RelRecSD.C4$Value/scalar.C4
	RelRecSD.C4$upper <- RelRecSD.C4$upper/scalar.C4
    RelRecSD.C4$lower <- RelRecSD.C4$lower/scalar.C4

    # Define recrutiment table for plot (absolute or relative recs)
	if(relRec==0){RecTable.C4 <- RecSD2.C4}
	if(relRec==1){RecTable.C4 <- RelRecSD.C4}
	}

	
	##############
    #   CASE 5 
    ##############
    
    if(class(Case5)=="list")
    {
    # Make dataframe with Rec, years and 95CI for all eras    
    RecSD.C5 <- matchfun2("Recr_Virgin",0,"SPRratio_1",-1,cols=1:3,matchcol1=1,matchcol2=1,objmatch=DerivedQuants.C5,objsubset=DerivedQuants.C5,substr1=TRUE,substr2=TRUE)
    RecSD.C5$Yr <- substring(RecSD.C5$Label,6,nchar(RecSD.C5$Label[1])-1)
    RecSD.C5$Yr[2] <- as.numeric(RecSD.C5$Yr[3])-1
    RecSD.C5$Yr[1] <- as.numeric(RecSD.C5$Yr[2])-1
    RecSD.C5$Yr <- as.numeric(RecSD.C5$Yr)
    # Make columns with 95%CI
    RecSD.C5$upper <- RecSD.C5$Value + 1.96*RecSD.C5$StdDev
    RecSD.C5$lower <- RecSD.C5$Value - 1.96*RecSD.C5$StdDev
    RecSD.C5$lower[RecSD.C5$lower < 0] <- 0		# If <0 , make it 0
    # Convert to desired units
    RecSD.C5$Value <- RecSD.C5$Value/recUnits
    RecSD.C5$upper <- RecSD.C5$upper/recUnits
    RecSD.C5$lower <- RecSD.C5$lower/recUnits
    # Convert from quarters (EPO models) to year values
	RecSD.C5$Yr2 <- RecSD.C5$Yr
	RecSD.C5$Yr2 <- year1 + (RecSD.C5$Yr/4)-0.25
	# Drop first two rows (recVir and recInit) from table (better for general plotting head)
	RecSD2.C5 <- RecSD.C5[3:dim(RecSD.C5)[1],]

    # Make dataframe with relative Rec, years and 95CI for all eras
    recVirg.C5 <- RecSD.C5$Value[1]
    recMean.C5 <- mean(RecSD2.C5$Value[RecSD2.C5$Yr<=(endYr.C5+1)])
	RelRecSD.C5 <- RecSD2.C5
	if(scalar==0){scalar.C5=recVirg.C5} 	# scale to virgin rec
	if(scalar==1){scalar.C5=recMean.C5}	# scale to mean rec
	RelRecSD.C5$Value <- RelRecSD.C5$Value/scalar.C5
	RelRecSD.C5$upper <- RelRecSD.C5$upper/scalar.C5
    RelRecSD.C5$lower <- RelRecSD.C5$lower/scalar.C5

    # Define recrutiment table for plot (absolute or relative recs)
	if(relRec==0){RecTable.C5 <- RecSD2.C5}
	if(relRec==1){RecTable.C5 <- RelRecSD.C5}
	}
	
	###########
	## PLOT
	###########
    # Do not plot FORECAST
    if(forecast==F)
    {
	    # Prepare quantities for plot - CASE 1
	    Yrs <- RecTable.C1$Yr[RecTable.C1$Yr<=(endYr.C1+1)]
	    RecVals.C1 <- RecTable.C1$Value[RecTable.C1$Yr<=(endYr.C1+1)]
	    RecCIup.C1 <- RecTable.C1$upper[RecTable.C1$Yr<=(endYr.C1+1)]
	    RecCIlo.C1 <- RecTable.C1$lower[RecTable.C1$Yr<=(endYr.C1+1)]
	    x.C1 <- RecTable.C1$Yr2[RecTable.C1$Yr%in%Yrs[1:length(Yrs)]]
	    y.C1 <- RecVals.C1[1:length(RecVals.C1)]
	    
	    # Prepare quantities for plot - CASE 2
	    Yrs <- RecTable.C2$Yr[RecTable.C2$Yr<=(endYr.C2+1)]
	    RecVals.C2 <- RecTable.C2$Value[RecTable.C2$Yr<=(endYr.C2+1)]
	    RecCIup.C2 <- RecTable.C2$upper[RecTable.C2$Yr<=(endYr.C2+1)]
	    RecCIlo.C2 <- RecTable.C2$lower[RecTable.C2$Yr<=(endYr.C2+1)]
	    x.C2 <- RecTable.C2$Yr2[RecTable.C2$Yr%in%Yrs[1:length(Yrs)]]
	    y.C2 <- RecVals.C2[1:length(RecVals.C2)]
	    
	    # Prepare quantities for plot - CASE 3
	    if(class(Case3)=="list")
	    {
		    Yrs <- RecTable.C3$Yr[RecTable.C3$Yr<=(endYr.C3+1)]
		    RecVals.C3 <- RecTable.C3$Value[RecTable.C3$Yr<=(endYr.C3+1)]
		    RecCIup.C3 <- RecTable.C3$upper[RecTable.C3$Yr<=(endYr.C3+1)]
		    RecCIlo.C3 <- RecTable.C3$lower[RecTable.C3$Yr<=(endYr.C3+1)]
		    x.C3 <- RecTable.C3$Yr2[RecTable.C3$Yr%in%Yrs[1:length(Yrs)]]
		    y.C3 <- RecVals.C3[1:length(RecVals.C3)]
	    }

	    # Prepare quantities for plot - CASE 4
	    if(class(Case4)=="list")
	    {
		    Yrs <- RecTable.C4$Yr[RecTable.C4$Yr<=(endYr.C4+1)]
		    RecVals.C4 <- RecTable.C4$Value[RecTable.C4$Yr<=(endYr.C4+1)]
		    RecCIup.C4 <- RecTable.C4$upper[RecTable.C4$Yr<=(endYr.C4+1)]
		    RecCIlo.C4 <- RecTable.C4$lower[RecTable.C4$Yr<=(endYr.C4+1)]
		    x.C4 <- RecTable.C4$Yr2[RecTable.C4$Yr%in%Yrs[1:length(Yrs)]]
		    y.C4 <- RecVals.C4[1:length(RecVals.C4)]
	    }
	    
	    # Prepare quantities for plot - CASE 5
	    if(class(Case5)=="list")
	    {
		    Yrs <- RecTable.C5$Yr[RecTable.C5$Yr<=(endYr.C5+1)]
		    RecVals.C5 <- RecTable.C5$Value[RecTable.C5$Yr<=(endYr.C5+1)]
		    RecCIup.C5 <- RecTable.C5$upper[RecTable.C5$Yr<=(endYr.C5+1)]
		    RecCIlo.C5 <- RecTable.C5$lower[RecTable.C5$Yr<=(endYr.C5+1)]
		    x.C5 <- RecTable.C5$Yr2[RecTable.C5$Yr%in%Yrs[1:length(Yrs)]]
		    y.C5 <- RecVals.C5[1:length(RecVals.C5)]
	    }

	    
	    # Do not plot 95CI
	    if(uncertainty==F)
	    {    
		    # Make plot
		    windows(8,5)
		    par(mar=c(4,6,2,1))
		    #xlim <- range(pretty(x.C1,x.C2))
		    if(length(xlim)==1){xlim <- range(pretty(x.C1,x.C2))}
		    if(length(xlim)==2){xlim <- xlim}

		    ylim <- range(0,max(y.C1,y.C2))
		    if(class(Case3)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3))}
		    if(class(Case4)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3,y.C4))}
		    if(class(Case5)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3,y.C4,y.C5))}
		    options(scipen=1) 				# Do not use scientific notation in plotting
		   
	    	# Plot CASE1
	    	plot(x.C1, y.C1, xlim=xlim, ylim=ylim, axes=T, type="l",col="blue", ylab="",xlab="", lwd=2,las=1)
	    	# Plot yearly values
	    	x2.C1 <- unique(floor(x.C1))
	    	y2.C1 <- y.C1[x.C1%in%x2.C1]
	    	points(x2.C1,y2.C1, pch=19, col="blue", cex=1)
	
	    	# Plot CASE2
	    	lines(x.C2, y.C2, lty=2, pch=21, col="red", lwd=2)
	    	# Plot yearly values
	    	x2.C2 <- unique(floor(x.C2))
	    	y2.C2 <- y.C2[x.C2%in%x2.C2]
	    	points(x2.C2,y2.C2, pch=24, col="red", lwd=1.25, cex=1)

	    	# Plot CASE3
	    	if(class(Case3)=="list")
	    	{
		    	lines(x.C3, y.C3, lty=1, pch=21, col="green", lwd=2)
		    	# Plot yearly values
		    	x2.C3 <- unique(floor(x.C3))
		    	y2.C3 <- y.C3[x.C3%in%x2.C3]
		    	points(x2.C3,y2.C3, pch=22, col="green", lwd=1.25, cex=1)
	    	}
	
	    	# Plot CASE4
	    	if(class(Case4)=="list")
	    	{
		    	lines(x.C4, y.C4, lty=2, pch=21, col="black", lwd=2)
		    	# Plot yearly values
		    	x2.C4 <- unique(floor(x.C4))
		    	y2.C4 <- y.C4[x.C4%in%x2.C4]
		    	points(x2.C4,y2.C4, pch=23, col="black", lwd=1.25, cex=1)
	    	}
	    	
	    	# Plot CASE5
	    	if(class(Case5)=="list")
	    	{
		    	lines(x.C5, y.C5, lty=3, pch=21, col="purple", lwd=2)
		    	# Plot yearly values
		    	x2.C5 <- unique(floor(x.C5))
		    	y2.C5 <- y.C5[x.C5%in%x2.C5]
		    	points(x2.C5,y2.C5, pch=25, col="purple", lwd=1.25, cex=1)
	    	}
	    	
	    	# Make legend
	    	leg.C1 <- Legends[1]
	    	leg.C2 <- Legends[2]
	    	if(class(Case3)=="list"){leg.C3 <- Legends[3]}
	    	if(class(Case4)=="list"){leg.C4 <- Legends[4]}
	    	if(class(Case5)=="list"){leg.C5 <- Legends[5]}
	    	
	    	# Plot legend
	    	if(legendOption==0)
		    {
			    legend(x=x.C1[1]+2, y=ylim[2], legend = leg.C1, lty=1, pch=19, cex=1, col="blue", lwd=2, bty="n")
			    legend(x=x.C2[1]+2, y=.9*ylim[2], legend = leg.C2, lty=1, pch=24, cex=1, col="red", lwd=2, bty="n")
			    if(class(Case3)=="list"){legend(x=x.C3[1]+2, y=.8*ylim[2], legend = leg.C3, lty=1, pch=22, cex=1, col="green", lwd=2, bty="n")}
			    if(class(Case4)=="list"){legend(x=x.C4[1]+10, y=ylim[2], legend = leg.C4, lty=2, pch=23, cex=1, col="black", lwd=2, bty="n")}
			    if(class(Case5)=="list"){legend(x=x.C5[1]+10, y=.9*ylim[2], legend = leg.C5, lty=3, pch=25, cex=1, col="purple", lwd=2, bty="n")}
		    }
		    
		    if(legendOption!=0)
		    {
			    legend(x=x.C1[1]+5, y=ylim[2], legend = leg.C1, lty=1, pch=19, cex=.75, col="blue", lwd=2, bty="n")
			    legend(x=x.C2[1]+5, y=.9*ylim[2], legend = leg.C2, lty=1, pch=24, cex=.75, col="red", lwd=2, bty="n")
			    if(class(Case3)=="list"){legend(x=x.C3[1]+5, y=.8*ylim[2], legend = leg.C3, lty=1, pch=22, cex=.75, col="green", lwd=2, bty="n")}
			    if(class(Case4)=="list"){legend(x=x.C4[1]+20, y=ylim[2], legend = leg.C4, lty=2, pch=23, cex=.75, col="black", lwd=2, bty="n")}
			    if(class(Case5)=="list"){legend(x=x.C5[1]+20, y=.9*ylim[2], legend = leg.C5, lty=3, pch=25, cex=.75, col="purple", lwd=2, bty="n")}
		    }

		    
		    # Plot Labels
		    title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    if(relRec==0)
		    {
			    title(ylab="Recruitment (millions of fish)", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento (milliones de peces)", cex.lab=1.6, line=3)
		    }
		    if(relRec==1)
		    {
			    title(ylab="Relative recruitment", cex.lab=1.6, line=5)
			    title(ylab="Reclutamiento relativo", cex.lab=1.6, line=3)
			    abline(h=1, lty="dashed", lwd=1.25)
		    }
	    }
	    	    
	    # Plot 95CI
	    #if(uncertainty==T)
	    #{
		    ## Prepare CI quantities for plot
		    #yCIup <- RecTable$upper[1:length(RecCIup)]
		    #yCIlo <- RecTable$lower[1:length(RecCIlo)]
	    		    
		    ## Make plot
		    # windows(8,5)
		    #par(mar=c(4,6,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(yCIup))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    ## Plot CIs
		    #lines(x,yCIup,col="black",lty="dashed", lwd=1.25)
		    #lines(x,yCIlo,col="black",lty="dashed", lwd=1.25)
		    ## Plot yearly values
		    #x2 <- unique(floor(x))
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    ## Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #if(relRec==0)
		    #{
			 #   title(ylab="Recruitment (millions of fish)", cex.lab=1.6, line=5)
			  #  title(ylab="Reclutamiento (milliones de peces)", cex.lab=1.6, line=3)
		    #}
		    #if(relRec==1)
		    #{
			 #   title(ylab="Relative recruitment", cex.lab=1.6, line=5)
			  #  title(ylab="Reclutamiento relativo", cex.lab=1.6, line=3)
			   # abline(h=1, lty="dashed", lwd=1.25)
		    #}	
	    #}
    }
        
    # Plot FORECAST
    #if(forecast==T)
    #{
	    # Prepare quantities for plot
	    #Yrs <- RecTable$Yr
	    #RecVals <- RecTable$Value
	    #RecCIup <- RecTable$upper
	    #RecCIlo <- RecTable$lower
	    #x <- RecTable$Yr2[RecTable$Yr%in%Yrs[1:length(Yrs)]]
	    #y <- RecVals[1:length(RecVals)]
	    #xLast <- RecTable$Yr2[RecTable$Yr==(endYr+1)] 	# Get last year of historical Era
	    #yLast <- RecVals[RecTable$Yr==(endYr+1)] 	# Get SpawnBio last in last year of historical Era

	    # Do not plot 95CI
	    #if(uncertainty==F)
	    #{		
		    ## Make plot
		    #windows(8,5)
		    #par(mar=c(4,6,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(y))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    ## Plot yearly values
		    #x2 <- unique(floor(x))
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(xLast,yLast, pch=19, col="black", cex=1.5)
		    # Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #if(relRec==0)
		    #{
			 #   title(ylab="Recruitment (millions of fish)", cex.lab=1.6, line=5)
			  #  title(ylab="Reclutamiento (milliones de peces)", cex.lab=1.6, line=3)
		    #}
		    #if(relRec==1)
		    #{
			 #   title(ylab="Relative recruitment", cex.lab=1.6, line=5)
			  #  title(ylab="Reclutamiento relativo", cex.lab=1.6, line=3)
			   # abline(h=1, lty="dashed", lwd=1.25)
		    #}	
	    #}
	    
	    # Plot 95CI
	    #if(uncertainty==T)
	    #{    
		    # Prepare CI quantities for plot
		    #yCIup <- RecTable$upper[1:length(RecCIup)]
		    #yCIlo <- RecTable$lower[1:length(RecCIlo)]
		        		
		    # Make plot
		    #windows(8,5)
		    #par(mar=c(4,6,2,1))
		    #xlim <- range(pretty(x))
		    #ylim <- range(0,pretty(yCIup))
		    #options(scipen=1) 				# Do not use scientific notation in plotting
		    #plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25,las=1)
		    # Plot CIs
		    #lines(x,yCIup,col="black",lty="dashed", lwd=1.25)
		    #lines(x,yCIlo,col="black",lty="dashed", lwd=1.25)
		    # Plot yearly values
		    #x2 <- unique(floor(x))
		    #y2 <- y[x%in%x2]
		    #points(x2,y2, pch=19, col="black", cex=.75)
		    #points(xLast,yLast, pch=19, col="black", cex=1.5)
		    ## Plot Labels
		    #title(xlab="Year-Ano", cex.lab=1.6, line=3)
		    #if(relRec==0)
		    #{
			#    title(ylab="Recruitment (millions of fish)", cex.lab=1.6, line=5)
			#    title(ylab="Reclutamiento (milliones de peces)", cex.lab=1.6, line=3)
		    #}
		    #if(relRec==1)
		    #{
			     #title(ylab="Relative recruitment", cex.lab=1.6, line=5)
			     #title(ylab="Reclutamiento relativo", cex.lab=1.6, line=3)
			     #abline(h=1, lty="dashed", lwd=1.25)
		    #}	
	    #}
    #}
}
#plotRecXcases(Case1=BaseRep, Case2=Retro2008Rep, Case3=Retro2007Rep, Case4=Retro2006Rep, Case5=Retro2005Rep, Legends=c("2009","2008","2007","2006","2005"), year1=1975, recUnits=1000000, relRec=1, scalar=1, forecast=F, uncertainty=F, retro=1, EndYrCases=seq(136,120,-4),legendOption=1)


####################################################################################
### 	Growth (TWO CASES)
####################################################################################

#PathOtolithDat <- "C:/Documents and Settings/alexdasilva/My Documents/IATTC/IATTC_2009/MEETINGS/SARM10/ASSESSMENTS_2009/BET_EPO/SS_V302D_Feb09/SS3_runs_SARM10/R_output/input/otolith_data.csv"

# Make reps
#BasePath <-'V:/alexdasilva/2009/SSruns/BET-EPO/SS_V302D_Feb09/SS3_runs_SARM10/BASE_CASE/BC_hybrid_doMSY_wHess/'
#BaseRep <- SSv3_output(dir=BasePath,ncols=200,covar=F)
#RichardsPath <- 'V:/alexdasilva/2009/SSruns/BET-EPO/SS_V302D_Feb09/SS3_runs_SARM10/SENSITIVITIES/GROWTH/Richards/Richards_L2_fix186-5_hybrid_doMSY_doHess/'
#RichardsRep <- SSv3_output(dir=RichardsPath,ncols=200,covar=F)

#Case1=BaseRep
#Case2=RichardsRep
#PlotOtoliths=1
#PathOtolithDat=PathOtolithDat
#leg.C1=c("von Bertalanffy (base case - caso base)")
#leg.C2=c("Richards")

plotGrowthXcases <- function(Case1, Case2, PlotOtoliths, PathOtolithDat, leg.C1, leg.C2)
{
	# Get quantities from Case1
	morph_indexing.C1 <- Case1$morph_indexing
	growdat.C1 <- Case1$endgrowth
	nseasons.C1 <- Case1$nseasons
	nsexes.C1 <- Case1$nsexes
	# Get quantities from Case2
	morph_indexing.C2 <- Case2$morph_indexing
	growdat.C2 <- Case2$endgrowth
	nseasons.C2 <- Case2$nseasons
	nsexes.C2 <- Case2$nsexes

	# Read the otolith data
	if(PlotOtoliths==1){OtolithDat <- read.csv(PathOtolithDat,header=T)}
	
	# Prepare quantities for plot
	
	##############
    #   CASE 1 
    ##############

	# Mid year mean length at age with 95% range of lengths (by sex if applicable)
	mainmorphs.C1 <- morph_indexing.C1$Index[morph_indexing.C1$Bseas==1]
	growdatF.C1 <- growdat.C1[growdat.C1$Morph==mainmorphs.C1[1],]
	growdatF.C1$Sd_Size <- growdatF.C1$SD_Mid
	growdatF.C1$high <- growdatF.C1$Len_Mid + 1.96*growdatF.C1$Sd_Size
	growdatF.C1$low <- growdatF.C1$Len_Mid - 1.96*growdatF.C1$Sd_Size
	x.C1 <- growdatF.C1$Age
	y.C1 <- growdatF.C1$Len_Mid
	# Prepare CI quantities for plot
	yCIup.C1 <- growdatF.C1$high
	yCIlo.C1 <- growdatF.C1$low

	
	##############
    #   CASE 2 
    ##############

	# Mid year mean length at age with 95% range of lengths (by sex if applicable)
	mainmorphs.C2 <- morph_indexing.C2$Index[morph_indexing.C2$Bseas==1]
	growdatF.C2 <- growdat.C2[growdat.C2$Morph==mainmorphs.C2[1],]
	growdatF.C2$Sd_Size <- growdatF.C2$SD_Mid
	growdatF.C2$high <- growdatF.C2$Len_Mid + 1.96*growdatF.C2$Sd_Size
	growdatF.C2$low <- growdatF.C2$Len_Mid - 1.96*growdatF.C2$Sd_Size
	x.C2 <- growdatF.C2$Age
	y.C2 <- growdatF.C2$Len_Mid
	# Prepare CI quantities for plot
	yCIup.C2 <- growdatF.C2$high
	yCIlo.C2 <- growdatF.C2$low

	
	# Make plot
	windows(8,5)
	par(mar=c(4.5,4.5,2,1))
	xlim <- range(0,max(x.C1,x.C2))
	ylim <- range(0,pretty(yCIup.C1,yCIup.C2))
	#ylim <- range(0,max(y.C1,y.C2))
	
	# Plot case 1	
	plot(x.C1, y.C1, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25, las=1)
	# Plot CIs
	polygon(c(x.C1,rev(x.C1)), c(yCIlo.C1,rev(yCIup.C1)), col='gray', border="NA")
	lines(x.C1,y.C1,col="blue",lty=1, lwd=2)
	lines(x.C1,yCIup.C1,col="blue",lty=3, lwd=1.25)
	lines(x.C1,yCIlo.C1,col="blue",lty=3, lwd=1.25)
	
	# Plot case 1	
	lines(x.C2, y.C2, col="red", lty=2, lwd=2)
	# Plot CIs
	polygon(c(x.C2,rev(x.C2)), c(yCIlo.C2,rev(yCIup.C2)), col='red', border="NA", density=10, angle=45)
	lines(x.C2,y.C2,col="red", lty=2, lwd=2)
	lines(x.C2,yCIup.C2,col="red",lty=3, lwd=1.25)
	lines(x.C2,yCIlo.C2,col="red",lty=3, lwd=1.25)
	
	if(PlotOtoliths==1){points(OtolithDat[,1],OtolithDat[,2], pch=19, col="black", cex=.5)}

	#if(nsexes > 1)
	#{
	#	growdatM <- growdat[growdat$Morph==mainmorphs[2],]
	#	xm <- growdatM$Age
	#	growdatM$Sd_Size <- growdatM$SD_Mid
	#	growdatM$high <- growdatM$Len_Mid + 1.96*growdatM$Sd_Size
	#	growdatM$low <- growdatM$Len_Mid - 1.96*growdatM$Sd_Size
	#	lines(xm,growdatM$Len_Mid,col="blue",lwd=2,type="l")
	#	lines(xm,growdatM$high,col="blue",lwd=1,lty="dashed")
	#	lines(xm,growdatM$low,col="blue",lwd=1,lty="dashed")
	#	grid()
	#	legend("topleft",bty="n", c("Females","Males"), lty=1, col = c("red","blue"))
    #}
    
    # Plot legends
    
	legend(x=x.C1[10], y=.3*ylim[2], legend = leg.C1, lty=1, lwd=2, col="blue", bty="n", cex=1.25)
	legend(x=x.C1[10], y=.2*ylim[2], legend = leg.C2, lty=2, lwd=2, col="red", bty="n", cex=1.25)
    
    # Plot Labels
	title(xlab="Age in quarters - Edad en trimestres", cex.lab=1.3, line=3)
	title(ylab="Length (cm) - Talla (cm)", cex.lab=1.3, line=3)
}
#plotGrowthXcases(Case1=BaseRep, Case2=RichardsRep, PlotOtoliths=1, PathOtolithDat=PathOtolithDat, leg.C1=c("von Bertalanffy (base case - caso base)"), leg.C2=c("Richards"), xlim=c(1975,2015))



####################################################################################
### 	SIZE-SELECTIVITY CURVES (TWO CASES)
####################################################################################

# Make reps
#FitWCPOPath <- 'V:/alexdasilva/2009/SSruns/BET-EPO/SS_V302D_Feb09/SS3_runs_SARM10/SENSITIVITIES/EPO-WCPO_2-4-6/fit_WCPO_data_hybrid _doMSY_wHess/'
#FitWCPOrep <- SSv3_output(dir=FitWCPOPath,ncols=300,covar=F)
#NoFitWCPOPath <- 'V:/alexdasilva/2009/SSruns/BET-EPO/SS_V302D_Feb09/SS3_runs_SARM10/SENSITIVITIES/EPO-WCPO_2-4-6/WCPO_data_SHAREselex_hybrid_doMSY_wHess/'
#NoFitWCPOrep <- SSv3_output(dir=NoFitWCPOPath,ncols=300,covar=F)

#Case1=FitWCPOrep
#Case2=NoFitWCPOrep
#FleetNums=seq(1,25)
#numRows=5
#numCols=5

plotSizeSelexXcases <- function(Case1, Case2, FleetNums=c(1,2,3,4,5,6,7,8,9,14,15), numRows, numCols)
{
	# Get quantities from Case1
	startYr.C1 <- Case1$startyr
	endYr.C1 <- Case1$endyr
	numSexes.C1 <- Case1$nsexes
	SizeSelexDat.C1 <- Case1$sizeselex
	numSizeBins.C1 <- Case1$nlbins

	# Get quantities from Case2
	startYr.C2 <- Case2$startyr
	endYr.C2 <- Case2$endyr
	numSexes.C2 <- Case2$nsexes
	SizeSelexDat.C2 <- Case2$sizeselex

	# Common quantities to both cases
	numFleets <- Case1$nfleets
	FleetNames <- Case1$FleetNames
	SizeBins <- Case1$lbinspop
	
	# Subset SizeSelexDat for endYr and gender=1
	SizeSelex.C1 <- subset(SizeSelexDat.C1, year==endYr.C1 & gender==1)
	SizeSelex.C2 <- subset(SizeSelexDat.C2, year==endYr.C2 & gender==1)
	
	# Plot
	windows(8,6)
	par(mfrow=c(numRows,numCols), mar=c(2,2,2,1), omi=c(.5,.5,.2,0))
	for(ifleet in 1:numFleets)
	{
		if(ifleet%in%FleetNums)
		{
			plot(SizeBins, SizeSelex.C1[ifleet,5:dim(SizeSelex.C1)[2]], type="l", ylim=c(0,1), ylab="",xlab="", col="blue", lwd=2, lty=1, cex.axis=.75, mgp=c(2,0.5,0), tcl=-.3, las=1)
			lines(SizeBins, SizeSelex.C2[ifleet,5:dim(SizeSelex.C2)[2]], col="red", lty=2, lwd=2)

			leg <- FleetNames[ifleet]
			#leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
			title(main=leg, cex.main=1)
		}
	}

	#legend(5,0.95,lwd=2, col=c("black", "slate grey"), legend=c("SS3"))
	mtext(side=1, outer=T, "Length (cm)-Talla (cm)", line=1.5, cex=1.25)
	mtext(side=2, outer=T, "Selectivity and retention - Selectividad e retencion", line=1.5, cex=1.25)
}
#plotSizeSelexXcases(Case1=FitWCPOrep, Case2=NoFitWCPOrep, FleetNums=seq(1,25), numRows=5, numCols=5)


####################################################################################
### 	PHASE PLOTS
####################################################################################

#PathPhasePlotsDat <- "C:/Documents and Settings/alexdasilva/My Documents/IATTC/IATTC_2009/MEETINGS/SARM10/ASSESSMENTS_2009/BET_EPO/SS_V302D_Feb09/SS3_runs_SARM10/R_output/input/PHASE_PLOTS_BET_to_R.csv"

#makePhasePlots <- function(PathPhasePlotsDat)
#{
#	PhasePlotsDat <- read.csv(PathPhasePlotsDat,header=T)
		
	# Prepare quantities for plot
	
	#Get FmultFmult
	
	#mainmorphs <- morph_indexing$Index[morph_indexing$Bseas==1]
	#growdatF <- growdat[growdat$Morph==mainmorphs[1],]
	#growdatF$Sd_Size <- growdatF$SD_Mid
	#growdatF$high <- growdatF$Len_Mid + 1.96*growdatF$Sd_Size
	#growdatF$low <- growdatF$Len_Mid - 1.96*growdatF$Sd_Size
	#x <- growdatF$Age
	#y <- growdatF$Len_Mid
	# Prepare CI quantities for plot
	#yCIup <- growdatF$high
	#yCIlo <- growdatF$low

	# Make plot
	#windows(8,5)
	#par(mar=c(4.5,4.5,2,1))
	#xlim <- range(pretty(x))
	#ylim <- range(0,pretty(yCIup))		
	#plot(x, y, xlim=xlim, ylim=ylim, axes=T, type="l",col="black", ylab="",xlab="", lwd=1.25, las=1)
	# Plot CIs
	#polygon(c(x,rev(x)), c(yCIlo,rev(yCIup)), col='gray', border="NA")
	#lines(x,y,col="black",lty=1, lwd=1.25)
	#lines(x,yCIup,col="black",lty=3, lwd=1.25)
	#lines(x,yCIlo,col="black",lty=3, lwd=1.25)
	#if(PlotOtoliths==1){points(OtolithDat[,1],OtolithDat[,2], pch=19, col="black", cex=.75)}

	#if(nsexes > 1)
	#{
	#	growdatM <- growdat[growdat$Morph==mainmorphs[2],]
	#	xm <- growdatM$Age
	#	growdatM$Sd_Size <- growdatM$SD_Mid
	#	growdatM$high <- growdatM$Len_Mid + 1.96*growdatM$Sd_Size
	#	growdatM$low <- growdatM$Len_Mid - 1.96*growdatM$Sd_Size
	#	lines(xm,growdatM$Len_Mid,col="blue",lwd=2,type="l")
	#	lines(xm,growdatM$high,col="blue",lwd=1,lty="dashed")
	#	lines(xm,growdatM$low,col="blue",lwd=1,lty="dashed")
	#	grid()
	#	legend("topleft",bty="n", c("Females","Males"), lty=1, col = c("red","blue"))
    #}
    
    # PLOT OTOLITH DATA
    
    # Plot Labels
	#title(xlab="Age in quarters - Edad en trimestres", cex.lab=1.3, line=3)
	#title(ylab="Length (cm) - Talla (cm)", cex.lab=1.3, line=3)
#}
#plotGrowth(myreplist,PlotOtoliths=1, PathOtolithDat=PathOtolithDat)


####################################################################################
###		AVERAGE QUARTERLY FISHING MORTALITY (Fig 4.1)
####################################################################################

#replist <- myreplist

#MatAge <- replist$M_at_age

#replist$endyr
#unique(replist$gender)


###

### AVERAGE ANNUAL FISHING MORTALITY (FIG 4.3)


## Impact plots (Fig 4.8)

## Averege weight (Fig 4.9)


### Management quantitites (Fig 5.2)

### Phase plot (Figure 5.3)


### PREDICTED QUARTERLY CATCHES

### SENSITIVITY PLOTS


 










### WORKING HERE ON NEW PLOTS


####################################################################################
### 	Time series (MULTIPLE CASES) - summary biomass   (NEW PLOT)
####################################################################################

plotBioSmryXcases1 <- function(Case1, Case2, Case3, Case4, Case5, Legends, year1, convertTon, retro, EndYrCases)
{
	# Get quantities from replist
	startYr <- Case1$startyr
	numSeasons <- Case1$nseasons
	numAreas <- Case1$nareas
	TSraw.C1 <- Case1$timeseries
	TSraw.C2 <- Case2$timeseries
	if(class(Case3)=="list"){TSraw.C3 <- Case3$timeseries}
	if(class(Case4)=="list"){TSraw.C4 <- Case4$timeseries}
	if(class(Case5)=="list"){TSraw.C5 <- Case5$timeseries}

	# Define endYr
	# Standard analysis
	if(retro!=1)
	{
		endYr.C1 <- Case1$endyr
		endYr.C2 <- Case2$endyr
		if(class(Case3)=="list"){endYr.C3 <- Case3$endyr}
		if(class(Case4)=="list"){endYr.C4 <- Case4$endyr}
		if(class(Case5)=="list"){endYr.C5 <- Case5$endyr}
	}
	# Retrospective analysis
	if(retro==1)
	{
		endYr.C1 <- EndYrCases[1]
		endYr.C2 <- EndYrCases[2]
		endYr.C3 <- EndYrCases[3]
		endYr.C4 <- EndYrCases[4]
		endYr.C5 <- EndYrCases[5]
	}
	
	#####################################
    #   CASE 1 - prepare quantities
    #####################################

	# Prepare quantities for plot
	TSraw.C1$Yr <- TSraw.C1$Yr + (TSraw.C1$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat.C1 <- TSraw.C1[TSraw.C1$Yr <= endYr.C1+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat.C1 <- TSdat.C1[TSdat.C1$Seas==1 & TSdat.C1$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat.C1$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat.C1$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	if(convertTon==0) {BioSmry.C1 <- TSdat.C1$Bio_smry}
	if(convertTon==1) {BioSmry.C1 <- TSdat.C1$Bio_smry/1000}
	x.C1 <- Years
	y.C1 <- BioSmry.C1[3:length(BioSmry.C1)]
		
	######################################
    #   CASE 2 - prepare quantities
    ######################################

	# Prepare quantities for plot
	TSraw.C2$Yr <- TSraw.C2$Yr + (TSraw.C2$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat.C2 <- TSraw.C2[TSraw.C2$Yr <= endYr.C2+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat.C2 <- TSdat.C2[TSdat.C2$Seas==1 & TSdat.C2$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat.C2$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat.C2$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	if(convertTon==0) {BioSmry.C2 <- TSdat.C2$Bio_smry}
	if(convertTon==1) {BioSmry.C2 <- TSdat.C2$Bio_smry/1000}
	x.C2 <- Years
	y.C2 <- BioSmry.C2[3:length(BioSmry.C2)]
	
	
	######################################
    #   CASE 3 - prepare quantities
    ######################################

	# Prepare quantities for plot
	TSraw.C3$Yr <- TSraw.C3$Yr + (TSraw.C3$Seas-1)/numSeasons
	# Subset for Era of interest - Option 1 (HISTORICAL ERA)
	TSdat.C3 <- TSraw.C3[TSraw.C3$Yr <= endYr.C3+1,]  	# Subset TimeSeries for historical era (include first forecast yr)	
	TSdat.C3 <- TSdat.C3[TSdat.C3$Seas==1 & TSdat.C3$Area==1,]
	# Subset Years and Total Biomass quantities for plot
	YearsRaw <- TSdat.C3$Yr
	numYears <- length(YearsRaw)-2 		# Get number of years (drop VIRG and INIT)
	if(year1==1) {Years<- TSdat.C3$Yr}
	if(year1!=1) {Years <- year1 + 0:(numYears-1)/4}	# Convert from quarters (EPO models) to year values
	if(convertTon==0) {BioSmry.C3 <- TSdat.C3$Bio_smry}
	if(convertTon==1) {BioSmry.C3 <- TSdat.C3$Bio_smry/1000}
	x.C3 <- Years
	y.C3 <- BioSmry.C3[3:length(BioSmry.C3)]


	#################
    #   Make plot 
    #################

	# Make plot
	windows(8,5)
	par(mar=c(4,8,2,1))
	xlim <- range(pretty(x.C1))
	ylim <- range(0,max(y.C1,y.C2))
	if(class(Case3)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3))}
	if(class(Case4)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3,y.C4))}
	if(class(Case5)=="list"){ylim <- range(0,max(y.C1,y.C2,y.C3,y.C4,y.C5))}

	options(scipen=1) 				# Do not use scientific notation in plotting
	
	# Plot CASE1
	plot(x.C1, y.C1, xlim=xlim, ylim=ylim, axes=T, type="l",col="blue", ylab="",xlab="", lwd=2,las=1)
	# Plot yearly values
	x2.C1 <- unique(floor(x.C1))
	y2.C1 <- y.C1[x.C1%in%x2.C1]
	points(x2.C1,y2.C1, pch=19, col="blue", cex=1)
	
	# Plot CASE2
	lines(x.C2, y.C2, lty=2, pch=21, col="red", lwd=2)
	# Plot yearly values
	x2.C2 <- unique(floor(x.C2))
	y2.C2 <- y.C2[x.C2%in%x2.C2]
	points(x2.C2,y2.C2, pch=24, col="red", lwd=1.25, cex=1)

	# Plot CASE3
	if(class(Case3)=="list")
	{
		lines(x.C3, y.C3, lty=1, pch=21, col="green", lwd=2)
		# Plot yearly values
		x2.C3 <- unique(floor(x.C3))
		y2.C3 <- y.C3[x.C3%in%x2.C3]
		points(x2.C3,y2.C3, pch=22, col="green", lwd=1.25, cex=1)
	}

	# Plot CASE4
	if(class(Case4)=="list")
	{
		lines(x.C4, y.C4, lty=2, pch=21, col="black", lwd=2)
		# Plot yearly values
		x2.C4 <- unique(floor(x.C4))
		y2.C4 <- y.C4[x.C4%in%x2.C4]
		points(x2.C4,y2.C4, pch=23, col="black", lwd=1.25, cex=1)
	}

	# Plot CASE5
	if(class(Case5)=="list")
	{
		lines(x.C5, y.C5, lty=3, pch=21, col="purple", lwd=2)
		# Plot yearly values
		x2.C5 <- unique(floor(x.C5))
		y2.C5 <- y.C5[x.C5%in%x2.C5]
		points(x2.C5,y2.C5, pch=25, col="purple", lwd=1.25, cex=1)
	}

	# Make legend
	leg.C1 <- Legends[1]
	leg.C2 <- Legends[2]
	if(class(Case3)=="list"){leg.C3 <- Legends[3]}
	if(class(Case4)=="list"){leg.C4 <- Legends[4]}
	if(class(Case5)=="list"){leg.C5 <- Legends[5]}

	
	# Plot legend
	legend(x=x.C1[1]+2, y=.5*ylim[2], legend = leg.C1, lty=1, pch=19, cex=1, col="blue", lwd=2, bty="n")
	legend(x=x.C2[1]+2, y=.4*ylim[2], legend = leg.C2, lty=1, pch=24, cex=1, col="red", lwd=2, bty="n")
	if(class(Case3)=="list"){legend(x=x.C3[1]+2, y=.3*ylim[2], legend = leg.C3, lty=1, pch=22, cex=1, col="green", lwd=2, bty="n")}
	if(class(Case4)=="list"){legend(x=x.C4[1]+2, y=.2*ylim[2], legend = leg.C4, lty=2, pch=23, cex=1, col="black", lwd=2, bty="n")}
	if(class(Case5)=="list"){legend(x=x.C5[1]+2, y=.1*ylim[2], legend = leg.C5, lty=3, pch=25, cex=1, col="purple", lwd=2, bty="n")}

	# Plot Labels
	title(xlab="Year-Ano", cex.lab=1.6, line=3)
	title(ylab="Summary biomass (t)", cex.lab=1.6, line=7)
	title(ylab="Biomasa sumaria (t)", cex.lab=1.6, line=5)
 
    #abline(h=0,col="grey")
}
#plotBioSmryXcases2(Case1=BaseRep, Case2=Retro2008Rep, Case3=Retro2007Rep, Case4=Retro2006Rep, Case5=Retro2005Rep, Legends=c("2009","2008","2007","2006","2005"), year1=1975, convertTon=1, retro=1, EndYrCases=seq(136,120,-4))
