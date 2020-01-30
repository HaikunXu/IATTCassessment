#' Plot size selectivity for SS runs
#' 
#' \code{cpue_fit} This function plot estimated selectivity curves for specified fleets
#' 
#' @export
#' 

plotSelex <- function(Path, replist, FleetNums, numRows, numCols, w, h)
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
    tiff(paste0(Path,"Size_Selex.tif"),width = w, height = h, units = "px")
    par(mfrow=c(numRows,numCols), mar=c(3,3,3,3), omi=c(1,1,1,1))
    for(ifleet in 1:numFleets)
    {
        if(ifleet%in%FleetNums)
        {
            plot(SizeBins, SizeSelex[ifleet,6:dim(SizeSelex)[2]], type="l", ylim=c(0,1), ylab="",xlab="", col="black", lwd=2, cex.axis=1.5, mgp=c(2,0.5,0), tcl=-.3, las=1)
            leg <- FleetNames[ifleet]
            #leg <- paste("Fishery ", ifleet," - Pescaria ", ifleet, sep="")
            title(main=leg, cex.main=2)
        }
    }
    
    #legend(5,0.95,lwd=2, col=c("black", "slate grey"), legend=c("SS3"))
    mtext(side=1, outer=T, "Length (cm)-Talla (cm)", line=2, cex=2.5)
    mtext(side=2, outer=T, "Selectivity and retention - Selectividad e retencion", line=2, cex=2.5)
    dev.off()
}