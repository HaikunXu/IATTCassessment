#' Plot size selectivity for SS runs
#' 
#' \code{PlotSelex} This function plot estimated selectivity curves for specified fleets
#' 
#' @export
#' 

# Path <- c("C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET research/EPO/S-Spline")
# FleetNums <- c(1,2,3,4,5,6)

PlotSelex <- function(Path, replist, FleetNums, numRows, numCols, w, h)
{
    # Get quantities from replist
    replist <- r4ss::SS_output(dir = Path, ncols = 400, covar = T, printstats = F, verbose = FALSE)
    startYr <- replist$startyr
    endYr <- replist$endyr
    numSexes <- replist$nsexes
    numFleets <- replist$nfleets
    SizeSelexDat <- replist$sizeselex
    numSizeBins <- replist$nlbins
    SizeBins <- replist$lbinspop
    FleetNames <- replist$FleetNames
    
    SizeSelex <- SizeSelexDat %>% filter(Yr %in% c(startYr,endYr),
                                         Sex==1,
                                         Fleet %in% FleetNums,
                                         Factor=="Lsel")
    SizeSelex$FLeet_Names <- FleetNames[SizeSelex$Fleet]
    
    SizeSelex_DF <- SizeSelex %>% gather(6:(ncol(SizeSelex)-2),key="Length",value="Selectivity") %>%
        mutate(Length=as.numeric(Length))
    
    ggplot(data=SizeSelex_DF) +
        # geom_line(aes(x=Length,y=Selectivity,color=Time)) +
        geom_line(aes(x=Length,y=Selectivity)) +
        facet_wrap(~FLeet_Names) +
        theme_bw(15)
    
}