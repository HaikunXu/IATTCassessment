#' Plot size selectivity for SS runs
#' 
#' /code{PlotSelex} This function plot estimated selectivity curves for specified fleets
#' 
#' @export
#' 
# 
# Path <- c("D:/OneDrive - IATTC/IATTC/2023/SAC14/Assessment/Stepwise/M0/")
# FleetNums <- c(1,2,3,4,5,6,24,25)

PlotSelex <- function(Path, FleetNums, numRows = 0, w = 8, h = 8)
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
    
    SizeSelex <- SizeSelexDat %>% filter(Yr %in% c(endYr),
                                         Sex==1,
                                         Fleet %in% FleetNums,
                                         Factor=="Lsel")
    SizeSelex$FLeet_Names <- FleetNames[SizeSelex$Fleet]
    
    SizeSelex_DF <- SizeSelex %>% gather(6:(ncol(SizeSelex)-2),key="Length",value="Selectivity") %>%
        mutate(Length=as.numeric(Length))
    
    if(numRows == 0) {
        ggplot(data=SizeSelex_DF) +
        geom_line(aes(x=Length,y=Selectivity)) +
        facet_wrap(~FLeet_Names) +
        theme_bw(15)
    }
    else {
      ggplot(data=SizeSelex_DF) +
        geom_line(aes(x=Length,y=Selectivity)) +
        facet_wrap(~FLeet_Names, nrow = numRows) +
        theme_bw(15)
    }

    ggsave(file = paste0(Path,"Sel.png"), width = w, height = h)
    ggsave(file = paste0(Path,"Sel.pdf"), width = w, height = h)
    
    ggplot(data=SizeSelex_DF) +
      geom_line(aes(x=Length,y=Selectivity, color = FLeet_Names)) +
      # facet_wrap(~FLeet_Names, nrow = numRows) +
      theme_bw(15)
    ggsave(file = paste0(Path,"Sel2.png"), width = w, height = h)
    ggsave(file = paste0(Path,"Sel2.pdf"), width = w, height = h)
    
}