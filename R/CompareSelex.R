#' Compare size selectivity for SS runs
#' 
#' /code{CompareSelex} This function plot compare selectivity curves among models
#' 
#' @export
#' 

Path <- c("C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET research/EPO/R-DN",
          "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET research/EPO/R-Spline")
FleetNums <- c(1,2,3,4,5,6)
Legend <- c("R-DN","R-Spline")

CompareSelex <- function(Path, FleetNums, Legend)
{
  # Get quantities from replist
  for (i in 1:length(Path)) {
    print(i)
    replist <- r4ss::SS_output(dir = Path[i], ncols = 400, covar = T, printstats = F, verbose = FALSE)
    startYr <- replist$startyr
    endYr <- replist$endyr
    numSexes <- replist$nsexes
    numFleets <- replist$nfleets
    SizeSelexDat <- replist$sizeselex
    numSizeBins <- replist$nlbins
    SizeBins <- replist$lbinspop
    # Subset SizeSelexDat for endYr and gender=1
    SizeSelex <- SizeSelexDat %>% filter(Yr %in% c(startYr,endYr),
                                         Sex==1,
                                         Fleet %in% FleetNums,
                                         Factor=="Lsel") %>%
      mutate(Time=ifelse(Yr==startYr,"Early","Late"))
    
    # SizeSelex <- SizeSelex[FleetNums,6:dim(SizeSelex)[2]]
    SizeSelex$Model <- rep(Legend[i],each=2)
    SizeSelex$Fleet <- rep(replist$FleetNames[FleetNums],each=2)
    if(i==1) SizeSelex_All <- SizeSelex
    else SizeSelex_All <- rbind(SizeSelex_All,SizeSelex)
  }
  
  SizeSelex_All2 <- SizeSelex_All %>%
    gather(6:(ncol(SizeSelex_All)-2),key="Length",value="Selectivity") %>%
    mutate(Length=as.numeric(Length))
  
  ggplot(data=SizeSelex_All2) +
    geom_line(aes(x=Length,y=Selectivity,color=Model,linetype=Time),size=1) +
    facet_wrap(~Fleet) +
    theme_bw(15)
  
  }