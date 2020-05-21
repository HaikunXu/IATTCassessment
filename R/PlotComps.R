#' Plot fisheries comps
#' 
#' /code{PlotComps} This function plot fisheries aggergated comps
#' 
#' @export

PlotComps <- function(Path,Save_Path,Fleets,label)
{
  #Fleets are all yhje fleets you want to plot
  #IncludeF are the fleets which data was used in the objective function
  
  ####5/16/2020
  
  Rep <- r4ss::SS_output(dir = Path, ncols = 400, covar = T, printstats = FALSE, verbose = FALSE)
  FleetNames<-Rep$FleetNames[Fleets]
  NewName <- data.frame(Fleet=1:23,Fishery2=paste0("F",fisheries," (",FleetNames,")"),Gear=label)
  
  LenComps <- Rep$lendbase %>% filter(Fleet %in% Fleets,Sex==1) %>%
    group_by(Fleet,Bin) %>% summarise(Comp=mean(Obs))
  
  LenComps <- left_join(LenComps,NewName) %>% mutate(Fishery2=factor(Fishery2, levels = NewName$Fishery2))

  f <- ggplot(data=LenComps)+geom_ribbon(aes(x=Bin,ymin=0,ymax=Comp,fill=Gear))+theme_bw(16) +
    facet_wrap(~Fishery2) + ylab("Mean length freqeuncy") +
    scale_fill_manual(values=c("red","blue"))
  
  ggsave(f, file = paste0(Save_Path, "comps.png"), width = 10, height = 10)
  ggsave(f, file = paste0(Save_Path, "comps.eps"), width = 10, height = 10)
  
  return(f)
  
}