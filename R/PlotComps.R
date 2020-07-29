#' Plot fisheries comps
#' 
#' /code{PlotFComps} This function plot fisheries aggergated comps
#' 
#' @export

PlotComps <- function(Path,Save_Path,Fleets,size,label,width=10,height=10)
{
  #Fleets are all the fleets you want to plot
  #IncludeF are the fleets which data was used in the objective function
  
  ####5/16/2020
  
  Rep <- r4ss::SS_output(dir = Path, ncols = 400, covar = T, printstats = FALSE, verbose = FALSE)
  FleetNames<-Rep$FleetNames[Fleets]
  LFleets <- Fleets[which(size==FALSE)]
  SFleets <- Fleets[which(size==TRUE)]
  
  LNewName <- data.frame(Fleet=LFleets,Fishery2=paste0("F",LFleets," (",FleetNames[LFleets],")"),Gear=label[LFleets])
  SNewName <- data.frame(Fleet=SFleets,Fishery2=paste0("S",SFleets," (",FleetNames[SFleets],")"),Gear=label[SFleets])
  
  NewName <- rbind(LNewName,SNewName) 
  
  LenComps <- Rep$lendbase %>% filter(Fleet %in% Fleets,Sex==1) %>%
    group_by(Fleet,Bin) %>% summarise(comp=mean(Obs*Nsamp_adj)) %>% 
    group_by(Fleet) %>% mutate(Comp=comp/sum(comp)/2,Bin=Bin+1)
  
  SizeComps <- Rep$sizedbase %>% filter(Fleet %in% Fleets,Sex==1) %>%
    group_by(Fleet,Bin) %>% summarise(comp=mean(Obs*Nsamp_adj)) %>% 
    group_by(Fleet) %>% mutate(Comp=comp/sum(comp)/10,Bin=Bin+5)
  
  comps <- rbind(LenComps,SizeComps)
  
  Comps <- left_join(comps,NewName) %>% mutate(Fishery2=factor(Fishery2, levels = NewName$Fishery2))
  
  f <- ggplot(data=Comps)+geom_ribbon(aes(x=Bin,ymin=0,ymax=Comp,fill=Gear))+theme_bw(16) +
    facet_wrap(~Fishery2) + ylab("Weighted length freqeuncy") + xlab("Length (cm)") +
    scale_fill_manual(values=c("red","blue"))
  
  ggsave(f, file = paste0(Save_Path, "Comps.png"), width = width, height = height)
  ggsave(f, file = paste0(Save_Path, "Comps.eps"), width = width, height = height)
  
  return(f)
  
}