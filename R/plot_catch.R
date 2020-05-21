#' Plot fisheries catch
#' 
#' /code{plot_catch} This function automayically plot catch time series by fishery
#' 
#' @export

plot_catch = function(Path, Save_Path, fisheries, label) {
  
  replist <- r4ss::SS_output(dir = Path, ncols = 400, covar = T, printstats = FALSE, verbose = FALSE)
  TimeSeries <- replist$timeseries
  FleetNames <- replist$FleetNames[fisheries]
  NewName <- data.frame(Fishery=1:23,Fishery2=paste0("F",fisheries," (",FleetNames,")"))
  
  Ccol <- which(substr(names(TimeSeries),start=1,stop=7)=="dead(B)")
  Crow <- which(TimeSeries$Era=="TIME")
  Catch <- TimeSeries[Crow,c(2,Ccol)]
  names(Catch) <- c("year",1:length(fisheries))
  Catch_final <- Catch %>% gather(names(Catch)[2:(length(fisheries)+1)],key=Fishery,value=catch) %>%
    mutate(Fishery=as.numeric(Fishery),Gear=label[Fishery],Year=ceiling(year/4)+1974) %>%
    group_by(Fishery,Year,Gear) %>% summarise(Catch=sum(catch))
  
  Catch_final <- left_join(Catch_final,NewName) %>% mutate(Fishery2=factor(Fishery2, levels = NewName$Fishery2))
  
  f1 <- ggplot(data=Catch_final) +
    geom_line(aes(x=Year,y=Catch,color=Gear)) +
    facet_wrap(~Fishery2,nrow=6) +
    theme_bw(15) + xlab("") + ylab("Annual catch by fishery") +
    scale_color_manual(values=c("red","blue"))
    
  ggsave(f1, file = paste0(Path, "catch1.png"), width = 12, height = 10)
  ggsave(f1, file = paste0(Path, "catch1.eps"), width = 12, height = 10)
  
  Catch_Gear <- Catch %>% gather(names(Catch)[2:(length(fisheries)+1)],key=Fishery,value=catch) %>%
    mutate(Fishery=as.numeric(Fishery),Gear=label[Fishery],Year=ceiling(year/4)+1974) %>%
    group_by(Year,Gear) %>% summarise(Catch=sum(catch)) %>% spread(Gear,Catch)
  
  f2 <- ggplot(data=Catch_Gear) +
    geom_ribbon(aes(x = Year, ymin = 0, ymax = PS), fill = "blue") +
    geom_ribbon(aes(x = Year, ymin = PS, ymax = PS + LL), fill = "red") +
    theme_bw(15) + xlab("") + ylab("Annual catch by gear")
 
  ggsave(f2, file = paste0(Save_Path, "catch2.png"), width = 12, height = 6)
  ggsave(f2, file = paste0(Save_Path, "catch2.eps"), width = 12, height = 6)
  
  # return(f)
}

