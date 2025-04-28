#' Compare cpue from different VAST models
#' 
#' /code{cpue_compare} This function plot the comaprison of standardized cpue from different VAST models
#' 
#' @export
#' 

cpue_compare <- function(Path, Legend, Region, Save_Path, rescale = TRUE, ylabel = "CPUE", xlabel = "", ylim, xlim, CV = FALSE, w = 15, h = 8, smooth = FALSE, version = "New", s = 0.1, Figure_name = "CPUE_Compare", Year_first = 0, Year_last = 2500, Legend_name = "Index") {
  if(version == "Old") {
    index <- read.csv(paste0(Path[1],"Table_for_SS3.csv"))
    Index <- data.frame("Year"=index$Year,"Index"=index$Estimate_metric_tons,"Legend"=Legend[1],"CV"=index$SD_log, Fleet=Region[1])
    
    if(length(Legend)>1) {
      for (i in 2:length(Path)) {
        index <- read.csv(paste0(Path[i],"Table_for_SS3.csv"))
        Index <- rbind(Index,data.frame("Year"=index$Year,"Index"=index$Estimate_metric_tons,"Legend"=Legend[i],"CV"=index$SD_log, Fleet=Region[i]))
      }
    }
  }  
  else {
    index <- read.csv(paste0(Path[1],"Index.csv"))
    Index <- data.frame("Year"=index$Time,"Index"=index$Estimate,"Legend"=Legend[1],"CV"=index$Std..Error.for.ln.Estimate., Fleet=Region[1])
    
    if(length(Legend)>1) {
      for (i in 2:length(Path)) {
        index <- read.csv(paste0(Path[i],"Index.csv"))
        Index <- rbind(Index,data.frame("Year"=index$Time,"Index"=index$Estimate,"Legend"=Legend[i],"CV"=index$Std..Error.for.ln.Estimate., Fleet=Region[i]))
      }
    }
  }
  
  Index <- Index %>% mutate(Legend=factor(Legend),Region=Fleet,Year=Year/4+1974.875) %>%
    filter(Index > 0,
           Year >= Year_first,
           Year <= Year_last)
  write.csv(Index, file = paste0(Save_Path, "Index.csv"), row.names = FALSE)
  
  if(rescale==TRUE) Index <- Index %>% group_by(Legend,Fleet) %>% mutate(Index=Index/mean(Index))
  
  if(CV==TRUE) {
    f <- ggplot(data = Index) + geom_line(aes(x = Year, y = Index, color = Legend)) + coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
      theme_bw(15) + ylab(ylabel) + xlab(xlabel) + geom_point(aes(x = Year, y = Index, color = Legend)) +
      geom_ribbon(aes(x = Year, ymin = Index * exp(-1.96 * CV), ymax = Index * exp(1.96 * CV), fill = Legend), alpha=0.2) +
      labs(color=Legend_name,fill=Legend_name) +
      facet_wrap( ~ Region, nrow = length(unique(Region)))
  }
  else {
    f <- ggplot(data = Index) +
      # geom_line(aes(x = Year, y = Index, color = Legend)) +
      geom_smooth(aes(x = Year, y = Index, color = Legend)) +
      coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
      theme_bw(15) + ylab(ylabel) + xlab(xlabel) + 
      geom_point(aes(x = Year, y = Index, color = Legend)) +
      labs(color=Legend_name) +
      facet_wrap( ~ Region, nrow = length(unique(Region)))
  }
  
  
  if(rescale==TRUE) {
    if(CV==TRUE) {
      f <- ggplot(data = Index) + geom_line(aes(x = Year, y = Index, color = Legend)) +
        theme_bw(15) + geom_hline(yintercept=1) + ylab(ylabel) + coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
        geom_point(aes(x = Year, y = Index, color = Legend)) +
        geom_ribbon(aes(x = Year, ymin = Index * exp(-1.96 * CV), ymax = Index * exp(1.96 * CV), fill = Legend), alpha=0.2) +
        labs(color=Legend_name,fill=Legend_name) +
        facet_wrap( ~ Region, nrow = length(unique(Region)))
    }
    else {
      if(smooth == TRUE) {
        f <- ggplot(data = Index) +
          geom_smooth(aes(x = Year, y = Index, color = Legend), se = FALSE, span = s) +
          theme_bw(15) +
          geom_hline(yintercept=1, linetype = "dashed") +
          ylab(ylabel) + coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
          geom_point(aes(x = Year, y = Index, color = Legend), alpha = 0.5) +
          labs(color=Legend_name) +
          facet_wrap( ~ Region, nrow = length(unique(Region)))
      }
      else {
        f <- ggplot(data = Index) +
          geom_line(aes(x = Year, y = Index, color = Legend)) +
          theme_bw(15) +
          geom_hline(yintercept=1, linetype = "dashed") +
          ylab(ylabel) + coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
          geom_point(aes(x = Year, y = Index, color = Legend), alpha = 0.5) +
          labs(color=Legend_name) +
          facet_wrap( ~ Region, nrow = length(unique(Region)))
      }
      
    }
    
  }
  
  ggsave(f, file = paste0(Save_Path, Figure_name, ".png"), width = w, height = h)
  ggsave(f, file = paste0(Save_Path, Figure_name, ".pdf"), width = w, height = h)
  
  
  return(f)
}
