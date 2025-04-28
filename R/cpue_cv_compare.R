#' Compare cpue from different VAST models
#' 
#' /code{cpue_cv_compare} This function plot the comaprison of standardized cpue from different VAST models
#' 
#' @export
#' 

cpue_cv_compare <- function(Path, Legend, Region, Save_Path, ylabel = "CV", xlabel = "", ylim, xlim, w = 15, h = 8, Figure_name = "CPUE_CV_Compare", Legend_name = "CV") {

  index <- read.csv(paste0(Path[1],"Index.csv"))
  Index <- data.frame("Year"=index$Time,"Index"=index$Estimate,"Legend"=Legend[1],"CV"=index$Std..Error.for.ln.Estimate., Fleet=Region[1])
  
  if(length(Legend)>1) {
    for (i in 2:length(Path)) {
      index <- read.csv(paste0(Path[i],"Index.csv"))
      Index <- rbind(Index,data.frame("Year"=index$Time,"Index"=index$Estimate,"Legend"=Legend[i],"CV"=index$Std..Error.for.ln.Estimate., Fleet=Region[i]))
    }
  }
  
  Index <- Index %>% mutate(Legend=factor(Legend),Region=Fleet,Year=Year/4+1974.875) %>%
    filter(Index > 0)
  write.csv(Index, file = paste0(Save_Path, "Index_CV.csv"), row.names = FALSE)
  
  f <- ggplot(data = Index) +
    geom_line(aes(x = Year, y = CV, color = Legend)) +
    coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
    theme_bw(15) + ylab(ylabel) + xlab(xlabel) + 
    geom_point(aes(x = Year, y = CV, color = Legend)) +
    labs(color=Legend_name) +
    facet_wrap(~Region)
  
  ggsave(f, file = paste0(Save_Path, Figure_name, ".png"), width = w, height = h)
  ggsave(f, file = paste0(Save_Path, Figure_name, ".pdf"), width = w, height = h)
  
  return(f)
}