#' Compare cpue from different VAST models
#' 
#' \code{cpue_compare} This function plot the comaprison of standardized cpue from different VAST models
#' 
#' @export
#' 

cpue_compare <- function(Path, Legend, Save_Path) {
    index <- read.csv(paste0(Path[1],"Table_for_SS3.csv"))
    Index <- data.frame(matrix(NA,nrow=nrow(index),ncol=length(Path)+1))
    names(Index) <- c("Year",Legend)
    Index$Year <- index$Year
    Index[,2] <- index$Estimate_metric_tons
    
    for (i in 2:length(Path)) {
        index <- read.csv(paste0(Path[i],"Table_for_SS3.csv"))
        Index[,i+1] <- index$Estimate_metric_tons
    }  
    
    Index <- Index %>% gather(Legend,key=Model,value=cpue) %>% mutate(Model=factor(Model)) %>%
        group_by(factor(Model)) %>%
        mutate(CPUE=cpue/mean(cpue))
    
    f <- ggplot(data = Index) + geom_line(aes(x = Year, y = CPUE, color = Model)) +
        theme_bw(12) + geom_hline(yintercept=1)
    
    ggsave(f, file = paste0(Save_Path, "CPUE_Compare.png"), width = 12, height = 6)
}
