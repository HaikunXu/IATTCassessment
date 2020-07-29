#' Compare cpue from different VAST models
#' 
#' \code{cpue_compare} This function plot the comaprison of standardized cpue from different VAST models
#' 
#' @export
#' 

cpue_compare <- function(Path, Legend, Save_Path, rescale, ylabel, xlabel, ylim, xlim, CV) {
    index <- read.csv(paste0(Path[1],"Table_for_SS3.csv"))
    Index <- data.frame("Year"=index$Year/4+1974.75,"Index"=index$Estimate_metric_tons,"Legend"=Legend[1],"CV"=index$SD_log)

    if(length(Legend)>1) {
        for (i in 2:length(Path)) {
            index <- read.csv(paste0(Path[i],"Table_for_SS3.csv"))
            Index <- rbind(Index,data.frame("Year"=index$Year/4+1974.75,"Index"=index$Estimate_metric_tons,"Legend"=Legend[i],"CV"=index$SD_log))
        }
        }
    
    Index <- Index %>% mutate(Legend=factor(Legend))
    
    if(rescale==TRUE) Index <- Index %>% group_by(Legend) %>% mutate(Index=Index/mean(Index))
    
    if(CV==TRUE) {
        f <- ggplot(data = Index) + geom_line(aes(x = Year, y = Index, color = Legend)) + coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
            theme_bw(15) + ylab(ylabel) + xlab(xlabel) + geom_point(aes(x = Year, y = Index, color = Legend)) +
            geom_ribbon(aes(x = Year, ymin = Index * exp(-1.96 * CV), ymax = Index * exp(1.96 * CV), fill = Legend), alpha=0.2) +
            labs(color="Index",fill="Index") 
    }
    else {
        f <- ggplot(data = Index) + geom_line(aes(x = Year, y = Index, color = Legend)) + coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
            theme_bw(15) + ylab(ylabel) + xlab(xlabel) + geom_point(aes(x = Year, y = Index, color = Legend)) +
            labs(color="Index",fill="Index") 
    }
        
    
    if(rescale==TRUE) {
        if(CV==TRUE) {
            f <- ggplot(data = Index) + geom_line(aes(x = Year, y = Index, color = Legend)) +
                theme_bw(15) + geom_hline(yintercept=1) + ylab(ylabel) + coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
                geom_point(aes(x = Year, y = Index, color = Legend)) +
                geom_ribbon(aes(x = Year, ymin = Index * exp(-1.96 * CV), ymax = Index * exp(1.96 * CV), fill = Legend), alpha=0.2) +
                labs(color="Index",fill="Index")
        }
        else {
            f <- ggplot(data = Index) + geom_line(aes(x = Year, y = Index, color = Legend)) +
                theme_bw(15) + geom_hline(yintercept=1) + ylab(ylabel) + coord_cartesian(ylim = ylim, xlim=xlim, expand = FALSE) +
                geom_point(aes(x = Year, y = Index, color = Legend)) +
                labs(color="Index",fill="Index")   
        }

    }
    
    # ggsave(f, file = paste0(Save_Path, "CPUE_Compare.png"), width = 15, height = 6)
    # ggsave(f, file = paste0(Save_Path, "CPUE_Compare.eps"), width = 15, height = 6, device=cairo_ps)
    # 
    return(f)
}
