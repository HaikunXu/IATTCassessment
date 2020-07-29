#' Diagnostics for SS cpue fit
#' 
#' \code{cpue_fit} This function provides cue diagnotics
#' 
#' @export
#' 

cpue_fit <- function(Path, s, title, Fleets, xlim, ylim) {
    myreplist <- r4ss::SS_output(dir = Path, ncols = 400, covar = T, printstats = F, verbose = FALSE)
    
    cpue <- myreplist$cpue %>% filter(Fleet %in% Fleets) %>% mutate(Fleet=factor(Fleet))
    n_fleet <- length(Fleets)
    # ggplot(data=cpue) + geom_violin(aes(x=factor(Fleet),y=Dev))
    rmse <- cpue %>% group_by(Fleet) %>% summarise(RMSE = sqrt(mean((Dev/SE)^2)))
    
    f1 <- ggplot(data = cpue) +
        # geom_errorbar(aes(x = Time, ymin = -SE, ymax = SE), color = "black", alpha= 0.5) +
        geom_point(aes(x = Time/4+1974.75, y = Dev/SE, color = factor(Fleet))) + geom_text(aes(x = -Inf, 
        y = -Inf, label = paste0("RMSE = ",round(RMSE, 2))), data = rmse, hjust = -0.5, vjust = -1) + geom_smooth(aes(x = Time/4+1974.75, y = Dev/SE, 
        color = factor(Fleet), fill = factor(Fleet)), span = s, se = FALSE) + facet_wrap(~Fleet, nrow = n_fleet) + geom_hline(yintercept = 0) +
        theme_bw(12) + ylab("CPUE residual / CPUE std error") + xlab("Year") +
        ggtitle(title) + ggeasy::easy_center_title()
    
    ggsave(f1, file = paste0(Path, "CPUE_Fit1.png"), width = 6, height = 6)
    
    cpue <- cpue %>% mutate(Quarter = (Yr - 1)%%4 + 1)
    
    f2 <- ggplot(data = cpue) + geom_boxplot(aes(x = factor(Fleet), y = Dev/SE, color = factor(Quarter))) + geom_hline(yintercept = 0) + 
        theme_bw(12) + ylab("CPUE residual / CPUE std error") + ggeasy::easy_center_title()
    
    ggsave(f2, file = paste0(Path, "CPUE_Fit2.png"), width = 6, height = 6)
    
    f3 <- ggplot(data=cpue) +
        geom_point(aes(x=Time/4+1974.75,y=Obs)) +
        geom_errorbar(aes(x=Time/4+1974.75,ymin=exp(log(Obs)-1.96*SE), ymax=exp(log(Obs)+1.96*SE)),alpha=0.5) +
        geom_line(aes(x=Time/4+1974.75,y=Exp,color=Fleet),size=1.2) +
        theme_bw(16) + ylab("") + xlab("")+ ggtitle(title) + ggeasy::easy_center_title() + 
        coord_cartesian(expand = FALSE,xlim=xlim,ylim) + theme(legend.position = "none")
    
    ggsave(f3, file = paste0(Path, "CPUE_Fit3.png"), width = 6, height = 6)
    
    
    RMSE <- list("f1"=f1,"f2"=f2,"f3"=f3,"rmse"=rmse)
    return(RMSE)
}

