#' Diagnostics for SS cpue fit
#' 
#' \code{cpue_fit} This function provides cue diagnotics
#' 
#' @export
#' 

cpue_fit <- function(Path, s) {
    myreplist = SS_output(dir = Path, ncols = 400, covar = F, printstats = F, verbose = F)
    cpue <- myreplist$cpue
    n_fleet <- length(unique(cpue$Fleet))
    # ggplot(data=cpue) + geom_violin(aes(x=factor(Fleet),y=Dev))
    rmse <- cpue %>% group_by(Fleet) %>% summarise(RMSE = sqrt(mean((Dev/SE)^2)))
    
    f <- ggplot(data = cpue) +
        # geom_errorbar(aes(x = Time, ymin = -SE, ymax = SE), color = "black", alpha= 0.5) +
        geom_point(aes(x = Time, y = Dev/SE, color = factor(Fleet))) + geom_text(aes(x = -Inf, 
        y = -Inf, label = paste0("RMSE = ",round(RMSE, 2))), data = rmse, hjust = -0.5, vjust = -1) + geom_smooth(aes(x = Time, y = Dev/SE, 
        color = factor(Fleet), fill = factor(Fleet)), span = s, se = FALSE) + facet_wrap(~Fleet, nrow = n_fleet) + geom_hline(yintercept = 0) +
        theme_bw(12) + ylab("CPUE residual / CPUE std error") + xlab("Year")
    
    ggsave(f, file = paste0(Path, "CPUE_Fit1.png"), width = 8, height = 6)
    
    cpue <- cpue %>% mutate(Quarter = (Time - 1)%%4 + 1)
    
    f <- ggplot(data = cpue) + geom_boxplot(aes(x = factor(Fleet), y = Dev, color = factor(Quarter))) + geom_hline(yintercept = 0) + 
        theme_bw(12) + ylab("CPUE residual")
    
    ggsave(f, file = paste0(Path, "CPUE_Fit2.png"), width = 6, height = 6)
    
}
