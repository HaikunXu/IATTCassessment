#' Diagnostics for SS cpue fit
#' 
#' \code{cpue_fit} This function provides cue diagnotics
#' 
#' @export
#' 

cpue_fit <- function(Path) {
    myreplist = SS_output(dir = Path, ncols = 400, covar = F, printstats = F, verbose = F)
    cpue <- myreplist$cpue
    n_fleet <- length(unique(cpue$Fleet))
    # ggplot(data=cpue) + geom_violin(aes(x=factor(Fleet),y=Dev))
    rmse <- cpue %>% group_by(Fleet) %>% summarise(RMSE = sqrt(mean(Dev^2)))
    
    f <- ggplot(data = cpue) + geom_point(aes(x = Time, y = Dev, color = factor(Fleet))) + geom_text(aes(x = -Inf, 
        y = -Inf, label = round(RMSE, 2)), data = rmse, hjust = -0.5, vjust = -1) + geom_smooth(aes(x = Time, 
        y = Dev, color = factor(Fleet), fill = factor(Fleet)), span = 0.25) + facet_wrap(~Fleet, 
        nrow = n_fleet) + ylim(c(-1, 1)) + geom_hline(yintercept = 0) + theme_bw(12) + ylab("CPUE residuals")
    
    ggsave(f, file = paste0(Path, "CPUE_Fit1.png"), width = 6, height = 10)
    
    cpue <- cpue %>% mutate(Quarter= (Time-1.5) %% 4 +1)
    
    f <- ggplot(data=cpue) +
      geom_boxplot(aes(x=factor(Fleet),y=Dev,color=factor(Quarter))) +
      geom_hline(yintercept = 0) + 
      theme_bw(12) +
      ylab("CPUE residuals") +
      ylim(c(-1, 1))
    
    ggsave(f, file = paste0(Path, "CPUE_Fit2.png"), width = 6, height = 6)
    
}
