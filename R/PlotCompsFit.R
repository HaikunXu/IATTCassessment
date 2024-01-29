#' Plot the residual of mean length for each fleet
#' 
#' \code{PlotCompsFit} This function plots the residual of mean length for each fleet
#' 
#' @export

PlotCompsFit <- function(Path, fleets_len, fleets_size, span){
  
  Rep <- r4ss::SS_output(dir = Path,covar = F,verbose = FALSE, printstats = FALSE)
  
  # check length comps' residual pattern
  length_fit <- Rep$len_comp_fit_table %>%
    data.frame() %>%
    filter(Fleet %in% fleets_len) %>%
    mutate(Year = as.numeric(Yr)/4 + 1974.874)
  
  f1 <- ggplot(data=length_fit) +
    geom_hline(yintercept = 0) +
    geom_point(aes(x=Year, y=All_delta/All_exp_mean)) +
    geom_smooth(aes(x=Year, y=All_delta/All_exp_mean,weight = Nsamp_adj),span = span) +
    facet_wrap(~Fleet_Name) +
    ylab("% bias in mean length") +
    theme_bw(14)
  ggsave(f1, file=paste0(Path1,"bias_meanL_PS.png"),width=9,height=9)
  
  # check size comps' residual pattern
  size_fit <- Rep$size_comp_fit_table %>%
    data.frame() %>%
    filter(Fleet %in% fleets_size) %>%
    mutate(Year = as.numeric(Yr)/4 + 1974.874)
  
  f2 <- ggplot(data=size_fit) +
    geom_hline(yintercept = 0) +
    geom_point(aes(x=Year, y=All_delta/All_exp_mean)) +
    geom_smooth(aes(x=Year, y=All_delta/All_exp_mean,weight = Nsamp_adj),span = span) +
    facet_wrap(~Fleet_Name) +
    ylab("% bias in mean length") +
    theme_bw(14)
  ggsave(f2, file=paste0(Path,"bias_meanL_LL.png"),width=9,height=9)
  
}

