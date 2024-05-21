#' Plot ASPM results
#' 
#' \code{plot_ASPM} This function plots quarterly SBR for the stock assessment report 
#' 
#' @export

plot_ASPM2 = function(SS_Dir, lyear, fyear, legend, Save_Dir, ymax, figure_name, title, xlim, alpha) {
  for (i in 1:length(lyear)) {
    
    skip_to_next <- FALSE
    tryCatch({
      cor_mat <- read.table(paste0(SS_Dir[i], "ss3.std"), skip = 1, fill = NA, header = FALSE)
    }, error = function(e) {skip_to_next <<- TRUE})
    if(skip_to_next) {next}
    
    names(cor_mat) <- c("index","name","value","std.dev")
    SB_est <- cor_mat$value[which(cor_mat$name == "SSB_std")[2:((lyear[i] - (fyear[i]-1)) * 4 + 1)]]
    SB_std <- cor_mat$std.dev[which(cor_mat$name == "SSB_std")[2:((lyear[i] - (fyear[i]-1)) * 4 + 1)]]
    # if(sum(SB_std)==0) SB_std <- cor_mat$std_dev[which(cor_mat$name == "depletion")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
    SB <- data.frame(est = SB_est, std = SB_std, year = rep(fyear[i]:lyear[i], each = 4), yq = seq(fyear[i], lyear[i] + 0.75, 0.25))
    
    if(i==1) SB_A <- SB %>% mutate(model=legend[i])
    else SB_A <- rbind(SB_A,SB %>% mutate(model=legend[i]))
  }
  
  SB_A <- SB_A %>% data.frame() %>% mutate(model=factor(model))
  
  f <- ggplot(data = SB_A) + 
    geom_ribbon(aes(x = yq, ymin = est - 1.96 * std, ymax = est + 1.96 * std, color = model), alpha=alpha, data=SB_A) + 
    geom_line(aes(x = yq, y = est, color = model), size = 1) + 
    geom_point(aes(x = yq, y = est, color = model), size = 1.5,data = SB_A %>% filter(yq==year)) + 
    theme_bw(20) + xlab("") + ylab("") + geom_hline(yintercept = 1, linetype = "dashed") +
    # coord_cartesian(ylim = c(0,ymax),xlim=xlim,expand = FALSE)+
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(f, file = paste0(Save_Dir, figure_name, "-SBR2.png"), width = 12, height = 8)
  # ggsave(f, file = paste0(Save_Dir, figure_name, "-SBR2.edf"), width = 12, height = 8)
  
  return(SB_A)
  
}