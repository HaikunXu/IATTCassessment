#' Plot quarterly SB
#' 
#' \code{plot_SB} This function plots quarterly SB for the stock assessment report 
#' 
#' @export

plot_SB = function(SS_Dir, lyear, fyear, legend, Save_Dir, ymax, figure_name = "", title = "", xlim, alpha = 0.1) {
    for (i in 1:length(lyear)) {
        cor_mat <- read.table(paste0(SS_Dir[i], "ss3.std"), skip = 1, fill = NA, header = FALSE)
        names(cor_mat) <- c("index","name","value","std.dev")
        SB_est <- cor_mat$value[which(cor_mat$name == "SSB_std")[3:((lyear[i] - (fyear[i]-1)) * 4 + 3)]]
        SB_std <- cor_mat$std.dev[which(cor_mat$name == "SSB_std")[3:((lyear[i] - (fyear[i]-1)) * 4 + 3)]]
        # if(sum(SB_std)==0) SB_std <- cor_mat$std_dev[which(cor_mat$name == "SSB_std")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
        SB <- data.frame(est = SB_est, std = SB_std, 
                         year = c(rep(fyear[i]:lyear[i], each = 4), lyear[i] + 1),
                         yq = seq(fyear[i], lyear[i] + 1, 0.25))
        
        if(i==1) SB_A <- SB %>% mutate(Spec=legend[i])
        else SB_A <- rbind(SB_A,SB %>% mutate(Spec=legend[i]))
    }
    
    SB_A <- SB_A %>% data.frame()
    
    f <- ggplot(data = SB_A) +
      geom_ribbon(aes(
        x = yq,
        ymin = est - 1.96 * std,
        ymax = est + 1.96 * std,
        fill = Spec
      ),
      alpha = alpha) +
      geom_line(aes(x = yq, y = est, color = Spec), size = 1) +
      geom_point(aes(x = yq, y = est, color = Spec),
                 size = 2,
                 data = SB_A %>% filter(yq == year)) +
      theme_bw(20) + xlab("") + ylab("") +
      coord_cartesian(ylim = c(0, ymax),
                      xlim = xlim,
                      expand = FALSE) +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(f, file = paste0(Save_Dir, figure_name, "-SB.png"), width = 12, height = 8)
    ggsave(f, file = paste0(Save_Dir, figure_name, "-SB.pdf"), width = 12, height = 8)
    
    return(SB_A)
    
}