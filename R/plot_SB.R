#' Plot quarterly SBR
#' 
#' \code{plot_SB} This function plots quarterly SBR for the stock assessment report 
#' 
#' @export

plot_SB = function(SS_Dir, lyear, fyear, legend, Save_Dir, ymax, figure_name, title) {
    print("rename ss3.cor to ss.cor for runs not using SS 3.30!")
    for (i in 1:length(lyear)) {
        cor_mat <- read.table(paste0(SS_Dir[i], "ss.cor"), skip = 1, fill = NA, header = TRUE)
        SB_est <- cor_mat$value[which(cor_mat$name == "depletion")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
        SB_std <- cor_mat$std.dev[which(cor_mat$name == "depletion")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
        if(sum(SB_std)==0) SB_std <- cor_mat$std_dev[which(cor_mat$name == "depletion")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
        SB <- data.frame(est = SB_est, std = SB_std, year = rep(fyear[i]:lyear[i], each = 4), yq = seq(fyear[i], lyear[i] + 0.75, 0.25))
    
        if(i==1) SB_A <- SB %>% mutate(Model=legend[i])
        else SB_A <- rbind(SB_A,SB %>% mutate(Model=legend[i]))
    }
    
    SB_A <- SB_A %>% data.frame() %>% mutate(Model=factor(Model))
    
    f <- ggplot(data = SB_A) + geom_ribbon(aes(x = yq, ymin = est * exp(-1.96 * std), ymax = est * exp(1.96 * std), fill = Model), alpha=0.1) + 
        geom_line(aes(x = yq, y = est, color = Model), size = 1) + 
        geom_point(aes(x = yq, y = est, color = Model), size = 2,data = SB_A %>% filter(yq==year)) + 
        theme_bw(20) + xlab("") + ylab("") + geom_hline(yintercept = 1, linetype = "dashed") +
        coord_cartesian(ylim = c(0,ymax))+ ggtitle(title)

    ggsave(f, file = paste0(Save_Dir, figure_name, "-SB.png"), width = 12, height = 8)
    # ggsave(f_all, file = paste0(Save_Dir, "R.eps"), width = 6, height = 8)
    
    return(f)
    
}

