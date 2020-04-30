#' Plot quarterly and annual recruitment
#' 
#' \code{plot_Rdev} This function plot quarterly and annual recruitment for the stock assessment report 
#' 
#' @export

plot_Rdev = function(SS_Dir, lyear, fyear, legend, Save_Dir, ymax, figure_name, xlim) {
    for (i in 1:length(lyear)) {
        cor_mat <- read.table(paste0(SS_Dir[i], "ss.cor"), skip = 1, fill = NA, header = TRUE)
        R_est <- cor_mat$value[which(cor_mat$name == "recdev1")]#[3:((lyear[i] - (fyear[i]-1)) * 4 + 2)]]
        R <- data.frame(est = R_est, year = rep(fyear[i]:lyear[i], each = 4), yq = seq(fyear[i], lyear[i] + 0.75, 0.25))

        R_quarterly <- R %>% mutate(R = est, Model=legend[i])

        if(i==1) R_Q <- R_quarterly
        else R_Q <- rbind(R_Q,R_quarterly)
    }
    
    R_Q <- R_Q %>% data.frame() %>% mutate(Model=factor(Model))

    f1 <- ggplot(data = R_Q) +# geom_ribbon(aes(x = yq, ymin = R * exp(-1.96 * STD), ymax = R * exp(1.96 * STD), fill = Model), alpha=0.1) + 
        geom_line(aes(x = yq, y = R, color = Model), size = 0.8) +
        theme_bw(20) + xlab("") + ylab("") + geom_hline(yintercept = 0, linetype = "dashed") +
        coord_cartesian(ylim = c(-ymax,ymax),xlim=xlim,expand = FALSE)
    
    ggsave(f1, file = paste0(Save_Dir, figure_name, "-Rdev.png"), width = 15, height = 10)
    ggsave(f1, file = paste0(Save_Dir, figure_name, "-Rdev.eps"), width = 15, height = 10,device=cairo_ps)
    
    return(f1)
}
