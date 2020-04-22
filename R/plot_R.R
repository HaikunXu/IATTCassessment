#' Plot quarterly and annual recruitment
#' 
#' \code{plot_R} This function plot quarterly and annual recruitment for the stock assessment report 
#' 
#' @export

plot_R = function(SS_Dir, lyear, fyear, legend, Save_Dir, ymax, figure_name, title) {
    for (i in 1:length(lyear)) {
        cor_mat <- read.table(paste0(SS_Dir[i], "ss.cor"), skip = 1, fill = NA, header = TRUE)
        R_est <- cor_mat$value[which(cor_mat$name == "recr_std")[3:((lyear[i] - (fyear[i]-1)) * 4 + 2)]]
        R_std <- cor_mat$std.dev[which(cor_mat$name == "recr_std")[3:((lyear[i] - (fyear[i]-1)) * 4 + 2)]]
        if(sum(R_std)==0) R_std <- cor_mat$std_dev[which(cor_mat$name == "recr_std")[3:((lyear[i] - (fyear[i]-1)) * 4 + 2)]]
        R <- data.frame(est = R_est, std = R_std, year = rep(fyear[i]:lyear[i], each = 4), yq = seq(fyear[i], lyear[i] + 0.75, 0.25))
        R_annual <- R %>% group_by(year) %>% summarise(Est = sum(est), Std = NA)
        # cov_mat <- matrix(NA, nrow = length(R_est), ncol = 4)
        index_init <- which(cor_mat$name == "recr_std")[3]
        
        for (y in fyear[i]:lyear[i]) {
            index_year <- 0:3 + index_init + (y - fyear[i]) * 4
            cor_y <- cor_mat[index_year, index_year + 4]
            var_y <- R$std[((y - fyear[i]) * 4 + 1):((y - (fyear[i]-1)) * 4)] %*% t(R$std[((y - fyear[i]) * 4 + 1):((y - (fyear[i]-1)) * 4)]) * 
                cor_y
            var_y[1, 2:4] <- var_y[2:4, 1]
            var_y[2, 3:4] <- var_y[3:4, 2]
            var_y[3, 4] <- var_y[4, 3]
            var_y <- data.matrix(var_y)
            # cov_mat[((y-1975)*4+1):((y-1974)*4),1:4] <- var_y
            R_annual$Std[which(R_annual$year == y)] <- sqrt(matrix(c(1, 1, 1, 1), nrow = 1, ncol = 4) %*% var_y %*% matrix(c(1, 
                                                                                                                             1, 1, 1), nrow = 4, ncol = 1))
        }
        
        R_quarterly <- R %>% mutate(R = est/mean(est), STD = std/est, Model=legend[i])
        R_annual <- R_annual %>% mutate(R = Est/mean(Est), STD = Std/Est, Model=legend[i])
        
        if(i==1) {
            R_Q <- R_quarterly
            R_A <- R_annual
        }
        else
        {
            R_Q <- rbind(R_Q,R_quarterly)
            R_A <- rbind(R_A,R_annual)
        }
    }
    
    R_Q <- R_Q %>% data.frame() %>% mutate(Model=factor(Model))
    R_A <- R_A %>% data.frame() %>% mutate(Model=factor(Model))
    
    f1 <- ggplot(data = R_Q) + geom_ribbon(aes(x = yq, ymin = R * exp(-1.96 * STD), ymax = R * exp(1.96 * STD), fill = Model), alpha=0.1) + 
        geom_line(aes(x = yq, y = R, color = Model), size = 1) + geom_point(aes(x = yq, y = R, color = Model),size=3,data = R_Q %>% filter(yq==year)) +
        theme_bw(20) + xlab("") + ylab("") + geom_hline(yintercept = 1, linetype = "dashed") +
        coord_cartesian(ylim = c(0,ymax[1]),expand = FALSE) + ggtitle(title)
    
    f2 <- ggplot(data = R_A) + geom_ribbon(aes(x = year, ymin = R * exp(-1.96 * STD), ymax = R * exp(1.96 * STD), fill = Model), alpha=0.1) +
        geom_line(aes(x = year, y = R, color = Model), size = 1) + geom_point(aes(x = year, y = R, color = Model),size=3) +
        theme_bw(20) + xlab("") + ylab("") + geom_hline(yintercept = 1, linetype = "dashed") +
        coord_cartesian(ylim = c(0,ymax[2]),expand = FALSE) + ggtitle(title)
    
    f_all <- gridExtra::grid.arrange(f1, f2, nrow = 2)
    ggsave(f_all, file = paste0(Save_Dir, figure_name, "-R.png"), width = 12, height = 15,device=cairo_ps)
    ggsave(f_all, file = paste0(Save_Dir, figure_name, "-R.eps"), width = 12, height = 15,device=cairo_ps)
    
    return(f2)
    
}
