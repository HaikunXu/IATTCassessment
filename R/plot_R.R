#' Plot quarterly and annual recruitment
#' 
#' \code{plot_R} This function plot quarterly and annual recruitment for the stock assessment report 
#' 
#' @export

plot_R = function(SS_Dir, lyear, Save_Dir) {
    
    cor_mat <- read.table(paste0(SS_Dir, "ss3.cor"), skip = 1, fill = NA, header = TRUE)
    R_est <- cor_mat$value[which(cor_mat$name == "recr_std")[3:((lyear - 1974) * 4 + 2)]]
    R_std <- cor_mat$std_dev[which(cor_mat$name == "recr_std")[3:((lyear - 1974) * 4 + 2)]]
    R <- data.frame(est = R_est, std = R_std, year = rep(1975:lyear, each = 4), yq = seq(1975, lyear + 0.75, 0.25))
    R_annual <- R %>% group_by(year) %>% summarise(Est = sum(est), Std = NA)
    # cov_mat <- matrix(NA, nrow = length(R_est), ncol = 4)
    index_init <- which(cor_mat$name == "recr_std")[3]
    
    for (y in 1975:lyear) {
        index_year <- 0:3 + index_init + (y - 1975) * 4
        cor_y <- cor_mat[index_year, index_year + 4]
        var_y <- R$std[((y - 1975) * 4 + 1):((y - 1974) * 4)] %*% t(R$std[((y - 1975) * 4 + 1):((y - 1974) * 4)]) * cor_y
        var_y[1, 2:4] <- var_y[2:4, 1]
        var_y[2, 3:4] <- var_y[3:4, 2]
        var_y[3, 4] <- var_y[4, 3]
        var_y <- data.matrix(var_y)
        # cov_mat[((y-1975)*4+1):((y-1974)*4),1:4] <- var_y
        R_annual$Std[which(R_annual$year == y)] <- sqrt(matrix(c(1, 1, 1, 1), nrow = 1, ncol = 4) %*% var_y %*% matrix(c(1, 
            1, 1, 1), nrow = 4, ncol = 1))
    }
    
    R <- R %>% mutate(R = est/mean(est), STD = std/est)
    R_annual <- R_annual %>% mutate(R = Est/mean(Est), STD = Std/Est)
    
    f1 <- ggplot(data = R) + geom_ribbon(aes(x = yq, ymin = R * exp(-1.96 * STD), ymax = R * exp(1.96 * STD)), fill = "grey") + 
        geom_line(aes(x = yq, y = R), size = 1) + geom_point(aes(x = yq, y = R), data = R[seq(1, nrow(R), 4), ]) + theme_bw(12) + 
        xlab("") + ylab("") + geom_hline(yintercept = 1, linetype = "dashed")
    
    f2 <- ggplot(data = R_annual) + geom_ribbon(aes(x = year, ymin = R * exp(-1.96 * STD), ymax = R * exp(1.96 * STD)), fill = "grey") + 
        geom_line(aes(x = year, y = R), size = 1) + geom_point(aes(x = year, y = R)) + theme_bw(12) + xlab("") + ylab("") + 
        geom_hline(yintercept = 1, linetype = "dashed")
    
    f_all <- grid.arrange(f1, f2, nrow = 2)
    ggsave(f_all, file = paste0(Save_Dir, "R.png"), width = 6, height = 8)
    ggsave(f_all, file = paste0(Save_Dir, "R.eps"), width = 6, height = 8)
    
}
