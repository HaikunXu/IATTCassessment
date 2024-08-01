#' Fishing mortality by age group
#' 
#' \code{faa} This function computes the fishing mortality by age group for the stock assessment report 
#' 
#' @export

faa = function(Dir, Last_Year, xlim, ylim) {
    
    myreplist = r4ss::SS_output(dir = Dir, covar = F, verbose = FALSE, printstats = FALSE)
    
    Z <- myreplist$Z_at_age
    M <- myreplist$M_at_age

    F_M <- Z
    F_M[, 4:43] <- Z[, 4:43] - data.matrix(M[, 4:43])
    
    F_Matrix <- F_M %>% gather("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
        "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
        "34", "35", "36", "37", "38", "39", "40", key = "Age", value = "FAA")
    F_Matrix$Age <- as.numeric(F_Matrix$Age)
    F_Matrix$Year2 <- ceiling(F_Matrix$Yr/4) + 1974
    
    # F_Matrix <- na.omit(F_Matrix %>% mutate('Group'=cut(Age, breaks = c(-1,4,8,12,20,39))))
    
    F_vector <- F_Matrix %>% group_by(Sex, Year2, Age) %>% summarise(F_annual = sum(FAA)) %>% mutate(Age = cut(Age, 
        breaks = c(0, 4, 8, 12, 19, 40), labels = c("1-4", "5-8", "9-12", "13-19", "20+")))
    
    F_vector <- na.omit(F_vector)
    
    F_vector <- F_vector %>% group_by(Age, Year2) %>% summarise(F_group = mean(F_annual))
    
    # spread(key = Group, value = F_group)
    
    f <- ggplot(data = F_vector %>% filter(Year2 <= Last_Year)) + geom_line(aes(x = Year2, y = F_group, color=Age)) + 
    theme_bw(20) + ylab("Average annual F") + xlab("Year") + coord_cartesian(xlim=xlim,ylim=ylim,expand=FALSE)
    
    ggsave(f,file = paste0(Dir, "faa.png"), width = 6, height = 4)
    ggsave(f,file = paste0(Dir, "faa.eps"), width = 6, height = 4)
    
    return(f)
    
}
