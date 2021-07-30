#' Fishing mortality by age group
#' 
#' \code{faa_a} This function computes the fishing mortality by age group for the stock assessment report 
#' 
#' @export

Dir <- c("C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/R-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/RG-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/RM-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/RS-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/G-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/M1-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/M2-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/S-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/L-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/LG-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/LM-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/LS-1/",
                 "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/DW-1/")
Save_Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/"
Model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
Last_Year <- 2019
xlim <- c(1978,2021)
ylim <- c(0,1)

faa_a = function(Dir, Model, Last_Year, xlim, ylim) {
  
  for (i in 1:length(Model)) {
    print(i)
      myreplist = r4ss::SS_output(dir = Dir[i], ncols = 500, covar = F, verbose = FALSE, printstats = FALSE)
      
      Z <- myreplist$Z_at_age
      M <- myreplist$M_at_age
      # M_Matrix <- rbind(matrix(rep(data.matrix(M[1, ]), nrow(Z)/2), nrow = nrow(Z)/2, byrow = T), matrix(rep(data.matrix(M[2, 
      #     ]), nrow(Z)/2), nrow = nrow(Z)/2, byrow = T))
      
      F_M <- Z
      F_M[, 4:43] <- Z[, 4:43] - data.matrix(M[, 4:43])
      
      F_Matrix <- F_M %>% gather("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                                 "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                                 "34", "35", "36", "37", "38", "39", "40", key = "Age", value = "FAA")
      F_Matrix$Age <- as.numeric(F_Matrix$Age)
      F_Matrix$Year2 <- ceiling(F_Matrix$Yr/4) + 1974
      
      # F_Matrix <- na.omit(F_Matrix %>% mutate('Group'=cut(Age, breaks = c(-1,4,8,12,20,39))))
      
      F_vector <- F_Matrix %>% group_by(Sex, Year2, Age) %>% summarise(F_annual = sum(FAA)) %>% mutate(Age = cut(Age, 
                                                                                                                 breaks = c(0, 4, 8, 12, 19, 40), labels = c("1-4 quarters", "5-8 quarters", "9-12 quarters", "13-19 quarters", "20+ quarters")))
      F_vector <- na.omit(F_vector)
      
      if(i==1) {
        FAA <- F_vector %>% group_by(Age, Year2) %>% summarise(F_group = mean(F_annual)) %>% mutate(Model=Model[i])
      } else {
        FAA <- rbind(FAA, F_vector %>% group_by(Age, Year2) %>% summarise(F_group = mean(F_annual)) %>% mutate(Model=Model[i]))
      }
    }
    
  Weight <- data.frame("Model"=Model,"Weight"=c(0.01,0.13,0.02,0.05,0.24,0.01,0.02,0.09,0.04,0.22,0.07,0.11))
  data <- left_join(FAA,Weight) %>% group_by(Age,Year2) %>%
    mutate(Weight2=Weight/sum(Weight))
  Data <- data %>% filter(Year2 <= Last_Year) %>% group_by(Age,Year2) %>%
    mutate(Combined=sum(F_group*Weight2))
  
  
  f <- ggplot(data = Data) +
    geom_line(aes(x = Year2, y = F_group, color=Model)) +
    geom_line(aes(x = Year2, y = Combined),data=Data %>% filter(Year2<2000),size=1.5) +
    geom_line(aes(x = Year2, y = Combined),data=Data %>% filter(Year2>1999),size=1.5) +
    facet_wrap(~Age,nrow=5) +
    theme_bw(20) + ylab("Average annual F") + xlab("Year") +
    coord_cartesian(xlim=xlim,ylim=ylim,expand=FALSE)
  
  ggsave(f,file = paste0(Save_Dir, "faa_a.png"), width = 12, height = 15)
  
  f <- ggplot(data = Data) +
    geom_line(aes(x = Year2, y = F_group, color=Model,alpha=Weight),size=1) +
    geom_line(aes(x = Year2, y = Combined),data=Data %>% filter(Year2<2000),size=1.5) +
    geom_line(aes(x = Year2, y = Combined),data=Data %>% filter(Year2>1999),size=1.5) +
    facet_wrap(~Age,nrow=5) +
    theme_bw(20) + ylab("Average annual F") + xlab("Year") +
    coord_cartesian(xlim=xlim,ylim=ylim,expand=FALSE)
  
  ggsave(f,file = paste0(Save_Dir, "faa_a2.png"), width = 12, height = 15)

}
