#' Fishing mortality by age group
#' 
#' \code{faa} This function computes the fishing mortality by age group for the stock assessment report 
#' 
#' @export

faa = function(myreplist, Last_Year, Species, Save_Dir) {
  
# myreplist = SS_output(dir=SS_Path,ncols=400,covar=F)

Z <- myreplist$Z_at_age
M_Matrix <- rbind(matrix(rep(data.matrix(BET_M[1,]),nrow(Z)/2),nrow=nrow(Z)/2,byrow = T),
                  matrix(rep(data.matrix(BET_M[2,]),nrow(Z)/2),nrow=nrow(Z)/2,byrow = T))

F_M <- Z
F_M[,4:44] <- Z[,4:44] - M_Matrix

F_Matrix <- F_M %>% gather("0","1","2","3","4","5","6","7","8","9","10",
                                            "11","12","13","14","15","16","17","18","19","20",
                                            "21","22","23","24","25","26","27","28","29","30",
                                            "31","32","33","34","35","36","37","38","39","40",
                                            key="Age",value="FAA")
F_Matrix$Age <- as.numeric(F_Matrix$Age)
F_Matrix$Year2 <- ceiling(F_Matrix$Year/4)+1974

# F_Matrix <- na.omit(F_Matrix %>% mutate("Group"=cut(Age, breaks = c(-1,4,8,12,20,39))))

F_vector <- F_Matrix %>% group_by(Gender,Year2,Age) %>% summarise(F_annual=sum(FAA)) %>%
  mutate("Group"=cut(Age, breaks = c(0,4,8,12,29,39))) 

F_vector <- na.omit(F_vector)

F_vector <- F_vector %>% group_by(Group,Year2) %>% summarise(F_group=mean(F_annual))

  # spread(key = Group, value = F_group)

ggplot(data=F_vector %>% filter(Year2<=Last_Year)) +
  geom_line(aes(x=Year2,y=F_group)) +
  facet_wrap(~Group,nrow=5) +
  theme_bw(12) +
  ylab("F") + xlab("Year")

ggsave(file=paste0(Save_Dir,"faa.png"),width = 8, height = 8)
ggsave(file=paste0(Save_Dir,"faa.eps"),width = 8, height = 8)

}