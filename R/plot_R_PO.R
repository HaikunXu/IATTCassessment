#' Plot quarterly and annual recruitment
#' 
#' \code{plot_R_Po} This function plot quarterly and annual recruitment for the stock assessment report 
#' 
#' @export

plot_R_PO = function(SS_Dir, legend, Save_Dir, area, figure_name) {
  for (i in 1:length(SS_Dir)) {
    print(i)
    myreplist = SS_output(dir=SS_Dir[i],ncols=1000,covar=F,forecast=FALSE,verbose = FALSE, printstats = FALSE)
    if(i==1) {
      R <- data.frame("year"=myreplist$timeseries$Yr,
                    "R"=myreplist$timeseries$Recruit_0,
                    "Year"=ceiling(myreplist$timeseries$Yr/4)+1953,
                    "Area"=myreplist$timeseries$Area,
                    "Model"=legend[i])
      SB <- data.frame("year"=myreplist$timeseries$Yr/4+1953.75,
                   "SB"=myreplist$timeseries$SpawnBio,
                   "Year"=ceiling(myreplist$timeseries$Yr/4)+1953,
                   "Area"=myreplist$timeseries$Area,
                   "Model"=legend[i])
      Mov <- data.frame(myreplist$movement[1,7:47],"Model"=legend[i])
      names(Mov)[1:41] <- 0:40
      Movement <- Mov %>% gather(1:41,key="Age",value="Movement")
    }
    else {
      R <- rbind(R,data.frame("year"=myreplist$timeseries$Yr,
                      "R"=myreplist$timeseries$Recruit_0,
                      "Year"=ceiling(myreplist$timeseries$Yr/4)+1953,
                      "Area"=myreplist$timeseries$Area,
                      "Model"=legend[i]))
      SB <- rbind(SB,data.frame("year"=myreplist$timeseries$Yr/4+1953.75,
                       "SB"=myreplist$timeseries$SpawnBio,
                       "Year"=ceiling(myreplist$timeseries$Yr/4)+1953,
                       "Area"=myreplist$timeseries$Area,
                       "Model"=legend[i]))
      Mov <- data.frame(myreplist$movement[1,7:47],"Model"=legend[i])
      names(Mov)[1:41] <- 0:40
      Movement <- rbind(Movement,Mov %>% gather(1:41,key="Age",value="Movement"))
    }
  }
  
  R_a <- R %>% filter(Year>=1979,Year<2018) %>%
    group_by(Area,Year,Model) %>% summarise(Recruitment=sum(R)) %>%
    group_by(Area,Model) %>% mutate(Recruit=Recruitment/mean(Recruitment))
  
  f1 <- ggplot(data=R_a) +
    geom_line(aes(x=Year,y=Recruit,color=Model)) +
    geom_point(aes(x=Year,y=Recruit,color=Model)) +
    theme_bw(20) +
    facet_wrap(~Area,nrow=2) +
    geom_hline(yintercept = 1, linetype="dashed")
  
  ggsave(f1, file = paste0(Save_Dir, figure_name, "-R.png"), width = 12, height = 8)
  # ggsave(f1, file = paste0(Save_Dir, "-R.eps"), width = 12, height = 8,device=cairo_ps)
  
  f12 <- ggplot(data=R_a) +
    geom_line(aes(x=Year,y=Recruitment,color=Model)) +
    geom_point(aes(x=Year,y=Recruitment,color=Model)) +
    theme_bw(20) +
    facet_wrap(~Area,nrow=2, scales="free_y") +
    geom_hline(yintercept = 1, linetype="dashed")
  
  ggsave(f12, file = paste0(Save_Dir, figure_name, "-Recruit.png"), width = 12, height = 8)
  
  
  SB_a <- SB %>% filter(Year>=1979,Year<2018)
  
  f2 <- ggplot(data=SB_a) +
    geom_line(aes(x=year,y=SB,color=Model)) +
    facet_wrap(~Area,nrow=2) +
    theme_bw(20)
  
  ggsave(f2, file = paste0(Save_Dir, figure_name, "-SB.png"), width = 12, height = 8)
  # ggsave(f2, file = paste0(Save_Dir, "-SB.eps"), width = 12, height = 8,device=cairo_ps)
  
  SB_old <- SB
  
  SB0 <- SB %>% group_by(Area,Model) %>% filter(year==min(year))
  
  SB$SB0 <- rep(SB0$SB,each=length(unique(SB$year)))
  
  SB_a <- SB %>% mutate(SBR=SB/SB0) %>% filter(Year>=1979,Year<2018)
  
  f3 <- ggplot(data=SB_a) +
    geom_line(aes(x=year,y=SBR,color=Model)) +
    facet_wrap(~Area,nrow=2) +
    theme_bw(20)
    # geom_hline(yintercept = 1, linetype="dashed")
  
  ggsave(f3, file = paste0(Save_Dir, figure_name, "-SBR.png"), width = 12, height = 8)
  
  f4 <- ggplot(data=Movement) +
    geom_line(aes(x=as.numeric(Age),y=Movement,color=Model)) +
    theme_bw(16)
  
  ggsave(f4, file = paste0(Save_Dir, figure_name, "-Mov.png"), width = 8, height = 6)
  
}
