#' Plot retrospective pattern
#' 
#' \code{plot_retro} This function plot retrospective SB and R for the stock assessment report 
#' 
#' @export

plot_retro = function(SS_Dir, lyear, fyear, Save_Dir) {
    for (i in 1:length(lyear)) {
        Myreplist = SS_output(dir=SS_Dir[i],ncols=400,covar=F,forecast=FALSE,verbose = FALSE,printstats = FALSE)
        if(i==1) {
            SBR <- data.frame("Year" = 1975 + (Myreplist$timeseries$Yr-1)/4, "SBR" = Myreplist$timeseries$SpawnBio, "Assess_Year" = lyear[i])   
        }
        else {
            SBR <- rbind(SBR,data.frame("Year" = 1975 + (Myreplist$timeseries$Yr-1)/4, "SBR" = Myreplist$timeseries$SpawnBio, "Assess_Year" = lyear[i]))
        }
    }
    
    SBR <- SBR %>% mutate(label1 = ifelse(Year>=fyear&Year<(Assess_Year+1),1,0), label2 = ifelse(Year==Assess_Year+0.75,1,0))
    SBR$Assess_Year <- as.factor(SBR$Assess_Year+1)
    
    f <- ggplot(data=SBR %>% filter(label1==1)) +
        geom_line(aes(x=Year,y=SBR,color=Assess_Year)) +
        geom_point(aes(x=Year,y=SBR,color=Assess_Year),data = SBR %>% filter(label2==1),size=3) +
        # coord_cartesian(ylim=c(0.15,0.3)) +
        labs(x = "", y = "Spawning biomass") +
        theme_bw(16)
    
    ggsave(f, file = paste0(Save_Dir, "Retro.png"), width = 12, height = 8)
    
}
