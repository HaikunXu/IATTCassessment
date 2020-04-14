#' Plot retrospective pattern
#' 
#' \code{plot_retro} This function plot retrospective SB and R for the stock assessment report 
#' 
#' @export

plot_retro = function(SS_Dir, lyear, fyear, Save_Dir, title) {
    # spawning biomass ratio
    for (i in 1:length(lyear)) {
        Myreplist = SS_output(dir=SS_Dir[i],ncols=400,covar=F,forecast=FALSE,verbose = FALSE,printstats = FALSE)
        if(i==1) {
            SBR <- data.frame("Year" = 1975 + (Myreplist$timeseries$Yr-1)/4, "SBR" = Myreplist$timeseries$SpawnBio/Myreplist$timeseries$SpawnBio[1], "Assess_Year" = lyear[i])   
        }
        else {
            SBR <- rbind(SBR,data.frame("Year" = 1975 + (Myreplist$timeseries$Yr-1)/4, "SBR" = Myreplist$timeseries$SpawnBio/Myreplist$timeseries$SpawnBio[1], "Assess_Year" = lyear[i]))
        }
    }
    
    SBR <- SBR %>% mutate(label1 = ifelse(Year>=fyear&Year<(Assess_Year+1),1,0), label2 = ifelse(Year==Assess_Year+0.75,1,0))
    SBR$Assess_Year <- as.factor(SBR$Assess_Year+1)
    
    f1 <- ggplot(data=SBR %>% filter(label1==1)) +
        geom_line(aes(x=Year,y=SBR,color=Assess_Year)) +
        geom_point(aes(x=Year,y=SBR,color=Assess_Year),data = SBR %>% filter(label2==1),size=3) +
        coord_cartesian(ylim=c(0,1.2)) +
        labs(x = "", y = "Spawning biomass ratio") +
        theme_bw(16) + ggtitle(title)
        
    
    ggsave(f1, file = paste0(Save_Dir, "Retro_SBR.png"), width = 8, height = 6)
    
    # Spawning biomass
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
    
    f2 <- ggplot(data=SBR %>% filter(label1==1)) +
        geom_line(aes(x=Year,y=SBR,color=Assess_Year)) +
        geom_point(aes(x=Year,y=SBR,color=Assess_Year),data = SBR %>% filter(label2==1),size=3) +
        # coord_cartesian(ylim=c(0.15,0.3)) +
        labs(x = "", y = "Spawning biomass") +
        theme_bw(16) + ggtitle(title)
    
    ggsave(f2, file = paste0(Save_Dir, "Retro_SB.png"), width = 8, height = 6)
    
    return(f1)
    
}
