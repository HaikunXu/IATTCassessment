#' Fishing impact plot
#' 
#' \code{impact_plot} This function automayically generates the fishery impact plot for SS v3.23
#' 
#' @export

impact_plot = function(Dir, n_year, BaseName, n_fishery) {
    
    step_name <- c("noDisc", "noPS", "noLL", "noF")
    n_step <- length(step_name)
    
    fishery1 <- seq(8, 11)
    fishery2 <- seq(1, 11)
    fishery3 <- seq(12, 19)
    fishery4 <- seq(1, 19)
    
    print("change starter file (use par and do not estimate) before this section!!!")
    
    # step = 0; read BC data
    CtrlDir <- paste0(paste0(Dir, BaseName), "/BET-EPO.dat")
    CtrlFile <- readLines(CtrlDir, warn = F)
    Line <- match("#_NOTE:  catch data is ignored for survey fleets", CtrlFile)
    Catch0 <- read.table(file = CtrlDir, nrows = (n_year + 1) * n_fishery + 1, skip = Line)
    names(Catch0) <- c("year", "seas", "fleet", "catch", "catch_se")
    Catch0 <- Catch0 %>% filter(year > 0, fleet > 0) %>% select(year, fleet, catch) %>% spread(fleet, catch)
    
    ### for SS3.30
    
    ### 
    
    ParDir <- paste0(paste0(Dir, BaseName, "/ss.par"))
    ParFile <- readLines(ParDir, warn = F)
    Init_F_1 <- as.numeric(ParFile[75])
    Init_F_14 <- as.numeric(ParFile[77])
    
    # loop starts here
    
    for (step in 1:n_step) {
        print(paste0("step: ", step_name[step]))
        unlink(paste0(Dir, step_name[step]), recursive = TRUE)
        dir.create(paste0(Dir, step_name[step]))
        files = c(paste0(Dir, BaseName, "/ss.par"), paste0(Dir, BaseName, "/go_nohess.bat"), paste0(Dir, BaseName, "/starter.ss"), paste0(Dir, BaseName, 
            "/forecast.ss"), paste0(Dir, BaseName, "/BET-EPO.ctl"), paste0(Dir, BaseName, "/BET-EPO.dat"), paste0(Dir, BaseName, "/ss.exe"))
        file.copy(from = files, to = paste0(Dir, step_name[step]))
        
        CtrlDir <- paste0(paste0(Dir, step_name[step]), "/BET-EPO.dat")
        CtrlFile <- readLines(CtrlDir, warn = F)
        
        Catch <- read.table(file = CtrlDir, nrows = (n_year + 1) * n_fishery + 1, skip = Line)
        names(Catch) <- c("year", "seas", "fleet", "catch", "catch_se")
        
        if (step == 1) 
            fishery <- fishery1
        if (step == 2) 
            fishery <- fishery2
        if (step == 3) 
            fishery <- fishery3
        if (step == 4) 
            fishery <- fishery4
        
        Catch1 <- Catch %>% filter(fleet %in% fishery) %>% mutate(catch = ifelse(year > 0, 0, catch))  # change catch to 0
        Catch2 <- Catch %>% filter((fleet %in% fishery) == FALSE)
        
        Catch_combined <- rbind(Catch1, Catch2)
        
        Catch <- Catch_combined %>% filter(year > 0, fleet > 0) %>% select(year, fleet, catch) %>% spread(fleet, catch)
        
        for (line in 1:((n_year + 1) * n_fishery + 1)) {
            CtrlFile[Line + line] <- gsub(",", "", toString(Catch_combined[line, ]))
        }
        
        writeLines(CtrlFile, CtrlDir)
        
        ParDir <- paste0(paste0(Dir, step_name[step]), "/ss.par")
        ParFile <- readLines(ParDir, warn = F)
        ParFile[75] <- toString(Init_F_1 * sum(Catch[1:20, 2:6])/sum(Catch0[1:20, 2:6]))
        ParFile[77] <- toString(Init_F_14 * sum(Catch[1:20, 13:20])/sum(Catch0[1:20, 13:20]))
        
        writeLines(ParFile, ParDir)
        
        command <- paste("cd", paste0(Dir, step_name[step]), "& go_noHess.bat", sep = " ")
        # command <- paste0(Dir,step_name[step],'/go_nohess.bat')
        x <- shell(cmd = command, intern = T, wait = T)
    }
    
    #### ssplot section
    
    Dir1 <- paste0(Dir, BaseName)
    myreplist1 = r4ss::SS_output(dir = Dir1, ncols = 400, covar = F, printstats = F)
    
    Dir2 <- paste0(Dir, step_name[1])
    myreplist2 = r4ss::SS_output(dir = Dir2, ncols = 400, covar = F, printstats = F)
    
    Dir3 <- paste0(Dir, step_name[2])
    myreplist3 = r4ss::SS_output(dir = Dir3, ncols = 400, covar = F, printstats = F)
    
    Dir4 <- paste0(Dir, step_name[3])
    myreplist4 = r4ss::SS_output(dir = Dir4, ncols = 400, covar = F, printstats = F)
    
    Dir5 <- paste0(Dir, step_name[4])
    myreplist5 = r4ss::SS_output(dir = Dir5, ncols = 400, covar = F, printstats = F)
    
    SB_dif <- data.frame(Year = myreplist1$timeseries$Yr[1:n_year + 2], SB = myreplist1$timeseries$SpawnBio[1:n_year + 2], noDisc = myreplist2$timeseries$SpawnBio[1:n_year + 
        2] - myreplist1$timeseries$SpawnBio[1:n_year + 2], noPS = myreplist3$timeseries$SpawnBio[1:n_year + 2] - myreplist1$timeseries$SpawnBio[1:n_year + 
        2], noLL = myreplist4$timeseries$SpawnBio[1:n_year + 2] - myreplist1$timeseries$SpawnBio[1:n_year + 2])
    
    SB_dif$noDisc_dif <- SB_dif$noDisc/apply(SB_dif[, 2:4], c(1), sum) * (myreplist5$timeseries$SpawnBio[3:174] - myreplist1$timeseries$SpawnBio[3:174])
    SB_dif$noPS_dif <- SB_dif$noPS/apply(SB_dif[, 2:4], c(1), sum) * (myreplist5$timeseries$SpawnBio[3:174] - myreplist1$timeseries$SpawnBio[3:174])
    SB_dif$noDLL_dif <- SB_dif$noLL/apply(SB_dif[, 2:4], c(1), sum) * (myreplist5$timeseries$SpawnBio[3:174] - myreplist1$timeseries$SpawnBio[3:174])
    
    write.csv(SB_dif, file = paste0(Dir, "SB.csv"), row.names = FALSE)
    
    ggplot(data = SB_dif) + geom_ribbon(aes(x = Year, ymin = 0, ymax = SB), fill = "red") + geom_ribbon(aes(x = Year, ymin = SB, ymax = SB + noDisc_dif), 
        fill = "green") + geom_ribbon(aes(x = Year, ymin = SB + noDisc_dif, ymax = SB + noDisc_dif + noPS_dif), fill = "purple") + geom_ribbon(aes(x = Year, 
        ymin = SB + noDisc_dif + noPS_dif, ymax = SB + noDisc_dif + noPS_dif + noDLL_dif), fill = "blue") + theme_bw() + ylab("Spawning biomass (mt)")
    
    ggsave(file = paste0(Dir, "impact_plot.png"), width = 8, height = 6)
    ggsave(file = paste0(Dir, "impact_plot.eps"), width = 8, height = 6)
}
