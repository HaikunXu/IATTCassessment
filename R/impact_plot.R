#' Fishing impact plot
#' 
#' \code{impact_plot} This function automayically generates the fishery impact plot for SS v3.23
#' 
#' @export

impact_plot = function(Dir, n_year, BaseName) {
    
    step_name <- c("noDisc", "noPS", "noLL", "noF")
    n_step <- length(step_name)
    
    fishery1 <- seq(8, 11)
    fishery2 <- seq(1, 11)
    fishery3 <- seq(12, 19)
    fishery4 <- seq(1, 19)
    
    print("change starter file (use par and do not estimate) before this section!!!")
    
    # step = 0; read BC data
    CtrlDir <- paste0(paste0(Dir, "BET_Base_SAC9"), "/BET-EPO.dat")
    CtrlFile <- readLines(CtrlDir, warn = F)
    Line <- 60
    Catch0 <- read.table(file = CtrlDir, nrows = n_year, skip = Line)
    
    ParDir <- paste0(paste0(Dir, "BET_Base_SAC9"), "/ss3.par")
    ParFile <- readLines(ParDir, warn = F)
    Init_F_1 <- as.numeric(ParFile[73])
    Init_F_14 <- as.numeric(ParFile[99])
    
    # loop starts here
    
    for (step in 1:n_step) {
        print(paste0("step: ", step_name[step]))
        unlink(paste0(Dir, step_name[step]), recursive = TRUE)
        dir.create(paste0(Dir, step_name[step]))
        files = c(paste0(Dir, "BET_Base_SAC9/ss3.par"), paste0(Dir, "BET_Base_SAC9/go_nohess.bat"), paste0(Dir, 
            "BET_Base_SAC9/starter.ss"), paste0(Dir, "BET_Base_SAC9/forecast.ss"), paste0(Dir, "BET_Base_SAC9/BET-EPO.ctl"), 
            paste0(Dir, "BET_Base_SAC9/BET-EPO.dat"), paste0(Dir, "BET_Base_SAC9/ss3.exe"))
        file.copy(from = files, to = paste0(Dir, step_name[step]))
        
        CtrlDir <- paste0(paste0(Dir, step_name[step]), "/BET-EPO.dat")
        CtrlFile <- readLines(CtrlDir, warn = F)
        Line <- match("#F1 F2 F3 F4 F5 F6 F7 F8 F9 F10 F11 F12 F13 F14 F15 F16 F17 F18 F19 Year season                                                                                                                                                                     ", 
            CtrlFile)
        
        Catch <- read.table(file = CtrlDir, nrows = n_year, skip = Line)
        
        if (step == 1) 
            fishery <- fishery1
        if (step == 2) 
            fishery <- fishery2
        if (step == 3) 
            fishery <- fishery3
        if (step == 4) 
            fishery <- fishery4
        
        Catch <- data.matrix(Catch)
        Catch[, fishery] <- 0
        
        for (line in 1:n_year) {
            CtrlFile[Line + line] <- gsub(",", "", toString(Catch[line, ]))
        }
        
        writeLines(CtrlFile, CtrlDir)
        
        ParDir <- paste0(paste0(Dir, step_name[step]), "/ss3.par")
        ParFile <- readLines(ParDir, warn = F)
        ParFile[73] <- toString(Init_F_1 * sum(Catch[1:20, 1:5])/sum(Catch0[1:20, 1:5]))
        ParFile[99] <- toString(Init_F_14 * sum(Catch[1:20, 12:19])/sum(Catch0[1:20, 12:19]))
        
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
    
    SB_dif <- data.frame(Year = myreplist1$timeseries$Yr[1:n_year + 2], SB = myreplist1$timeseries$SpawnBio[1:n_year + 
        2], noDisc = myreplist2$timeseries$SpawnBio[1:n_year + 2] - myreplist1$timeseries$SpawnBio[1:n_year + 
        2], noPS = myreplist3$timeseries$SpawnBio[1:n_year + 2] - myreplist1$timeseries$SpawnBio[1:n_year + 2], 
        noLL = myreplist4$timeseries$SpawnBio[1:n_year + 2] - myreplist1$timeseries$SpawnBio[1:n_year + 2])
    
    
    SB_dif$noDisc_dif <- SB_dif$noDisc/apply(SB_dif[, 2:4], c(1), sum) * (myreplist5$timeseries$SpawnBio[3:174] - 
        myreplist1$timeseries$SpawnBio[3:174])
    SB_dif$noPS_dif <- SB_dif$noPS/apply(SB_dif[, 2:4], c(1), sum) * (myreplist5$timeseries$SpawnBio[3:174] - 
        myreplist1$timeseries$SpawnBio[3:174])
    SB_dif$noDLL_dif <- SB_dif$noLL/apply(SB_dif[, 2:4], c(1), sum) * (myreplist5$timeseries$SpawnBio[3:174] - 
        myreplist1$timeseries$SpawnBio[3:174])
    
    write.csv(SB_dif, file = paste0(Dir, "SB.csv"), row.names = FALSE)
    
    ggplot(data = SB_dif) + geom_ribbon(aes(x = Year, ymin = 0, ymax = SB), fill = "red") + geom_ribbon(aes(x = Year, 
        ymin = SB, ymax = SB + noDisc_dif), fill = "green") + geom_ribbon(aes(x = Year, ymin = SB + noDisc_dif, 
        ymax = SB + noDisc_dif + noPS_dif), fill = "purple") + geom_ribbon(aes(x = Year, ymin = SB + noDisc_dif + 
        noPS_dif, ymax = SB + noDisc_dif + noPS_dif + noDLL_dif), fill = "blue") + theme_bw() + ylab("Spawning biomass (mt)")
    
    ggsave(file = paste0(Dir, "impact_plot.png"), width = 8, height = 6)
    ggsave(file = paste0(Dir, "impact_plot.eps"), width = 8, height = 6)
    
}
