#' Fishing impact plot
#' 
#' \code{impact_plot} This function automayically generates the fishery impact plot for SS v3.23
#' 
#' @export

impact_plot = function(Dir, n_year, BaseName, n_fishery, title) {
    
    step_name <- c("noDisc", "noPS", "noLL", "noF")
    n_step <- length(step_name)
    
    fishery1 <- 20
    fishery2 <- c(15:19,21:22)
    fishery3 <- seq(1,14)
    fishery4 <- seq(1,22)
    
    print("change starter file (use par and do not estimate) before this section!!!")
    
    # step = 0; read BC data
    CtrlDir <- paste0(paste0(Dir, BaseName), "/BET-EPO.dat")
    CtrlFile <- readLines(CtrlDir, warn = F)
    Line <- match("#_NOTE:  catch data is ignored for survey fleets", CtrlFile)
    Catch0 <- read.table(file = CtrlDir, nrows = (n_year+1)*n_fishery+1, skip = Line)
    # Catch0 <- read.table(file = CtrlDir, nrows = 3632, skip = Line)
    names(Catch0) <- c("year", "seas", "fleet", "catch", "catch_se")
    
    Catch0 <- Catch0 %>% filter(year > 0, fleet > 0) %>% select(year, fleet, catch) %>% spread(fleet, catch)
    
    ### for SS3.30
    
    ### 
    
    ParDir <- paste0(paste0(Dir, BaseName, "/ss.par"))
    ParFile <- readLines(ParDir, warn = F)
    Line_initial <- match("# Fcast_recruitments:", ParFile)
    Init_F_2 <- as.numeric(ParFile[Line_initial+3])
    Init_F_16 <- as.numeric(ParFile[Line_initial+5])
    
    # loop starts here
    
    for (step in 1:n_step) {
        print(paste0("step: ", step_name[step]))
        unlink(paste0(Dir, step_name[step]), recursive = TRUE)
        dir.create(paste0(Dir, step_name[step]))
        files = c(paste0(Dir, BaseName, "/ss.par"), paste0(Dir, BaseName, "/go_nohess.bat"), paste0(Dir, BaseName,
            "/starter.ss"), paste0(Dir, BaseName, "/forecast.ss"), paste0(Dir, BaseName, "/BET-EPO.ctl"), paste0(Dir,
            BaseName, "/BET-EPO.dat"), paste0(Dir, BaseName, "/ss.exe"))
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

        Catch <- Catch_combined %>% filter(year > 0, fleet > 0) %>% select(year, fleet, catch) %>% spread(fleet,
            catch)

        # plot(Catch$year,Catch$fleet,xlim=c(1,200),main=title)

        for (line in 1:((n_year + 1) * n_fishery + 1)) {
            CtrlFile[Line + line] <- gsub(",", "", toString(Catch_combined[line, ]))
        }

        writeLines(CtrlFile, CtrlDir)

        ParDir <- paste0(paste0(Dir, step_name[step]), "/ss.par")
        ParFile <- readLines(ParDir, warn = F)
        ParFile[Line_initial+3] <- toString(Init_F_2 * sum(Catch[1:20, 1:7])/sum(Catch0[1:20, 1:7]))
        ParFile[Line_initial+5] <- toString(Init_F_16 * sum(Catch[1:20, c(15:19,21:22)])/sum(Catch0[1:20, c(15:19,21:22)]))

        writeLines(ParFile, ParDir)

        setwd(paste0(Dir, step_name[step]))
        command <- paste("cd", paste0(Dir, step_name[step]), "& go_noHess.bat", sep = " ")
        # command <- paste0(Dir,step_name[step],'/go_nohess.bat')
        x <- shell(cmd = command, intern = T, wait = T)
    }
    
    #### ssplot section
    
    Dir1 <- paste0(Dir, BaseName)
    myreplist1 = r4ss::SS_output(dir = Dir1, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    Dir2 <- paste0(Dir, step_name[1])
    myreplist2 = r4ss::SS_output(dir = Dir2, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    Dir3 <- paste0(Dir, step_name[2])
    myreplist3 = r4ss::SS_output(dir = Dir3, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    Dir4 <- paste0(Dir, step_name[3])
    myreplist4 = r4ss::SS_output(dir = Dir4, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    Dir5 <- paste0(Dir, step_name[4])
    myreplist5 = r4ss::SS_output(dir = Dir5, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    SB_dif <- data.frame(
        Year = myreplist1$timeseries$Yr[3:n_year + 2],
        SB = myreplist1$timeseries$SpawnBio[3:n_year + 2],
        SB0 = myreplist5$timeseries$SpawnBio[3:n_year + 2],
        noDisc = myreplist2$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2],
        noPS = myreplist3$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2],
        noLL = myreplist4$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2]
    )
    
    SB_dif$noDisc_dif <- SB_dif$noDisc / apply(SB_dif[, 4:6], c(1), sum) * (SB_dif$SB0 - SB_dif$SB)
    SB_dif$noPS_dif <- SB_dif$noPS / apply(SB_dif[, 4:6], c(1), sum) * (SB_dif$SB0 - SB_dif$SB)
    SB_dif$noLL_dif <- SB_dif$noLL / apply(SB_dif[, 4:6], c(1), sum) * (SB_dif$SB0 - SB_dif$SB)
    
    SB_dif$Year <- SB_dif$Year / 4 + 1974.75
    
    write.csv(SB_dif, file = paste0(Dir, "SB.csv"), row.names = FALSE)
    
    f <-ggplot(data = SB_dif) +
      geom_ribbon(aes(x = Year, ymin = 0, ymax = SB), fill = "red") +
      geom_ribbon(aes(
        x = Year,
        ymin = SB,
        ymax = SB + noDisc_dif
      ), fill = "green") +
      geom_ribbon(aes(
        x = Year,
        ymin = SB + noDisc_dif,
        ymax = SB +
          noDisc_dif + noPS_dif
      ), fill = "purple") +
      geom_ribbon(aes(
        x = Year,
        ymin = SB + noDisc_dif + noPS_dif,
        ymax = SB0
      ), fill = "blue") +
      theme_bw() + ylab("") + xlab("") +
      coord_cartesian(expand = FALSE) + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(f, file = paste0(Dir, "impact_plots.png"), width = 8, height = 6)
    ggsave(f, file = paste0(Dir, "impact_plots.eps"), width = 8, height = 6)
    
    return(f)
}
