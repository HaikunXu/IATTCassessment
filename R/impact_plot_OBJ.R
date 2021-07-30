#' Fishing impact plot
#' 
#' \code{impact_plot_OBJ} This function automayically generates the fishery impact plot for SS v3.23
#' 
#' @export

impact_plot_OBJ = function(Dir, n_year, BaseName, n_fishery, title) {
    
    step_name <- c("noF13", "noF14", "noF15", "noF16", "noF17", "noOtherPS", "noLL","noF")
    n_step <- length(step_name)
    
    fishery1 <- 13
    fishery2 <- 14
    fishery3 <- 15
    fishery4 <- 16
    fishery5 <- 17
    fishery6 <- seq(18,23)
    fishery7 <- seq(1,12)
    fishery8 <- seq(1,23)
    
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
    Line_initial <- match("# Fcast_impl_error:", ParFile)
    Init_F_2 <- as.numeric(ParFile[Line_initial+3])
    Init_F_14 <- as.numeric(ParFile[Line_initial+5])
    
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
        if (step == 5)
            fishery <- fishery5
        if (step == 6)
            fishery <- fishery6
        if (step == 7)
            fishery <- fishery7
        if (step == 8)
            fishery <- fishery8
        
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
        ParFile[Line_initial+3] <- toString(Init_F_2 * sum(Catch[1:20, 1:6])/sum(Catch0[1:20, 1:6]))
        ParFile[Line_initial+5] <- toString(Init_F_14 * sum(Catch[1:20, 13:17])/sum(Catch0[1:20, 13:17]))

        writeLines(ParFile, ParDir)

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
    
    Dir6 <- paste0(Dir, step_name[5])
    myreplist6 = r4ss::SS_output(dir = Dir6, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    Dir7 <- paste0(Dir, step_name[6])
    myreplist7 = r4ss::SS_output(dir = Dir7, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    Dir8 <- paste0(Dir, step_name[7])
    myreplist8 = r4ss::SS_output(dir = Dir8, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    Dir9 <- paste0(Dir, step_name[8])
    myreplist9 = r4ss::SS_output(dir = Dir9, ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    SB_dif <- data.frame(Year = myreplist1$timeseries$Yr[3:n_year + 2],
                         SS = myreplist1$timeseries$SpawnBio[3:n_year + 2],
                         SB0 = myreplist9$timeseries$SpawnBio[3:n_year + 2],
                         noF13 = myreplist2$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2],
                         noF14 = myreplist3$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2],
                         noF15 = myreplist4$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2],
                         noF16 = myreplist5$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2],
                         noF17 = myreplist6$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2],
                         noOtherPS = myreplist7$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2],
                         noLL = myreplist8$timeseries$SpawnBio[3:n_year + 2] - myreplist1$timeseries$SpawnBio[3:n_year + 2])
    
    # SB_dif2 <- SB_dif %>% gather()
    
    SB_dif[["A2-OBJ"]] <- SB_dif$noF13/apply(SB_dif[, 4:10], c(1), sum) * (SB_dif$SB0 - SB_dif$SS)
    SB_dif[["A3-OBJ"]] <- SB_dif$noF14/apply(SB_dif[, 4:10], c(1), sum) * (SB_dif$SB0 - SB_dif$SS)
    SB_dif[["A4-OBJ"]] <- SB_dif$noF15/apply(SB_dif[, 4:10], c(1), sum) * (SB_dif$SB0 - SB_dif$SS)
    SB_dif[["A5-OBJ"]] <- SB_dif$noF16/apply(SB_dif[, 4:10], c(1), sum) * (SB_dif$SB0 - SB_dif$SS)
    SB_dif[["A6-OBJ"]] <- SB_dif$noF17/apply(SB_dif[, 4:10], c(1), sum) * (SB_dif$SB0 - SB_dif$SS)
    SB_dif[["Other-PS"]] <- SB_dif$noOtherPS/apply(SB_dif[, 4:10], c(1), sum) * (SB_dif$SB0 - SB_dif$SS)
    SB_dif[["All-LL"]] <- SB_dif$noLL/apply(SB_dif[, 4:10], c(1), sum) * (SB_dif$SB0 - SB_dif$SS)

    SB_dif$Year <- SB_dif$Year/4 + 1974.75
    
    SB_dif1 <- SB_dif[,c(1,2,11:17)] %>% gather(2:9,key="Fishery",value="Impact") %>%
        mutate(Fishery=factor(Fishery)) %>%
        mutate(Fishery=factor(Fishery,levels = levels(Fishery)[c(6,7,5,4,3,2,1,8)]))
    
    ggplot(data=SB_dif1) +
        geom_area(aes(x=Year,y=Impact,fill=Fishery),color="black",position = "stack") +
        theme_bw()
    
    SB_dif2 <- SB_dif[,c(1,11:17)] %>% gather(2:8,key="Fishery",value="Impact") %>%
        group_by(Year) %>% mutate(Impact_Prop=Impact/sum(Impact)) %>%
        mutate(Fishery=factor(Fishery)) %>%
        mutate(Fishery=factor(Fishery,levels = levels(Fishery)[c(6,7,5,4,3,2,1)]))
    
    f <- ggplot(data=SB_dif2) +
        geom_area(aes(x=Year,y=Impact_Prop,fill=Fishery),color="black",position = "stack") +
        theme_bw(15) +
        ylab("Proportional impact") +
        coord_cartesian(expand = FALSE)
    
    ggsave(f, file = paste0(Dir, "impact_plot_OBJ.png"), width = 8, height = 6)
    
    # write.csv(SB_dif, file = paste0(Dir, "SB2.csv"), row.names = FALSE)
    
    f <- ggplot(data = SB_dif) + 
        geom_ribbon(aes(x = Year, ymin = SB, ymax = SB + noF13_dif), fill = "red") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif, ymax = SB + noF13_dif + noF14_dif), fill = "orange") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif), fill = "yellow") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif + noF15_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif), fill = "green") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif), fill = "cyan") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif + noOtherPS_dif), fill = "blue") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif + noOtherPS_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif + noOtherPS_dif + noLL_dif), fill = "purple") +
        geom_line(aes(x = Year, y = SB),size=1.5) + 
        geom_line(aes(x = Year, y = SB0),size=1.5) + 
        theme_bw() + ylab("Spawning biomass (mt)")
        # ggtitle(title)
    
    ggsave(f, file = paste0(Dir, "impact_plot_OBJ.png"), width = 8, height = 6)
    # ggsave(f, file = paste0(Dir, "impact_plot.eps"), width = 8, height = 6)
 
    SB_dif$noF13_prop <- SB_dif$noF13/apply(SB_dif[, 4:10], c(1), sum)
    SB_dif$noF14_prop <- SB_dif$noF14/apply(SB_dif[, 4:10], c(1), sum)
    SB_dif$noF15_prop <- SB_dif$noF15/apply(SB_dif[, 4:10], c(1), sum)
    SB_dif$noF16_prop <- SB_dif$noF16/apply(SB_dif[, 4:10], c(1), sum)
    SB_dif$noF17_prop <- SB_dif$noF17/apply(SB_dif[, 4:10], c(1), sum)
    SB_dif$noOtherPS_prop <- SB_dif$noOtherPS/apply(SB_dif[, 4:10], c(1), sum)
    SB_dif$noLL_prop <- SB_dif$noLL/apply(SB_dif[, 4:10], c(1), sum)
    
    SB_dif2 <- SB_dif[,c(1,4:10)] %>% gather(2:9,key="Fishery",value="Impact")
    
    ggplot(data=SB_dif2) +
        geom_area(aes(x=Year,y=Impact,fill=Fishery))
    
    ggplot(data = SB_dif) + 
        geom_ribbon(aes(x = Year, ymin = SB, ymax = SB + noF13_dif), fill = "red") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif, ymax = SB + noF13_dif + noF14_dif), fill = "orange") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif), fill = "yellow") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif + noF15_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif), fill = "green") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif), fill = "cyan") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif + noOtherPS_dif), fill = "blue") +
        geom_ribbon(aes(x = Year, ymin = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif + noOtherPS_dif, ymax = SB + noF13_dif + noF14_dif + noF15_dif + noF16_dif + noF17_dif + noOtherPS_dif + noLL_dif), fill = "purple") +
        geom_line(aes(x = Year, y = SB),size=1.5) + 
        geom_line(aes(x = Year, y = SB0),size=1.5) + 
        theme_bw() + ylab("Spawning biomass (mt)")
    # return(f)
}
