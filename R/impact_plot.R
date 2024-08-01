#' Fishing impact plot
#' 
#' \code{impact_plot} This function automatically generates the fishery impact plot for SS v3.30
#' 
#' @param Dir The directory in which the base model is stored as a subfolder (not the directory of the base model folder)
#' @param n_year The number of model time steps
#' @param BaseName The name of the base model folder
#' @param n_fishery The number of fishery fleets
#' @param title The title of the impact plot
#' 
#' @export

impact_plot = function(Dir, n_year, BaseName = "Base", n_fishery, title) {
  
  step_name <- c("noDisc", "noPS", "noLL", "noF")
  n_step <- length(step_name)
  
  fishery1 <- 20 # discard fishery fleet
  fishery2 <- c(15:19,21:22) # PS fishery fleet
  fishery3 <- seq(1,14) # LL fishery fleets
  fishery4 <- seq(1,22) # all fleets
  
  print("change starter file (use par and do not estimate) before this section!!!")
  
  # step = 0; read base-case catch data
  DataDir <- paste0(paste0(Dir, BaseName), "/BET-EPO.dat")
  DataFile <- readLines(DataDir, warn = F)
  Line <- match("#_NOTE:  catch data is ignored for survey fleets", DataFile)
  Catch0 <- read.table(
    file = DataDir,
    nrows = (n_year + 1) * n_fishery + 1,
    skip = Line
  )
  names(Catch0) <- c("year", "seas", "fleet", "catch", "catch_se")
  
  Catch0 <- Catch0 %>% filter(year > 0, fleet > 0) %>%
    select(year, fleet, catch) %>% spread(fleet, catch)
  
  ### for SS3.30
  ParDir <- paste0(paste0(Dir, BaseName, "/ss3.par"))
  ParFile <- readLines(ParDir, warn = F)
  Line_initial <- match("# Fcast_recruitments:", ParFile)
  Init_F_2 <- as.numeric(ParFile[Line_initial + 3]) # one initial F parameter for LL fisheries
  Init_F_16 <- as.numeric(ParFile[Line_initial + 5]) # one initial F parameter for PS fisheries
  
  # loop starts here
  for (step in 1:n_step) {
    print(paste0("step: ", step_name[step]))
    unlink(paste0(Dir, step_name[step]), recursive = TRUE)
    dir.create(paste0(Dir, step_name[step])) # create a new subfolder to run each model in the impact plot
    
    files = c(paste0(Dir, BaseName, "/ss3.par"), 
              paste0(Dir, BaseName, "/go_nohess.bat"),
              paste0(Dir, BaseName, "/starter.ss"),
              paste0(Dir, BaseName, "/forecast.ss"),
              paste0(Dir, BaseName, "/BET-EPO.ctl"), 
              paste0(Dir, BaseName, "/BET-EPO.dat"), 
              paste0(Dir, BaseName, "/ss.exe"))
    
    file.copy(from = files, to = paste0(Dir, step_name[step]))
    
    DataDir <- paste0(paste0(Dir, step_name[step]), "/BET-EPO.dat")
    DataFile <- readLines(DataDir, warn = F)
    
    Catch <- read.table(file = DataDir, nrows = (n_year + 1) * n_fishery + 1, skip = Line)
    names(Catch) <- c("year", "seas", "fleet", "catch", "catch_se")
    
    if (step == 1) fishery <- fishery1
    if (step == 2) fishery <- fishery2
    if (step == 3) fishery <- fishery3
    if (step == 4) fishery <- fishery4
    
    Catch1 <- Catch %>% filter(fleet %in% fishery) %>%
      mutate(catch = ifelse(year > 0, 0, catch))  # change catch to 0
    Catch2 <- Catch %>% filter((fleet %in% fishery) == FALSE)
    
    Catch_combined <- rbind(Catch1, Catch2)
    
    Catch <- Catch_combined %>% filter(year > 0, fleet > 0) %>% 
      select(year, fleet, catch) %>% spread(fleet, catch)
    
    # Write new catch data frame 
    for (line in 1:((n_year + 1) * n_fishery + 1)) {
      DataFile[Line + line] <- gsub(",", "", toString(Catch_combined[line, ]))
    }
    
    writeLines(DataFile, DataDir)
    
    # adjust initial F parameters based on new catch time series
    ParDir <- paste0(paste0(Dir, step_name[step]), "/ss3.par")
    ParFile <- readLines(ParDir, warn = F)
    ParFile[Line_initial+3] <- Init_F_2 * sum(Catch[1:20, 2:8])/sum(Catch0[1:20, 2:8])
    ParFile[Line_initial+5] <- Init_F_16 * sum(Catch[1:20, c(15:19,21:22)+1])/sum(Catch0[1:20, c(15:19,21:22)+1])
    
    # write new par file
    writeLines(ParFile, ParDir)
    
    # run the SS model
    setwd(paste0(Dir, step_name[step]))
    command <- paste("cd", paste0(Dir, step_name[step]), "& go_noHess.bat", sep = " ")
    # command <- paste0(Dir,step_name[step],'/go_nohess.bat')
    x <- shell(cmd = command, intern = T, wait = T)
  }
  
  #### ssplot section
  
  Dir1 <- paste0(Dir, BaseName)
  myreplist1 = r4ss::SS_output(dir = Dir1, covar = F, printstats = F, verbose = FALSE)
  
  Dir2 <- paste0(Dir, step_name[1])
  myreplist2 = r4ss::SS_output(dir = Dir2, covar = F, printstats = F, verbose = FALSE)
  
  Dir3 <- paste0(Dir, step_name[2])
  myreplist3 = r4ss::SS_output(dir = Dir3, covar = F, printstats = F, verbose = FALSE)
  
  Dir4 <- paste0(Dir, step_name[3])
  myreplist4 = r4ss::SS_output(dir = Dir4, covar = F, printstats = F, verbose = FALSE)
  
  Dir5 <- paste0(Dir, step_name[4])
  myreplist5 = r4ss::SS_output(dir = Dir5, covar = F, printstats = F, verbose = FALSE)
  
  SB_dif <- data.frame(
    Year = myreplist1$timeseries$Yr[4:(n_year + 2)],
    SB = myreplist1$timeseries$SpawnBio[4:(n_year + 2)],
    SB0 = myreplist5$timeseries$SpawnBio[4:(n_year + 2)],
    noDisc = myreplist2$timeseries$SpawnBio[4:(n_year + 2)] - myreplist1$timeseries$SpawnBio[4:(n_year + 2)],
    noPS = myreplist3$timeseries$SpawnBio[4:(n_year + 2)] - myreplist1$timeseries$SpawnBio[4:(n_year + 2)],
    noLL = myreplist4$timeseries$SpawnBio[4:(n_year + 2)] - myreplist1$timeseries$SpawnBio[4:(n_year + 2)]
  )
  
  SB_dif$Disc <- SB_dif$noDisc / apply(SB_dif[, 4:6], c(1), sum) * (SB_dif$SB0 - SB_dif$SB)
  SB_dif$PS <- SB_dif$noPS / apply(SB_dif[, 4:6], c(1), sum) * (SB_dif$SB0 - SB_dif$SB)
  SB_dif$LL <- SB_dif$noLL / apply(SB_dif[, 4:6], c(1), sum) * (SB_dif$SB0 - SB_dif$SB)
  
  SB_dif$Year <- SB_dif$Year / 4 + 1974.75 # the model starts from 1975
  
  write.csv(SB_dif, file = paste0(Dir, "SB.csv"), row.names = FALSE)
  # 
  SB_all <- SB_dif[,c(1,2,7,8,9)] %>%
    gather(2:5, key = "Fishery", value = "SB")
  
  SB_all$Fishery <- factor(SB_all$Fishery, levels = c("LL", "PS", "Disc", "SB"))
  
  f <- ggplot(data = SB_all) +
    geom_area(aes(x = Year, y = SB, fill = Fishery)) +
    theme_bw() + ylab("") + xlab("") +
    coord_cartesian(expand = FALSE) + ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(f, file = paste0(Dir, "impact_plots.png"), width = 8, height = 6)
  
  return(SB_dif)
}
