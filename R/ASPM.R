#' Make ASPM runs
#' 
#' \code{ASPM} This function generates ASPM
#' 
#' @export

ASPM = function(Path, ASPM_Path, Rdevs) {
  
  files = c(
    paste0(Path, "/go_nohess.bat"),
    paste0(Path, "/starter.ss"),
    paste0(Path, "/forecast.ss"),
    paste0(Path, "/control.ss_new"),
    paste0(Path, "/BET-EPO.dat"),
    paste0(Path, "/ss.exe")
  )
  file.copy(from = files,
            to = ASPMPath,
            overwrite = TRUE)
  
  # use control_new
  dat <-
    SS_readdat_3.30(file = paste0(ASPMPath, "/BET-EPO.dat"),
                    verbose = FALSE)
  ctl <- SS_readctl_3.30(
    file = paste0(ASPMPath, "/control.ss_new"),
    verbose = FALSE,
    datlist = dat,
    use_datlist = TRUE
  )
  
  # q
  ctl$Q_options$float <- 0
  ctl$Q_parms$PHASE <- 1
  
  # turn off comp likelihood
  ctl$Variance_adjustment_list$Value <- 0
  
  # turn off selex par estimation
  ctl$size_selex_parms$PHASE <- -1
  ctl$size_selex_parms_tv$PHASE <- -1
  
  # no R devs and R regime shift for ASPM
  if (Rdevs == FALSE) {
    ctl$do_recdev <- 0
    ctl$recdev_early_phase <- -1
    if (length(ctl$SR_parms_tv$PHASE) > 1) {
      # if there is a R regime shift, turn it off
      ctl$SR_parms_tv$PHASE[2:length(ctl$SR_parms_tv$PHASE)] <- -1
    }
  }

  # no R bias adjustment
  ctl$max_bias_adj <- 0
  
  # make sure no G pars are estimated
  ctl$MG_parms$PHASE[1:6] <- -1
  
  # write the new control file
  SS_writectl_3.30(
    ctl,
    outfile = paste0(ASPMPath, "/BET-EPO.ctl"),
    overwrite = TRUE,
    verbose = FALSE
  )
  
  # not from the par file
  starterFile <- readLines(paste0(ASPMPath, "/starter.ss"), warn = F)
  starterFile[6] <- toString(0) # start from initial condition
  writeLines(starterFile, paste0(ASPMPath, "/starter.ss"))
  
  setwd(ASPMPath)
  print(ASPMPath)
  
  command <- paste("cd", ASPMPath, "& go_noHess.bat", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  # check the max gradient
  myreplist <- SS_output(
    dir = ASPMPath,
    # ncols = 400,
    covar = F,
    verbose = FALSE,
    printstats = FALSE
  )
  
  return(myreplist$maximum_gradient_component)
}
