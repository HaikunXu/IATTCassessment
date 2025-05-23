#' Make ASPM runs
#' 
#' \code{ASPM} This function generates ASPM
#' 
#' @export

ASPM = function(Path, ASPM_Path, Rdevs, Hessian = FALSE, dat_name = "BET-EPO.dat", ctl_name = "BET-EPO.ctl", ss_name = "ss.exe", par_line = 6) {
  
  dir.create(ASPM_Path) # create a folder to run the ASPM
  
  files = c(
    paste0(Path, "/go.bat"),
    paste0(Path, "/go_nohess.bat"),
    paste0(Path, "/starter.ss"),
    paste0(Path, "/forecast.ss"),
    paste0(Path, "/control.ss_new"),
    paste0(Path, "/", dat_name),
    paste0(Path, "/", ss_name)
  )
  file.copy(from = files,
            to = ASPM_Path,
            overwrite = TRUE)
  
  # use control_new
  dat <- r4ss::SS_readdat_3.30(file = paste0(ASPM_Path, "/", dat_name),
                               verbose = FALSE)
  
  ctl <- r4ss::SS_readctl_3.30(
    file = paste0(ASPM_Path, "/control.ss_new"),
    verbose = FALSE,
    datlist = dat,
    use_datlist = TRUE
  )
  
  # q
  # ctl$Q_options$float <- 0
  # ctl$Q_parms$PHASE <- 1
  
  # turn off comp likelihood
  ctl$Variance_adjustment_list$value <- 0
  
  # turn off selex par estimation
  ctl$size_selex_parms$PHASE <- -1
  ctl$size_selex_parms_tv$PHASE <- -1
  
  # added on April 14 2025
  ctl$age_selex_parms$PHASE <- -1
  
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
  r4ss::SS_writectl_3.30(
    ctl,
    outfile = paste0(ASPM_Path, "/", ctl_name),
    overwrite = TRUE,
    verbose = FALSE
  )
  
  # not from the par file
  starterFile <- readLines(paste0(ASPM_Path, "/starter.ss"), warn = F)
  starterFile[par_line] <- toString(0) # start from initial condition
  writeLines(starterFile, paste0(ASPM_Path, "/starter.ss"))
  
  setwd(ASPM_Path)
  print(ASPM_Path)
  
  if(Hessian == FALSE) command <- paste("cd", ASPM_Path, "& go_nohess.bat", sep = " ")
  else command <- paste("cd", ASPM_Path, "& go.bat", sep = " ")
    
  ss <- shell(cmd = command, intern = T, wait = T)
  
  # check the max gradient
  myreplist <- r4ss::SS_output(
    dir = ASPM_Path,
    # ncols = 400,
    covar = F,
    verbose = FALSE,
    printstats = FALSE
  )
  
  return(myreplist$maximum_gradient_component)
}