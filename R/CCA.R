#' Make CCA runs
#' 
#' \code{CCA} This function generates CCA
#' 
#' @export

CCA = function(Path, CCA_Path, Index_fleet, Hessian = FALSE, dat_name = "BET-EPO.dat", ctl_name = "BET-EPO.ctl", ss_name = "ss.exe", par_line = 6) {
  
  dir.create(CCA_Path)
  
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
            to = CCA_Path,
            overwrite = TRUE)
  
  # use control_new
  dat <- r4ss::SS_readdat_3.30(file = paste0(CCA_Path, "/", dat_name),
                    verbose = FALSE)
  
  ctl <- r4ss::SS_readctl_3.30(
    file = paste0(CCA_Path, "/control.ss_new"),
    verbose = FALSE,
    datlist = dat,
    use_datlist = TRUE
  )
  
  # turn off CPUE likelihood
  ctl$lambdas <- rbind(ctl$lambdas, c(1, Index_fleet, 1, 0, 1))
  ctl$N_lambdas <- ctl$N_lambdas + 1
  
  # do not estimate catchability
  ctl$Q_options$float <- 1
  ctl$Q_parms$PHASE <- -1
  
  # make sure no G pars are estimated
  ctl$MG_parms$PHASE[1:6] <- -1
  
  # write the new control file
  r4ss::SS_writectl_3.30(
    ctl,
    outfile = paste0(CCA_Path, "/", ctl_name),
    overwrite = TRUE,
    verbose = FALSE
  )
  
  # not from the par file
  starterFile <- readLines(paste0(CCA_Path, "/starter.ss"), warn = F)
  starterFile[par_line] <- toString(0) # start from initial condition
  writeLines(starterFile, paste0(CCA_Path, "/starter.ss"))
  
  setwd(CCA_Path)
  print(CCA_Path)
  
  if(Hessian == FALSE) command <- paste("cd", CCA_Path, "& go_nohess.bat", sep = " ")
  else command <- paste("cd", CCA_Path, "& go.bat", sep = " ")
  
  ss <- shell(cmd = command, intern = T, wait = T)
  
  # check the max gradient
  myreplist <- r4ss::SS_output(
    dir = CCA_Path,
    # ncols = 400,
    covar = F,
    verbose = FALSE,
    printstats = FALSE
  )
  
  return(myreplist$maximum_gradient_component)
}
