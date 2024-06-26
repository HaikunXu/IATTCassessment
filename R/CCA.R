#' Make CCA runs
#' 
#' \code{CCA} This function generates CCA
#' 
#' @export

CCA = function(Path, CCA_Path, Hessian = FALSE) {
  
  dir.create(CCA_Path)
  
  files = c(
    paste0(Path, "/go.bat"),
    paste0(Path, "/go_nohess.bat"),
    paste0(Path, "/starter.ss"),
    paste0(Path, "/forecast.ss"),
    paste0(Path, "/control.ss_new"),
    paste0(Path, "/BET-EPO.dat"),
    paste0(Path, "/ss.exe")
  )
  file.copy(from = files,
            to = CCA_Path,
            overwrite = TRUE)
  
  # use control_new
  dat <- r4ss::SS_readdat_3.30(file = paste0(CCA_Path, "/BET-EPO.dat"),
                    verbose = FALSE)
  
  ctl <- r4ss::SS_readctl_3.30(
    file = paste0(CCA_Path, "/control.ss_new"),
    verbose = FALSE,
    datlist = dat,
    use_datlist = TRUE
  )
  
  # turn off CPUE likelihood
  ctl$lambdas <- rbind(ctl$lambdas, c(1, 23, 1, 0, 1))
  ctl$N_lambdas <- ctl$N_lambdas + 1
  
  # do not estimate catchability
  ctl$Q_options$float <- 1
  ctl$Q_parms$PHASE <- -1
  
  # write the new control file
  r4ss::SS_writectl_3.30(
    ctl,
    outfile = paste0(CCA_Path, "/BET-EPO.ctl"),
    overwrite = TRUE,
    verbose = FALSE
  )
  
  # not from the par file
  starterFile <- readLines(paste0(CCA_Path, "/starter.ss"), warn = F)
  starterFile[6] <- toString(0) # start from initial condition
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
