#' Extract equilibrium catch
#' 
#' \code{equilibrium_catch} This function extracts the expected equilibrium catch 
#' 
#' @export

equilibrium_catch = function(SS_Dir, Fishery) {
  Rep = SS_output(dir=SS_Dir,ncols=400,covar=F,forecast=FALSE,verbose = FALSE,printstats = FALSE)
  Catch <- Rep$catch %>% filter(Fleet %in% Fishery)
  equilibrium_catch <- Catch %>% filter(Yr==min(Yr))
  
  return(equilibrium_catch)
}