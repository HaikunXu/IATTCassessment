#' Check whether the SS model is converged without parameters hitting the bound
#' 
#' \code{check_convergence} This function checks whether the SS model is converged without parameters hitting the bound
#' 
#' @export
check_convergence = function(Path) {
  Rep <- r4ss::SS_output(dir = Path, printstats = F, verbose = FALSE)
  
  print(paste0("Max gradient is: ", Rep$maximum_gradient_component))
  
  Par <- Rep$estimated_non_dev_parameters %>% filter(Status != "OK")
  print(paste0(nrow(Par), " parameters need to be checked:"))
  print(Par)
}
