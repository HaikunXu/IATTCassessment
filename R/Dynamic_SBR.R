#' #extract dynamic SBR
#' 
#' \code{Dynamic_SBR} This function extracts dynamic SBR
#' 
#' @export

Dynamic_SBR = function(Path = NA, Replist = NA) {
  
  if(is.na(Replist)) {
    Replist = r4ss::SS_output(
      dir = Path,
      covar = F,
      forecast = FALSE,
      verbose = FALSE,
      printstats = FALSE
    )
  }
  
  Dyna
  
  return(myreplist$maximum_gradient_component)
}