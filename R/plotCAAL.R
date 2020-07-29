#' Plot conditional age at length
#' 
#' \code{PlotCAAL} This function plots conditional age at length for SS runs
#' 
#' @export
#' 

PlotCAAL <- function(Path=Path,AgeBins=AgeBins){

  Rep = r4ss::SS_output(dir = Path,ncols = 400,covar = F,verbose = FALSE, printstats = FALSE)
  
  CAAL <- Rep$age_comp_fit_table
  
}
