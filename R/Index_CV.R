#' Compute the mean of Index CV using the ASPM_Rdevs run
#' 
#' \code{Index_CV} This function compute index cv 
#' 
#' @export

Index_CV = function(Path) {
  
  # from https://github.com/PacificCommunity/ofp-sam-2023-assessments/blob/main/cpue/cv/demo/simple/sigma.R
  Replist <- SS_output(
    dir = Path,
    covar = F,
    verbose = FALSE,
    printstats = FALSE
  )
  
  Index <- Replist$cpue
  sigma <- sqrt(sum((log(Index$Obs)-log(Index$Exp))^2) / length(Index$Obs))
  
  return(sigma)
}
