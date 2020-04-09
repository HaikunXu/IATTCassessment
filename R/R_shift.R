#' Compute R shift for BET (before and after 1994)
#' 
#' \code{R_shift} This function computes the ratio of median predicted recruitment before and after 1994
#' 
#' @export

R_shift <- function(Rep = Rep){
  
  R <- Rep$recruit
  
  R_med <- R %>% filter(era == "Main") %>%
    mutate(Period=ifelse(Yr<77,"Early","Late")) %>%
    group_by(Period) %>% summarise(med=median(pred_recr),dev_med=median(dev))
  
  R_shift <- c(R_med$med[2]/R_med$med[1],R_med$dev_med[2]/R_med$dev_med[1])
  
  return(R_shift[1])
}