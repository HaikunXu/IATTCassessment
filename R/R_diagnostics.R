#' Compute R shift for BET (before and after 1994) and mean(R)/R0
#' 
#' \code{R_diagnostics} This function computes the ratio of median predicted recruitment before and after 1994; and the ratio of mean recruitment to R0
#' 
#' @export

R_diagnostics <- function(Rep = Rep){
  
  R <- Rep$recruit
  R_med <- R %>% filter(era=="Main") %>%
    mutate(Period=ifelse(Yr<77,"Early","Late")) %>%
    group_by(Period) %>% summarise(med=median(pred_recr))
  R_shift <- R_med$med[2]/R_med$med[1]
  
  R0 <- exp(Rep$parameters$Value[which(Rep$parameters$Label=="SR_LN(R0)")])
  R_mean <- mean(R$pred_recr[which(R$era=="Main"&R$Yr>76)])
  R_ratio <- R_mean/R0
  
  return(c(R_shift,R_ratio))
}