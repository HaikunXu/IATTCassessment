#' Fishing mortality by age group
#' 
#' \code{pdf_cdf} This function computes the fishing mortality by age group for the stock assessment report 
#' 
#' @export

pdf_cdf = function(x, DF) {
  R_y <- DF
  mean <- R_y$Est
  sd <- R_y$Std
  weight <- R_y$Weight
  n_model <- length(weight)
  
  for (m in 1:n_model) {
    if(m == 1) pdf <- dnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
    else pdf <- pdf + dnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
  }
  
  for (m in 1:n_model) {
    if(m == 1) cdf <- pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
    else cdf <- cdf + pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
  }
  
  return(list("cdf"=cdf,"pdf"=pdf))
}