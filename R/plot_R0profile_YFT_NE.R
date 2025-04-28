#' Plot the R0 likelihood profile for the bigeye model
#' 
#' \code{plot_R0profile_YFT_NE} This function plots the R0 likelihood profile for the bigeye model
#' 
#' @export

plot_R0profile_YFT_NE = function(Path, R0, R0_MLE) {
  
  N = length(R0)
  NLL_a <- data.frame("Total"=rep(NA,N),
                      "Index"=rep(NA,N),
                      "F_comps" = rep(NA,N),
                      "I_comps"=rep(NA,N),
                      "Recruit"=rep(NA,N),
                      "R0"=R0)
  NLL_comp <- data.frame("R0"=R0)

  for (n in 1:N) {
    myreplist = r4ss::SS_output(dir = paste0(Path,toString(R0[n])),covar = F,verbose = FALSE, printstats = FALSE)
    print(n/N)

    NLL_a[n,c(1,2,5)] <- myreplist$likelihoods_used$values[c(1,4,8)]
    NLL_a[n,3] <- myreplist$likelihoods_by_fleet[10,2] # PS fishery comps
    # NLL_a[n,4] <- sum(myreplist$likelihoods_by_fleet[18,26:39]) # LL fishery comps
    NLL_a[n,4] <- myreplist$likelihoods_by_fleet[20,42]
    

  }
  
  NLL_amin <- NLL_a %>% gather(Total,Index,F_comps,I_comps,Recruit,value="nll",key="Component") %>%
    group_by(Component) %>% mutate(NLL=nll-min(nll))
  
  f1 <- ggplot() +
    geom_line(aes(x=R0,y=NLL,color=Component),data=NLL_amin,size=1) +
    geom_point(aes(x=R0,y=NLL,color=Component),data=NLL_amin,size=3) +
    theme_bw(12) +
    geom_vline(xintercept = R0_MLE, linetype = "dashed", size = 1) +
    xlab("ln(R0)") +
    ylab("NLL")
  
  ggsave(f1, file = paste0(Path, "R0.png"), width = 12, height = 8)
  
  return(NLL_amin)
    
}