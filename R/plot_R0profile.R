#' Plot the R0 likelihood profile for the bigeye model
#' 
#' \code{plot_R0profile} This function plots the R0 likelihood profile for the bigeye model
#' 
#' @export

plot_R0profile = function(Path, R0, R0_MLE, Fleet_comps) {
  
  N = length(R0)
  NLL_a <- data.frame("Total"=rep(NA,N),
                      "Index"=rep(NA,N),
                      "Fcomps_PS" = rep(NA,N),
                      "Fcomps_LL"=rep(NA,N),
                      "Scomps"=rep(NA,N),
                      "Recruit"=rep(NA,N),
                      "R0"=R0)
  NLL_comp <- data.frame("R0"=R0)
  for(f in 1:length(Fleet_comps)) NLL_comp <- cbind(NLL_comp, rep(NA,N))
  
  for (n in 1:N) {
    myreplist = r4ss::SS_output(dir = paste0(Path,toString(R0[n])),covar = F,verbose = FALSE, printstats = FALSE)
    print(n/N)

    NLL_a[n,c(1,2,6)] <- myreplist$likelihoods_used$values[c(1,4,8)]
    NLL_a[n,3] <- myreplist$likelihoods_by_fleet[10,2]
    NLL_a[n,4] <- sum(myreplist$likelihoods_by_fleet[18,3:9]) # fishery comps
    NLL_a[n,5] <- myreplist$likelihoods_by_fleet[18,25]
    
    NLL_temp <- myreplist$likelihoods_by_fleet
    for(c in 1:length(Fleet_comps)) {
      if(Fleet_comps[c]>7) NLL_comp[n,c+1] <- NLL_temp[NLL_temp$Label=="Length_like",Fleet_comps[c]+2]
      else NLL_comp[n,c+1] <- NLL_temp[NLL_temp$Label=="SizeFreq_like:_1",Fleet_comps[c]+2]
    }
    
  }
  
  NLL_amin <- NLL_a %>% gather(Total,Index,Fcomps_PS,Fcomps_LL,Scomps,Recruit,value="nll",key="Component") %>%
    group_by(Component) %>% mutate(NLL=nll-min(nll))
  
  f1 <- ggplot() +
    geom_line(aes(x=R0,y=NLL,color=Component),data=NLL_amin,size=1) +
    geom_point(aes(x=R0,y=NLL,color=Component),data=NLL_amin,size=3) +
    theme_bw(12) +
    geom_vline(xintercept = R0_MLE, linetype = "dashed", size = 1) +
    xlab("ln(R0)") +
    ylab("NLL")
  
  ggsave(f1, file = paste0(Path, "R0.png"), width = 12, height = 8)
  
  names(NLL_comp)[2:(length(Fleet_comps)+1)] <- names(NLL_temp)[Fleet_comps+2]
  NLL_comp_amin <- NLL_comp %>% gather(2:(length(Fleet_comps)+1),value="nll",key="Fleet") %>%
    group_by(Fleet) %>% mutate(NLL=nll-min(nll))
  NLL_comp_amin$Gear <- rep(c(rep("LL", 7), rep("OBJ", 5), rep("NOA", 2)), each = N)
  NLL_comp_amin$Area <- rep(c("1", "2", "3", "4", "5", "6", "7",
                              "1", "2", "3", "4", "5",
                              "1", "2"), each = N)
  
  
  f2 <- ggplot(NLL_comp_amin %>% filter(Gear != "NOA")) +
    geom_line(aes(x = R0, y = NLL, color = Area)) +
    geom_text(aes(x = R0, y = NLL, label = Area, color = Area),size = 6) +
    theme_bw() +
    facet_wrap(~ Gear, nrow = 1) +
    geom_vline(xintercept = R0_MLE,
               linetype = "dashed",
               size = 1) +
    xlab("ln(R0)")
  
  ggsave(f2, file = paste0(Path, "R0_F.png"), width = 10, height = 5)
  
  return(f1)
    
}