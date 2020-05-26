#' Generate R0 likelihoood profile figure
#' 
#' /code{R0_profile} This function makes R0 profile plot for BET SAC11 assessments
#' 
#' @export

R0_profile = function(Path,R0,title,L) {
  N = length(R0)
  if(L==FALSE) comps <- c(2:6,13:17,24:25)
  else comps <- c(2:6,13:17,24)
  NLL_a <- data.frame("Total"=rep(NA,N),
                      "Indices"=rep(NA,N),
                      "Fcomps"=rep(NA,N),
                      "Scomps"=rep(NA,N),
                      "Recruits"=rep(NA,N),
                      "R0"=R0)
  
  NLL_comp <- data.frame("R0"=R0)
  for(f in 1:length(comps)) NLL_comp <- cbind(NLL_comp, rep(NA,N))

  for (n in 1:N) {
    myreplist = r4ss::SS_output(dir = paste0(Path,toString(R0[n])),ncols = 400,covar = F,verbose = FALSE, printstats = FALSE)
    NLL_a[n,1:5] <- myreplist$likelihoods_used$values[c(1,4,5,6,7)]
    
    NLL_temp <- myreplist$likelihoods_by_fleet
    for(c in 1:length(comps)) {
      if(comps[c]<24) NLL_comp[n,c+1] <- NLL_temp[NLL_temp$Label=="Length_like",comps[c]+2]
      else NLL_comp[n,c+1] <- NLL_temp[NLL_temp$Label=="SizeFreq_like:_1",comps[c]+2]
    }
  }
  
  names(NLL_comp)[2:(length(comps)+1)] <- names(NLL_temp)[comps+2]
  NLL_amin <- NLL_a %>% gather(Total,Indices,Fcomps,Scomps,Recruits,value="nll",key="Component") %>%
    group_by(Component) %>% mutate(NLL=nll-min(nll))
  
  f1 <- ggplot() +
        geom_line(aes(x=R0,y=NLL,color=Component),data=NLL_amin,size=1) +
    geom_point(aes(x=R0,y=NLL,color=Component,shape=Component),data=NLL_amin,size=3) +
    theme_bw(20) +
    xlab("") +
    ylab("") +
    ggtitle(title) + ggeasy::easy_center_title()
  
  # ggsave(f1, file = paste0(Path, "R0_1.png"), width = 8, height = 6)
  
  
  NLL_comp_amin <- NLL_comp %>% gather(names(NLL_temp)[comps+2],value="nll",key="Component") %>%
    group_by(Component) %>% mutate(NLL=nll-min(nll))
  
  if(L==FALSE) {
      NLL_comp_amin$Fleet <- c(rep("Fishery-LL",N*5),rep("Fishery-OBJ",N*5),rep("Survey-Early",N),rep("Survey-Late",N))
  NLL_comp_amin$Area <- c(rep(c("A2","A3","A4","A5","A6"),each=N),
                          rep(c("A2","A3","A4","A5","A6"),each=N),
                          rep("EPO",2*N))
  }
  else {
    NLL_comp_amin$Fleet <- c(rep("Fishery-LL",N*5),rep("Fishery-OBJ",N*5),rep("Survey-Late",N))
    NLL_comp_amin$Area <- c(rep(c("A2","A3","A4","A5","A6"),each=N),
                            rep(c("A2","A3","A4","A5","A6"),each=N),
                            rep("EPO",N))
  }

  # 
  f2 <- ggplot() +
    geom_line(aes(x=R0,y=NLL,color=Area,linetype=Fleet),data=NLL_comp_amin) +
    # geom_point(aes(x=R0,y=NLL,color=Area,shape=Fleet),data=NLL_comp_amin) +
    theme_bw(12) +
    # geom_vline(xintercept = mean(R0),linetype="dashed") +
    xlab("ln(R0)") +
    ylab("NLL - min(NLL)")

  # ggsave(f2, file = paste0(Path, "R0_2.png"), width = 8, height = 6)

  f_all <- gridExtra::grid.arrange(f1, f2, nrow = 2)
  
  ggsave(f_all, file = paste0(Path, "R0.png"), width = 6, height = 6)
  
  f <- list("f1"=f1,"f2"=f2)
  return(f)
  
}
