#' Generate area code for longline
#' 
#' /code{R0_profile} This function makes R0 profile plot for BET SAC11 assessments
#' 
#' @export

R0_profile = function(Path,R0) {
  N = length(R0)
  comps <- c(2:6,13:17,24:25)
  NLL_a <- data.frame("Total"=rep(NA,N),
                      "Index"=rep(NA,N),
                      "Fcomps"=rep(NA,N),
                      "Scomps"=rep(NA,N),
                      "Recruitment"=rep(NA,N),
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
  NLL_amin <- NLL_a %>% gather(Total,Index,Fcomps,Scomps,Recruitment,value="nll",key="Component") %>%
    group_by(Component) %>% mutate(NLL=nll-min(nll))
  
  f1 <- ggplot() +
        geom_line(aes(x=R0,y=NLL,color=Component),data=NLL_amin %>% filter(Component=="Total"),size=1.5) +
geom_line(aes(x=R0,y=NLL,color=Component),data=NLL_amin %>% filter(Component!="Total")) +
    theme_bw(12) +
    xlab("") +
    ylab("NLL - min(NLL)")
  
  # ggsave(f1, file = paste0(Path, "R0_1.png"), width = 8, height = 6)
  
  
  NLL_comp_amin <- NLL_comp %>% gather(names(NLL_temp)[comps+2],value="nll",key="Component") %>%
    group_by(Component) %>% mutate(NLL=nll-min(nll))
  
  NLL_comp_amin$Fishery <- c(rep("LL-Fishery",N*5),rep("OBJ-Fishery",N*5),rep("LL-Survey",N*2))
  NLL_comp_amin$Area <- c(rep(c("A2","A3","A4","A5","A6"),each=N),
                          rep(c("A2","A3","A4","A5","A6"),each=N),
                          rep("EPO-early",N),
                          rep("EPO-late",N))
  
  f2 <- ggplot() +
    geom_line(aes(x=R0,y=NLL,color=Area,linetype=Fishery),data=NLL_comp_amin) +
    # geom_point(aes(x=R0,y=NLL,shape=Fishery),data=NLL_comp_amin) +
    theme_bw(12) +
    # geom_vline(xintercept = mean(R0),linetype="dashed") +
    xlab("ln(R0)") +
    ylab("NLL - min(NLL)")
  
  # ggsave(f2, file = paste0(Path, "R0_2.png"), width = 8, height = 6)
  
  f_all <- gridExtra::grid.arrange(f1, f2, nrow = 2)
  
  ggsave(f_all, file = paste0(Path, "R0.png"), width = 6, height = 10)
  
}
