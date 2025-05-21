#' Generate area code for longline
#' 
#' \code{get_jitter_results} This function make jitter plots
#' 
#' @export

get_jitter_results<-function(Dir,njitters)
{
  #this function will get the Jitter results from all the runs contained in the root directory Dir
  # and return a table with name of the run, number of the jitter, number of estimated parameters, NLL and gradient
  runs<-dir(Dir)
  OverallRes<-list()
  for(i in 1:length(runs)){
    MyDir<-paste0(Dir,runs[i],"/Jitter/")
    tmp<-list(NA)
    for (j in 1:njitters){
      result <- extract_NLL_npars_gradient(paste0(MyDir,"ss3.par_", j,".sso"))
      tmp<-rbind(tmp,result)
      
    }
    
    tmp2<-as.data.frame(tmp)[-1,]
    #x<-unlist(tmp$objective_value)
    jitterN<-seq(1:njitters)
    #y<-min(x)
    #x<-which(x==y)
    tt<-cbind(rep(runs[i],njitters),jitterN,tmp2)
    OverallRes<-rbind(OverallRes, tt)
  }
  return(OverallRes)
  
}