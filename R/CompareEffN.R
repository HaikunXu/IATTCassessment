#' Plot Pearson residuals for SS runs
#' 
#' \code{CompareEffN} This function compare estimated effective sample size in a table
#' 
#' @export

CompareEffN <- function(Path, myFleet, model_name){
  
  Rep <- r4ss::SS_output(dir = Path[1],ncols = 400,covar = F,verbose = FALSE, printstats = FALSE)
  
  dat <- Rep$lendbase
  tmp <- dat %>% filter(Kind=="LEN",Fleet %in% myFleet) %>%
      select(Yr,Fleet,effN) %>% group_by(Fleet) %>%
    summarise(EffN=mean(effN)) %>% mutate(Model=model_name[1])
  
  if(length(Path)>1) {
    for(m in 2:length(Path)) {
      Rep <- r4ss::SS_output(dir = Path[m],ncols = 400,covar = F,verbose = FALSE, printstats = FALSE)
      
      dat <- Rep$lendbase
      tmp <- rbind(tmp,dat %>% filter(Kind=="LEN",Fleet %in% myFleet) %>%
        select(Yr,Fleet,effN) %>% group_by(Fleet) %>%
        summarise(EffN=mean(effN)) %>% mutate(Model=model_name[m]))
    }
  }
  
  tmp_table <- tmp %>% spread(Model,EffN)
  
  return(tmp_table)
  
}

