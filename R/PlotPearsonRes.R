#' Plot Pearson residuals for SS runs
#' 
#' \code{PlotPearsonRes} This function makes Pearson residual plots for a specified fishery
#' 
#' @export

PlotPearsonRes <- function(Path, myFleet, fishery, title, ylim){
  
  Rep <- r4ss::SS_output(dir = Path,ncols = 400,covar = F,verbose = FALSE, printstats = FALSE)
  
  if(sum(fishery)==length(fishery)) { # all fleets are fishery fleets
    dat <- Rep$lendbase
    tmp <- dat %>% filter(Kind=="LEN",Fleet %in% myFleet) %>%
      select(Yr,Fleet,Pearson,Bin)
  }
  
  if(sum(fishery)==0) { # all fleets are survey fleets
    dat <- Rep$sizedbase
    tmp <- dat %>% filter(Kind=="SIZE",Fleet %in% myFleet) %>%
      select(Yr,Fleet,Pearson,Bin)
  }
  
  if(sum(fishery)>0&sum(fishery)<length(fishery)) { # both fishery adn survey fleets
    dat1 <- Rep$lendbase
    tmp1 <- dat1 %>% filter(Kind=="LEN",Fleet %in% myFleet[which(fishery==TRUE)]) %>%
      select(Yr,Fleet,Pearson,Bin)
    dat2 <- Rep$sizedbase
    tmp2 <- dat2 %>% filter(Kind=="SIZE",Fleet %in% myFleet[which(fishery==FALSE)]) %>%
      select(Yr,Fleet,Pearson,Bin)
    
    tmp <- rbind(tmp1,tmp2)
  }
  
  tmp <- tmp %>% mutate(Fleet=factor(Fleet),Yr=ceiling(Yr/4)+1974)
  
  tmp_y <- tmp %>% group_by(Fleet,Yr) %>% summarise(med=mean(Pearson))
  tmp_l <- tmp %>% group_by(Fleet,Bin) %>% summarise(med=mean(Pearson))
  
  f1 <- ggplot(data=tmp_y) +
    geom_line(aes(x=Yr,y=med,color=Fleet)) +
    geom_point(aes(x=Yr,y=med,color=Fleet)) +
    theme_bw(15) +
    xlab("Year") +ylab("Pearson residual") +
    geom_hline(yintercept = 0,linetype="dashed") +
    coord_cartesian(ylim=ylim) + ggtitle(title) + ggeasy::easy_center_title()

  
  f2 <- ggplot(data=tmp_l) +
    geom_line(aes(x=Bin,y=med,color=Fleet)) +
    geom_point(aes(x=Bin,y=med,color=Fleet)) +
    theme_bw(15) +
    xlab("Length (cm)") +ylab("Pearson residual") +
    geom_hline(yintercept = 0,linetype="dashed") +
    coord_cartesian(ylim=ylim) + ggtitle(title) + ggeasy::easy_center_title()
  
  f_all <- gridExtra::grid.arrange(f1,f2, nrow = 2)
  
  return(f_all)
  
}

