#' Plot Pearson residuals for SS runs
#' 
#' \code{PlotPearsonRes} This function makes Pearson residual plots for a specified fishery
#' 
#' @export

PlotPearsonRes <- function(Path, myFleet, fishery, binsize, title, xlim, ylim, alpha){
  
  Rep <- r4ss::SS_output(dir = Path,ncols = 400,covar = F,verbose = FALSE, printstats = FALSE)
  
  if(sum(fishery)==length(fishery)) { # all fleets are fishery fleets
    dat <- Rep$lendbase
    tmp <- dat %>% filter(Kind=="LEN",Fleet %in% myFleet) %>%
      select(Yr,Fleet,Pearson,Bin) %>% mutate(Bin=Bin+binsize[1]/2)
  }
  
  if(sum(fishery)==0) { # all fleets are survey fleets
    dat <- Rep$sizedbase
    tmp <- dat %>% filter(Kind=="SIZE",Fleet %in% myFleet) %>%
      select(Yr,Fleet,Pearson,Bin) %>% mutate(Bin=Bin+binsize[2]/2)
  }
  
  if(sum(fishery)>0&sum(fishery)<length(fishery)) { # both fishery adn survey fleets
    dat1 <- Rep$lendbase
    tmp1 <- dat1 %>% filter(Kind=="LEN",Fleet %in% myFleet[which(fishery==TRUE)]) %>%
      select(Yr,Fleet,Pearson,Bin) %>% mutate(Bin=Bin+binsize[1]/2)
    dat2 <- Rep$sizedbase
    tmp2 <- dat2 %>% filter(Kind=="SIZE",Fleet %in% myFleet[which(fishery==FALSE)]) %>%
      select(Yr,Fleet,Pearson,Bin) %>% mutate(Bin=Bin+binsize[2]/2)
    
    tmp <- rbind(tmp1,tmp2)
  }
  
  tmp <- tmp %>% mutate(Fleet=factor(Fleet),Yr=ceiling(Yr/4)+1974)
  
  tmp_y <- tmp %>% group_by(Fleet,Yr) %>% summarise(med=median(Pearson),low=quantile(Pearson,0.25),high=quantile(Pearson,0.75))
  tmp_l <- tmp %>% group_by(Fleet,Bin) %>% summarise(med=median(Pearson),low=quantile(Pearson,0.25),high=quantile(Pearson,0.75))
  
  f1 <- ggplot(data=tmp_y) +
    geom_ribbon(aes(x = Yr, ymin = low, ymax = high, fill = Fleet), alpha=alpha) + 
    geom_line(aes(x=Yr,y=med,color=Fleet),size=1) +
    # geom_point(aes(x=Yr,y=med,color=Fleet)) +
    theme_bw(15) +
    xlab("Year") +ylab("") +
    geom_hline(yintercept = 0,linetype="dashed") +
    coord_cartesian(ylim=ylim,xlim=xlim) + ggtitle(title) + ggeasy::easy_center_title()

  
  f2 <- ggplot(data=tmp_l) +
    geom_ribbon(aes(x = Bin, ymin = low, ymax = high, fill = Fleet), alpha=alpha) + 
    geom_line(aes(x=Bin,y=med,color=Fleet),size=1) +
    # geom_point(aes(x=Yr,y=med,color=Fleet)) +
    theme_bw(15) +
    xlab("Length (cm)") +ylab("") +
    geom_hline(yintercept = 0,linetype="dashed") +
    coord_cartesian(ylim=ylim) + ggtitle(title) + ggeasy::easy_center_title()
  
  f_all <- gridExtra::grid.arrange(f1,f2, nrow = 2)
  
  return(f_all)
  
}

