#' Plot size selectivity for SS runs
#' 
#' \code{PlotSpline} This function comapres estimated and emperical selectivity curves for specified Fleet
#' 
#' @export
#' 

PlotSpline<-function(Path,Save_Path,Fleet,model_name,fyear,lyear,dim,w,h){
  ###Mark's code
  # ****** Use this *******

  tiff(paste0(Save_Path,"Fleet",toString(Fleet)," Selex.tif"), width = w, height = h)  
  par(mfrow = dim)
  
  for(m in 1:length(Path)) {
    print(m)
    Rep <- r4ss::SS_output(dir = Path[m], covar = F, printstats = F, verbose = FALSE)
    
    tt<-Rep$sizeselex[Rep$sizeselex$Factor == "Lsel" & 
                        Rep$sizeselex$Fleet %in% Fleet & 
                        Rep$sizeselex$Sex %in% c(1), 
                      ]
    tt<-tt[nrow(tt),]
    
    plot(seq(20,186,2), tt[,15:98], type="l", main = model_name[m],
         xlab = "Length", ylab = "Selectivity", cex=3)
    
    tt2 <- Rep$sizedbase[Rep$sizedbase$Fleet %in% Fleet & 
                          Rep$sizedbase$Sex %in% c(1), 
                        ] %>% filter(Yr>=fyear,Yr<=lyear)
      
    tt2<-tapply(tt2$Obs, tt2$Bin, FUN = mean) # mean size freq
    
    tt3<-Rep$natlen[Rep$natlen$Sex %in% c(1,2) &                      #average over both sexes
                      Rep$natlen$"Beg/Mid" %in% c("B") &
                      Rep$natlen$Era %in% c("TIME"),
                    ] %>% filter(Yr>=fyear,Yr<=lyear)
    
    tt3_df <- tt3 %>% gather(13:122,key="Length",value=N) %>%
      mutate(L = cut(as.numeric(Length), breaks = c(seq(60, 190, 10), 250), right = F, labels = seq(60, 190, 10))) %>%
      na.omit() %>%
      group_by(L) %>%
      summarise(N_mean=mean(N))
    
    # tt3<-apply(tt3[,13:122], 2,mean)
    
    tt4<-tt2/tt3_df$N_mean
    
    lines(seq(65,185,10), tt4[1:13]/max(tt4[1:13]), col="red", type="p")
    # lines(seq(20,196,2),lowess(tt4[1:89],f=f)$y/max(lowess(tt4[1:89],f=f)$y),col="red")
  }
  dev.off()
}
