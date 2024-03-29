#' Plot size selectivity for SS runs
#' 
#' \code{PlotSpline_PO} This function comapres estimated and emperical selectivity curves for specified Fleet
#' 
#' @export
#' 

PlotSpline_PO<-function(Path,Save_Path,Fleet,model_name,f,fyear,lyear,dim,w,h){
  ###Mark's code
  # ****** Use this *******
  # AllFleet<-c(1:6,13:17,19:20,22)
  # FleetNames <- Rep$FleetNames
  
  # postscript(paste0(Save_Path,"Fleet",toString(Fleet)," Selex.eps"),width=9,height=9)
  # par(mfrow = dim,mar=c(2, 2, 2, 2) + 0.1)           #par(mfrow = c(6, 5),mar=c(5, 4, 4, 2) + 0.1)
  # 
  # for(m in 1:length(model)) {
  #   Rep <- r4ss::SS_output(dir = Path[m], ncols = 400, covar = F, printstats = F, verbose = FALSE)
  #   
  #   tt<-Rep$sizeselex[Rep$sizeselex$Factor == "Lsel" & 
  #                       Rep$sizeselex$Fleet %in% Fleet & 
  #                       Rep$sizeselex$Sex %in% c(1), 
  #                     ]
  #   tt<-tt[1,]
  #   
  #   plot(seq(20,198,2),tt[,15:104],main = model_name[m])
  #   
  #   tt2 <- Rep$lendbase[Rep$lendbase$Fleet %in% Fleet & 
  #                         Rep$lendbase$Sex %in% c(1), 
  #                       ]
  #   tt2<-tapply(tt2$Obs, tt2$Bin,FUN =mean)
  #   
  #   tt3<-Rep$natlen[Rep$natlen$Sex %in% c(1,2) &                                              #average oiver both sexes
  #                     Rep$natlen$"Beg/Mid" %in% c("B") &
  #                     Rep$natlen$Era %in% c("TIME"),
  #                   ] %>% filter(Yr>=fyear,Yr<=lyear)
  #   tt3<-apply(tt3[,13:122], 2,mean)
  #   
  #   tt4<-tt2/tt3[10:99]
  #   
  #   lines(seq(20,196,2),lowess(tt4[1:89],f=f)$y/max(lowess(tt4[1:89],f=f)$y),col="red")
  # }
  # dev.off()
  
  
  tiff(paste0(Save_Path,"Fleet",toString(Fleet)," Selex.tif"),width = w, height =h, res=300)  
  par(mfrow = dim,mar=c(2, 2, 2, 2) + 0.1)
  
  for(m in 1:length(Path)) {
    print(m)
    Rep <- r4ss::SS_output(dir = Path[m], ncols = 400, covar = F, printstats = F, verbose = FALSE)
    
    tt<-Rep$sizeselex[Rep$sizeselex$Factor == "Lsel" & 
                        Rep$sizeselex$Fleet %in% Fleet & 
                        Rep$sizeselex$Sex %in% c(1), 
    ]
    tt<-tt[1,]
    
    plot(seq(20,220,2),tt[,15:115],main = model_name[m])
    
    tt2 <- Rep$lendbase[Rep$lendbase$Fleet %in% Fleet & 
                          Rep$lendbase$Sex %in% c(1), 
    ]
    tt2<-tapply(tt2$Obs, tt2$Bin,FUN =mean)
    
    tt3<-Rep$natlen[Rep$natlen$Sex %in% c(1,2) &                                              #average oiver both sexes
                      Rep$natlen$"Beg/Mid" %in% c("B") &
                      Rep$natlen$Era %in% c("TIME"),
    ] %>% filter(Yr>=fyear,Yr<=lyear)
    tt3<-apply(tt3[,13:122], 2,mean)
    
    tt4<-tt2[6:95]/tt3[10:99]
    
    lines(seq(20,196,2),lowess(tt4[1:89],f=f)$y/max(lowess(tt4[1:89],f=f)$y),col="red")
    # lines(seq(20,196,2),tt4[1:89]/max(tt4[1:89]),col="red")
    
  }
  dev.off()
}
