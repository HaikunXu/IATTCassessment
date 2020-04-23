#' Plot size selectivity for SS runs
#' 
#' \code{PlotSplines} This function comapres estimated and emperical selectivity curves for specified fleets
#' 
#' @export
#' 

PlotSplines<-function(Rep=Rep,Path=Path){
  ###Mark's code
  # ****** Use this *******
  Allfleets<-c(1:6,13:17,19:20,22)
  FleetNames <- Rep$FleetNames
  
  postscript(paste0(Path,"Selex.eps"),width = 2000, height =2000)
  par(mfrow = c(4, 4),mar=c(2, 2, 2, 2) + 0.1)           #par(mfrow = c(6, 5),mar=c(5, 4, 4, 2) + 0.1)
  
  for(i in Allfleets)
  {
    fleets<-i
    tt<-Rep$sizeselex[Rep$sizeselex$Factor == "Lsel" & 
                        Rep$sizeselex$Fleet %in% fleets & 
                        Rep$sizeselex$Sex %in% c(1), 
                      ]
    tt<-tt[1,]
    
    plot(seq(20,198,2),tt[,15:104],main = FleetNames[i])
    
    tt2 <- Rep$lendbase[Rep$lendbase$Fleet %in% fleets & 
                          Rep$lendbase$Sex %in% c(1), 
                        ]
    tt2<-tapply(tt2$Obs, tt2$Bin,FUN =mean)
    
    tt3<-Rep$natlen[Rep$natlen$Sex %in% c(1,2) &                                              #average oiver both sexes
                      Rep$natlen$"Beg/Mid" %in% c("B") &
                      Rep$natlen$Era %in% c("TIME"),
                    ]
    tt3<-apply(tt3[,13:122], 2,mean)
    
    tt4<-tt2/tt3[10:99]
    
    lines(seq(20,196,2),tt4[1:89]/max(tt4[1:89]),col="red")
  }
  dev.off()
  
  
  tiff(paste0(Path,"Selex.tif"),width = 2000, height =2000, res=300)  
  
  par(mfrow = c(4, 4),mar=c(2, 2, 2, 2) + 0.1)           #par(mfrow = c(6, 5),mar=c(5, 4, 4, 2) + 0.1)
  
  for(i in Allfleets)
  {
    fleets<-i
    tt<-Rep$sizeselex[Rep$sizeselex$Factor == "Lsel" & 
                        Rep$sizeselex$Fleet %in% fleets & 
                        Rep$sizeselex$Sex %in% c(1), 
                      ]
    tt<-tt[1,]
    
    plot(seq(20,198,2),tt[,15:104],main = FleetNames[i])
    
    tt2 <- Rep$lendbase[Rep$lendbase$Fleet %in% fleets & 
                          Rep$lendbase$Sex %in% c(1), 
                        ]
    tt2<-tapply(tt2$Obs, tt2$Bin,FUN =mean)
    
    tt3<-Rep$natlen[Rep$natlen$Sex %in% c(1,2) &                                              #average oiver both sexes
                      Rep$natlen$"Beg/Mid" %in% c("B") &
                      Rep$natlen$Era %in% c("TIME"),
                    ]
    tt3<-apply(tt3[,13:122], 2,mean)
    
    tt4<-tt2/tt3[10:99]
    
    lines(seq(20,196,2),tt4[1:89]/max(tt4[1:89]),col="red")
  }
  dev.off()
}