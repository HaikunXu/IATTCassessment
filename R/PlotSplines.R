#' Plot size selectivity for SS runs
#' 
#' \code{PlotSplines} This function compares estimated and empirical selectivity curves for specified fleets
#' 
#' @export
#' 

PlotSplines<-function(Path=Path,Allfleets,f,fyears,lyears,nr,nc,w,h,name="Selex"){
  ###Mark's code
  # ****** Use this *******
  Rep = r4ss::SS_output(dir = Path, covar = F, verbose = FALSE, printstats = FALSE)
  
  FleetNames <- Rep$FleetNames
  
  
  tiff(paste0(Path,name,".tif"),width = w, height = h, res=150)  
  
  par(mfrow = c(nr, nc),mar=c(2, 2, 2, 2) + 0.1)           #par(mfrow = c(6, 5),mar=c(5, 4, 4, 2) + 0.1)
  
  for(i in 1:length(Allfleets))
  {
    fleets<-Allfleets[i]
    tt<-Rep$sizeselex[Rep$sizeselex$Factor == "Lsel" & 
                        Rep$sizeselex$Fleet %in% fleets & 
                        Rep$sizeselex$Sex %in% c(1), 
                      ]
    tt<-tt[max(which(tt$Yr<=lyears)),]
    
    plot(seq(20,186,2),tt[,15:98], type="l",
         main = paste0("F",toString(i)," (",FleetNames[Allfleets[i]],")"))
    
    # # if(lors[i]=="l") {
    #   tt2 <- Rep$lendbase[Rep$lendbase$Fleet %in% fleets &
    #                         Rep$lendbase$Sex %in% c(1),]
    # }
    # else {
      tt2 <- Rep$sizedbase[Rep$sizedbase$Fleet %in% fleets &
                             Rep$sizedbase$Sex %in% c(1),]
    # }

    tt2<-tapply(tt2$Obs, tt2$Bin, FUN =mean)
    
    tt3<-Rep$natlen[Rep$natlen$Sex %in% c(1,2) &            # average over both sexes
                      Rep$natlen$"Beg/Mid" %in% c("B") &
                      Rep$natlen$Era %in% c("TIME"),
                    ] %>% filter(Yr>=fyears,Yr<=lyears)
    
    tt3_df <- tt3 %>% gather(13:122,key="Length",value=N) %>%
      mutate(L = cut(as.numeric(Length), breaks = c(seq(60, 190, 10), 250), right = F, labels = seq(60, 190, 10))) %>%
      na.omit() %>%
      group_by(L) %>%
      summarise(N_mean=mean(N))

    # tt3<-apply(tt3[,13:122], 2, mean)
    # tt4<-tt2/tt3[10:99]
    
    tt4<-tt2/tt3_df$N_mean
    
    lines(seq(65,185,10),tt4[1:13]/max(tt4[1:13]),col="red",type="p")
    # lines(seq(65,185,10),lowess(tt4[1:13],f=f)$y/max(lowess(tt4[1:13],f=f)$y),col="red")
  }
  dev.off()
  
  
  # 
  # postscript(paste0(Path,"Selex.eps"),width = w, height = h)
  # 
  # par(mfrow = c(nr, nc),mar=c(2, 2, 2, 2) + 0.1)           #par(mfrow = c(6, 5),mar=c(5, 4, 4, 2) + 0.1)
  # 
  # for(i in 1:length(Allfleets))
  # {
  #   fleets<-Allfleets[i]
  #   tt<-Rep$sizeselex[Rep$sizeselex$Factor == "Lsel" & 
  #                       Rep$sizeselex$Fleet %in% fleets & 
  #                       Rep$sizeselex$Sex %in% c(1), 
  #   ]
  #   tt<-tt[1,]
  #   
  #   plot(seq(20,186,2),tt[,15:98], type="l",
  #        main = paste0("F",toString(i)," (",FleetNames[Allfleets[i]],")"))
  #   
  #   # # if(lors[i]=="l") {
  #   #   tt2 <- Rep$lendbase[Rep$lendbase$Fleet %in% fleets &
  #   #                         Rep$lendbase$Sex %in% c(1),]
  #   # }
  #   # else {
  #   tt2 <- Rep$sizedbase[Rep$sizedbase$Fleet %in% fleets &
  #                          Rep$sizedbase$Sex %in% c(1),]
  #   # }
  #   
  #   tt2<-tapply(tt2$Obs, tt2$Bin, FUN =mean)
  #   
  #   tt3<-Rep$natlen[Rep$natlen$Sex %in% c(1,2) &            # average over both sexes
  #                     Rep$natlen$"Beg/Mid" %in% c("B") &
  #                     Rep$natlen$Era %in% c("TIME"),
  #   ] %>% filter(Yr>=fyears,Yr<=lyears)
  #   
  #   tt3_df <- tt3 %>% gather(13:122,key="Length",value=N) %>%
  #     mutate(L = cut(as.numeric(Length), breaks = c(seq(60, 190, 10), 250), right = F, labels = seq(60, 190, 10))) %>%
  #     na.omit() %>%
  #     group_by(L) %>%
  #     summarise(N_mean=mean(N))
  #   
  #   # tt3<-apply(tt3[,13:122], 2, mean)
  #   # tt4<-tt2/tt3[10:99]
  #   
  #   tt4<-tt2/tt3_df$N_mean
  #   
  #   lines(seq(65,185,10),tt4[1:13]/max(tt4[1:13]),col="red",type="p")
  #   # lines(seq(65,185,10),lowess(tt4[1:13],f=f)$y/max(lowess(tt4[1:13],f=f)$y),col="red")
  # }
  # dev.off()
}