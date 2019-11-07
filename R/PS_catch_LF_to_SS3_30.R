###preparation of PS YFT data for the Benchmark model 2019
### New fishery definitions 
### C.V. Minte-Vera
### 11/06/2019
### Note: THIS code CAN BE MODIFIED FOR BET 
### Need the estimated catches and length frequency as "RData" Object from Cleridy

require(tidyverse)
dir_input<-"C:/Users/hkxu/Desktop/test/"
dir_output<-dir_input

load(paste0(dir_input,"YFT_formatted_1975-2018_External Review 2019_R version of CSV.RData"))
StartYear<- 1975
MyEqCatch<- 0 # Equilibrium catch equal zero
  
  
the.DPcatch<-rbind(yft.DPcatch.19751999,yft.DPcatch.20002018)
the.DPcomps<-rbind(yft.DPcomps.19751999,yft.DPcomps.20002018)
the.UNcatch<-rbind(yft.UNcatch.19751999,yft.UNcatch.20002018)
the.UNcomps<-rbind(yft.UNcomps.19751999,yft.UNcomps.20002018)
the.FOcatch<-rbind(yft.FOcatch.19751999,yft.FOcatch.20002018)
the.FOcomps<-rbind(yft.FOcomps.19751999,yft.FOcomps.20002018)


###Fleets
##FO fisheries are by area and by quarters 4,1 X 2,3 
nFO<- 2* length(unique(the.FOcomps$area))
nUN<-length(unique(the.UNcomps$area))
nDP<-length(unique(the.DPcomps$area))
nFl<-nFO+nUN+nDP

#Format data to enter SS3.30

#Floating Objects 
FOcomps<-the.FOcomps %>% gather(colnames(the.FOcomps)[5:205],key=Length,value=n,na.rm = TRUE) %>% 
  mutate(Length=as.numeric(str_sub(Length,2,5)))%>%mutate(Len2cm=2*floor(Length/2))%>% 
  filter(year>=StartYear) %>% mutate(Yrr=((((year+((quarter-1)/4))-1975)+0.25)/0.25))%>%
  mutate(season=((Yrr*0)+1))%>%mutate(gender=Yrr*0)%>%mutate(partition=gender)%>%mutate(thefleet=str_sub(area,2,5))%>%
  mutate( fleet = ifelse(quarter %in% 2:3, as.numeric(thefleet)+5,as.numeric(thefleet)))

FOcompsFinal<- FOcomps %>% group_by(Yrr, season, fleet,gender, partition,Len2cm) %>%
  summarise(nsamples=max(nwells,na.rm=T), freq=sum(n,na.rm=T)) %>%spread(key = Len2cm, value = freq) %>%
  arrange(fleet,Yrr)

FO<- cbind(as.data.frame(FOcompsFinal), as.data.frame(FOcompsFinal)[,7:107])
print("Done Floating Objects sets length frequency")
table(FO$fleet)

  
####Unassociated fleets starts at 11 (nFO +1)
UNcomps<-the.UNcomps %>% gather(colnames(the.UNcomps)[5:205],key=Length,value=n,na.rm = TRUE) %>% 
  mutate(Length=as.numeric(str_sub(Length,2,5)))%>%mutate(Len2cm=2*floor(Length/2))%>% 
  filter(year>=StartYear) %>% mutate(Yrr=((((year+((quarter-1)/4))-1975)+0.25)/0.25))%>%
  mutate(season=((Yrr*0)+1))%>%mutate(gender=Yrr*0)%>%mutate(partition=gender)%>%mutate(thefleet=str_sub(area,2,5))%>%
  mutate( fleet = as.numeric(thefleet)+nFO) 

UNcompsFinal<- UNcomps %>% group_by(Yrr, season, fleet,gender, partition,Len2cm) %>%
  summarise(nsamples=max(nwells,na.rm=T), freq=sum(n,na.rm=T)) %>%spread(key = Len2cm, value = freq) %>%
  arrange(fleet,Yrr)

UN<- cbind(as.data.frame(UNcompsFinal), as.data.frame(UNcompsFinal)[,7:107])

print("Done Unassociated sets length frequency")
table(UN$fleet)

###Dolphin sets fleets starts at nFO+nUN + 1
DPcomps<-the.DPcomps %>% gather(colnames(the.DPcomps)[5:205],key=Length,value=n,na.rm = TRUE) %>% 
  mutate(Length=as.numeric(str_sub(Length,2,5)))%>%mutate(Len2cm=2*floor(Length/2))%>% 
  filter(year>=StartYear) %>% mutate(Yrr=((((year+((quarter-1)/4))-1975)+0.25)/0.25))%>%
  mutate(season=((Yrr*0)+1))%>%mutate(gender=Yrr*0)%>%mutate(partition=gender)%>%mutate(thefleet=str_sub(area,2,5))%>%
  mutate( fleet = as.numeric(thefleet)+nFO+nUN)

DPcompsFinal<- DPcomps %>% group_by(Yrr, season, fleet,gender, partition,Len2cm) %>%
  summarise(nsamples=max(nwells,na.rm=T), freq=sum(n,na.rm=T)) %>%spread(key = Len2cm, value = freq) %>%
  arrange(fleet,Yrr)

DP<- cbind(as.data.frame(DPcompsFinal), as.data.frame(DPcompsFinal)[,7:107])

print("Done Dolphin sets length frequency")
print(table(DP$fleet))

#######Length Frequencies
#End<-as.vector(DP[1,])
#End<-c(-9999,rep(0,length(End)))
  
PS.LF<-rbind(FO,UN,DP)
print("Done all purse-seine length frequency")

print(table(PS.LF$fleet))

write.table(PS.LF,file=paste0(dir_output,"PS_Length_frequency_to_SS3_30.txt"),sep=" ",row.names = FALSE,col.names=TRUE)
print("purse-seine length frequency saved to the file: PS_Length_frequency_to_SS3_30.txt")
print(dir_output)



##########Catch
#Floating objects --- REMEMBER the number of areas is 5 (nFO/2) and fleets is 10 (nFO)
FOcatch<- the.FOcatch %>% filter(year>=StartYear) %>% gather(colnames(the.FOcatch)[3:(2+(nFO/2))],key=area,value=catch,na.rm = TRUE)%>%
  mutate(Yrr=((((year+((quarter-1)/4))-1975)+0.25)/0.25))%>%mutate(season=((Yrr*0)+1))%>% mutate(sd=Yrr*0+0.01) %>%
  mutate(thefleet=str_sub(area,2,5))%>% mutate( fleet = ifelse(quarter %in% 2:3, as.numeric(thefleet)+5,as.numeric(thefleet))) %>% 
  select(c('Yrr','season','fleet','catch','sd')) 

print("Done Catch Floating Objects")
print(table(FOcatch$fleet))
print("length frequency Floating Objects")
print(table(FO$fleet))

#########Unassociated fleets starts at 11 (nFO +1)

UNcatch<- the.UNcatch %>% filter(year>=StartYear) %>% gather(colnames(the.UNcatch)[3:(3+nUN-1)],key=area,value=catch,na.rm = TRUE)%>%
  mutate(Yrr=((((year+((quarter-1)/4))-1975)+0.25)/0.25))%>%mutate(season=((Yrr*0)+1))%>% mutate(sd=Yrr*0+0.01) %>%
  mutate(thefleet=str_sub(area,2,5))%>% mutate( fleet = as.numeric(thefleet)+nFO) %>%
  select(c('Yrr','season','fleet','catch','sd'))

print("Done Catch Unassociated")
print(table(UNcatch$fleet))
print("length frequency Unassociated")
print(table(UN$fleet))

###Dolphin sets fleets starts at nFO+nUN + 1
DPcatch<- the.DPcatch %>% filter(year>=StartYear) %>% gather(colnames(the.DPcatch)[3:(3+nDP-1)],key=area,value=catch,na.rm = TRUE)%>%
  mutate(Yrr=((((year+((quarter-1)/4))-1975)+0.25)/0.25))%>%mutate(season=((Yrr*0)+1))%>% mutate(sd=Yrr*0+0.01) %>%
  mutate(thefleet=str_sub(area,2,5))%>% mutate( fleet = as.numeric(thefleet)+nFO+nUN)%>%
  select(c('Yrr','season','fleet','catch','sd'))

print("Done Catch Dolphin sets")
print(table(DPcatch$fleet))
print("length frequency Dolphin sets")
print(table(DP$fleet))

#Equilibrium Catch
eq<-as_tibble(list(Yrr=rep(-999,nFl),season=rep(1,nFl),fleet=seq(1,nFl,1),catch=rep(MyEqCatch,nFl),sd=rep(0.01,nFl)))
print("Done Equilibrium Catch")
print(table(eq$fleet))

###FINAL CATCH
PS.Catch<-bind_rows(FOcatch,UNcatch,DPcatch,eq) %>% arrange(fleet,Yrr)
print("Done all purse seine catch")
table(PS.Catch$fleet)

write.table(PS.Catch,file=paste0(dir_output,"PSCatch_to_SS3_30.txt"),sep=" ",row.names = FALSE,col.names=TRUE)
print("purse-seine catch to the file: PSCatch_to_SS3_30.txt")
print(dir_output)




