##### Generate the data used for DEL survey's index and length comp standardization

setwd("C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/YFT assessment/Data/DEL survey/")

library(tidyverse)

load(file = 'cae data_1975-2019_for Haikun.RData')

data <- cae.19752019.frm %>% mutate(Year=(year-1975)*4+ceiling(month/3))
VID <- data %>% filter(year>1984) %>%
  group_by(year,vesno) %>% summarise(prop=sum(num.dsets)/sum(num.dsets+num.lsets+num.ssets),
                                     catch=sum(dph.yft)) %>% filter(prop>=0.75) %>% 
  group_by(vesno) %>% mutate(Y1=min(year),Y2=max(year),N_Y=length(unique(year))) %>%
  ungroup %>%
  filter(N_Y>=10,Y2-Y1>=18) %>%
  select(year,vesno,prop,Y1,catch)

VID$vessel <- "1"

for(i in 1:nrow(VID)) {
  print(i)
  VID$vessel[i]<-paste0(toString(VID$Y1[i]),"-",toString(VID$vesno[i]))
}

f <- ggplot(data=VID) +
  geom_point(aes(x=year,y=factor(vessel),color=prop,size=catch)) +
  scale_color_distiller(palette = "Spectral") +
  theme_bw(15) +
  ylab("vessel")
ggsave(f,file="VID75.png",width = 10,height = 15)

#
data_final <- data %>% filter(year>1984) %>%
  group_by(year,vesno) %>% mutate(prop=sum(num.dsets)/sum(num.dsets+num.lsets+num.ssets),
                                     catch=sum(dph.yft)) %>% filter(prop>=0.75) %>% 
  group_by(vesno) %>% mutate(Y1=min(year),Y2=max(year),N_Y=length(unique(year))) %>%
  ungroup %>%
  filter(N_Y>=10,Y2-Y1>=18) %>%
  mutate(Catch_KG=(dph.yft+sch.yft+log.yft)/days.fishing)

Data_Geostat <-data.frame(Catch_KG = data_final$Catch_KG,
                          Year = data_final$Year, 
                          Year2 = ceiling(data_final$Year/4)+1974,
                          Vessel = data_final$vesno,
                          Lat = data_final$latc1,
                          AreaSwept_km2 = 1, Lon=data_final$lonc1,
                          latc5=data_final$latc5,lonc5=data_final$lonc5,
                          days.fishing=data_final$days.fishing,
                          prop=data_final$prop)

Data_Geostat <- Data_Geostat %>% group_by(Lat,Lon) %>%
  mutate(n=length(unique(Year2)),ymin=min(Year2),ymax=max(Year2)) %>%
  filter(n>=30)

write.csv(Data_Geostat,file="CAE75-Area.csv",row.names = F)


######

load("C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/YFT assessment/Data/DEL survey/base files_2000-2019_for SAC 2020.RData")
load("C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/YFT assessment/Data/DEL survey/base files_1975-1999_for SAC 2020.RData")
load("C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/YFT benchmark/DEL Data/R functions_processing P-S L-F.RData")


Y = 2000
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2000$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2000,vesnos.wellests.new,Y)
lf.2000<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2001
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2001$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2001,vesnos.wellests.new,Y)
lf.2001<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2002
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2002$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2002,vesnos.wellests.new,Y)
lf.2002<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2003
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2003$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2003,vesnos.wellests.new,Y)
lf.2003<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2004
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2004$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2004,vesnos.wellests.new,Y)
lf.2004<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2005
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2005$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2005,vesnos.wellests.new,Y)
lf.2005<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2006
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2006$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2006,vesnos.wellests.new,Y)
lf.2006<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2007
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2007$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2007,vesnos.wellests.new,Y)
lf.2007<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2008
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2008$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2008,vesnos.wellests.new,Y)
lf.2008<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2009
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2009$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2009,vesnos.wellests.new,Y)
lf.2009<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2010
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2010$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2010,vesnos.wellests.new,Y)
lf.2010<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2011
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2011$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2011,vesnos.wellests.new,Y)
lf.2011<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2012
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2012$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2012,vesnos.wellests.new,Y)
lf.2012<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2013
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2013$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2013,vesnos.wellests.new,Y)
lf.2013<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2014
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2014$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2014,vesnos.wellests.new,Y)
lf.2014<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2015
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2015$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2015,vesnos.wellests.new,Y)
lf.2015<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2016
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2016$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2016,vesnos.wellests.new,Y)
lf.2016<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2017
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2017$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2017,vesnos.wellests.new,Y)
lf.2017<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2018
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2018$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2018,vesnos.wellests.new,Y)
lf.2018<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 2019
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.2019$ancillary.info,Vnum$vesno)
tmpz<-get.summedLF.5deg.V2.f(well.estimates.2019,vesnos.wellests.new,Y)
lf.2019<-cbind(tmpz$ancil.info,tmpz$lf)
#################################
# #
# Y = 1975
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1975$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1975,vesnos.wellests.new,Y)
# lf.1975<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1976
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1976$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1976,vesnos.wellests.new,Y)
# lf.1976<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1977
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1977$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1977,vesnos.wellests.new,Y)
# lf.1977<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1978
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1978$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1978,vesnos.wellests.new,Y)
# lf.1978<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1979
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1979$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1979,vesnos.wellests.new,Y)
# lf.1979<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1980
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1980$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1980,vesnos.wellests.new,Y)
# lf.1980<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1981
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1981$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1981,vesnos.wellests.new,Y)
# lf.1981<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1982
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1982$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1982,vesnos.wellests.new,Y)
# lf.1982<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1983
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1983$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1983,vesnos.wellests.new,Y)
# lf.1983<-cbind(tmpz$ancil.info,tmpz$lf)
# #
# Y = 1984
# Vnum <- VID %>% filter(year==Y)
# vesnos.wellests.new<-mark.vesnos.f(well.estimates.1984$ancillary.info$yft,Vnum$vesno)
# tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1984,vesnos.wellests.new,Y)
# lf.1984<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1985
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1985$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1985,vesnos.wellests.new,Y)
lf.1985<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1986
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1986$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1986,vesnos.wellests.new,Y)
lf.1986<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1987
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1987$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1987,vesnos.wellests.new,Y)
lf.1987<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1988
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1988$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1988,vesnos.wellests.new,Y)
lf.1988<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1989
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1989$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1989,vesnos.wellests.new,Y)
lf.1989<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1990
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1990$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1990,vesnos.wellests.new,Y)
lf.1990<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1991
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1991$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1991,vesnos.wellests.new,Y)
lf.1991<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1992
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1992$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1992,vesnos.wellests.new,Y)
lf.1992<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1993
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1993$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1993,vesnos.wellests.new,Y)
lf.1993<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1994
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1994$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1994,vesnos.wellests.new,Y)
lf.1994<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1995
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1995$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1995,vesnos.wellests.new,Y)
lf.1995<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1996
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1996$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1996,vesnos.wellests.new,Y)
lf.1996<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1997
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1997$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1997,vesnos.wellests.new,Y)
lf.1997<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1998
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1998$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1998,vesnos.wellests.new,Y)
lf.1998<-cbind(tmpz$ancil.info,tmpz$lf)
#
Y = 1999
Vnum <- VID %>% filter(year==Y)
vesnos.wellests.new<-mark.vesnos.f(well.estimates.1999$ancillary.info$yft,Vnum$vesno)
tmpz<-get.summedLF.5deg.YFT7599.V2.f(well.estimates.1999,vesnos.wellests.new,Y)
lf.1999<-cbind(tmpz$ancil.info,tmpz$lf)
###
# yft.unwtdLF.19752018<-rbind(lf.1975,lf.1976,lf.1977,lf.1978,lf.1979,lf.1980,lf.1981,lf.1982,lf.1983,lf.1984,lf.1985,lf.1986,lf.1987,lf.1988,lf.1989,lf.1990,lf.1991,lf.1992,lf.1993,lf.1994,lf.1995,lf.1996,lf.1997,lf.1998,lf.1999,lf.2000,lf.2001,lf.2002,lf.2003,lf.2004,lf.2005,lf.2006,lf.2007,lf.2008,lf.2009,lf.2010,lf.2011,lf.2012,lf.2013,lf.2014,lf.2015,lf.2016,lf.2017,lf.2018)
yft.unwtdLF.19752019<-rbind(lf.1985,lf.1986,lf.1987,lf.1988,lf.1989,lf.1990,lf.1991,lf.1992,lf.1993,lf.1994,lf.1995,lf.1996,lf.1997,lf.1998,lf.1999,lf.2000,lf.2001,lf.2002,lf.2003,lf.2004,lf.2005,lf.2006,lf.2007,lf.2008,lf.2009,lf.2010,lf.2011,lf.2012,lf.2013,lf.2014,lf.2015,lf.2016,lf.2017,lf.2018,lf.2019)
yft.unwtdLF.19752019 <- yft.unwtdLF.19752019 %>%
  mutate(latc5=lat.5deg + 2.5,lonc5=lon.5deg - 2.5,Year=(year-1975)*4+quarter)

Data_Geostat_Grid <- unique(Data_Geostat[,c("latc5","lonc5")])
LF_temp <- left_join(Data_Geostat_Grid, yft.unwtdLF.19752019)

write.csv(LF_temp,file="YFT_5degQrtrSetype_unwtdLF_75percent dolphin sets_1975to2019-Area.csv",row.names = F)
###
