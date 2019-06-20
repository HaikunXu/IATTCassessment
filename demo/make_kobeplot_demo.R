library(IATTCassessment)

BasePath <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/External Review/Recruit/Base - SAC9/"
KobePath <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/External Review/Recruit/KobePlot/Base/"
year1 <- 1975

Kobe.Out <- make_kobetable(year1=year1, BasePath=BasePath, KobePath=KobePath)

make_kobeplot(Kobe.Out=Kobe.Out, Slim=c(0,6), Flim=c(0,1.6))
