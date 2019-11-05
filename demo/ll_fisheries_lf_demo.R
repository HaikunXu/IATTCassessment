library(IATTCassessment)

load("C:/Users/hkxu/Desktop/LL/JPN/ce/Data/MOU_CE_20190727.RData")
load("C:/Users/hkxu/Desktop/LL/JPN/size/Data/MOU_size_20190727.RData")
Grid_Catch <- read.table("C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/YFT benchmark/LL F LF/Tb2c_CatchBET&YFT_AreasLL_newAreas.txt", header = TRUE, sep = ",") # Gridded 5by5 catch data


dir_output <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/YFT benchmark/LL F LF/"
LLcatch = ll_fisheries_lf(JPN_size = size, JPN_ce = ce.d, Grid_Catch=Grid_Catch, Species="YFT", last_year=2018, dir=dir_output)

dir_output <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/BET benchmark/LL F LF/"
LLcatch = ll_fisheries_lf(JPN_size = size, JPN_ce = ce.d, Grid_Catch=Grid_Catch, Species="BET", last_year=2018, dir=dir_output)
