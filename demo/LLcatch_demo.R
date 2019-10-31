library(IATTCassessment)
# data directory
dir_input <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/stock assessment/Spatial Model/LL Catch/2/"
# load both gridded data and FSR data
Grid_Catch <- read.table(paste0(dir_input,"Tb2c_CatchBET&YFT_AreasLL_newAreas.txt"), header = TRUE, sep = ",")
FSR_Catch <- read.table(paste0(dir_input,"Tb3_AnnualCatchBET_LL&otherGears.txt"), header = TRUE, sep = ",")
Grid_Catch <- data.frame(Grid_Catch)
FSR_Catch <- data.frame(FSR_Catch)
# ourput directory
dir_output <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/stock assessment/Spatial Model/LL Catch/2/"
# allocate longline catch for BET
LLcatch = ll_catch(Grid_Catch=Grid_Catch, FSR_Catch=FSR_Catch, Species="BET",last_year=2017.75, dir=dir_output)

dir_output <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/YFT benchmark/LL Catch/"
LLcatch = ll_catch(Grid_Catch=Grid_Catch, FSR_Catch=FSR_Catch, Species="YFT",last_year=2017.75, dir=dir_output)
