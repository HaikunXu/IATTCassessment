# set working directory
Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/stock assessment/Spatial Model/LL Catch/2/"

# load both gridded data and FSR data
Grid_Catch <- read.table(paste0(Dir,"Tb2c_CatchBET&YFT_AreasLL_newAreas.txt"), header = TRUE, sep = ",")
FSR_Catch <- read.table(paste0(Dir,"Tb3_AnnualCatchBET_LL&otherGears.txt"), header = TRUE, sep = ",")
Grid_Catch <- data.frame(Grid_Catch)
FSR_Catch <- data.frame(FSR_Catch)


LLcatch = LLcatch(Grid_Catch=Grid_Catch, FSR_Catch=FSR_Catch, Species="BET",last_year=2017.75)

# this is comment to add at least 1 new character to my commit