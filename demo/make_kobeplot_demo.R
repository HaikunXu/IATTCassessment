library(IATTCassessment)

BasePath <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/External Review/Recruit/Base - SAC9/"
KobePath <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2019/External Review/Recruit/KobePlot/Base/"
year1 <- 1975

Kobe.Out <- make_kobetable(year1=year1, BasePath=BasePath, KobePath=KobePath)

# # STD
# replist = r4ss::SS_output(dir=BasePath, ncols=400, covar=F)
# Table <- makeManagTable(replist, BasePath)
# 
# Fmult_scale <- Table$FmultScale
# STD_Table <- data.frame(read.table(file = paste0(BasePath,"ss.std"),header = TRUE))
# f_index <- which(STD_Table$name=="Mgmt_quant"&STD_Table$value>0)
# F_mult <- STD_Table$value[f_index[14]]
# F_mult_SD <- STD_Table$std.dev[f_index[14]]
# F_mult_recentSD <- sqrt(F_mult_SD^2*(1/Fmult_scale)^2)
# F_mult_last <- Kobe.Out$MSYtableOut[nrow(Kobe.Out$MSYtableOut),ncol(Kobe.Out$MSYtableOut)]
# F_recent_low <- 1/(F_mult_last-2*F_mult_last*F_mult_recentSD)
# F_recent_high <- 1/(F_mult_last+2*F_mult_last*F_mult_recentSD)
# 
# sb_index <- which(STD_Table$name=="SPR_std")

make_kobeplot(Kobe.Out=Kobe.Out, Slim=c(0,6), Flim=c(0,1.6), sd_f = 0.1, sd_b = 0.2)
