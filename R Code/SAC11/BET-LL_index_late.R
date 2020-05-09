#############################################################
### VAST model for BET LL late (1995-2019) index of abundance for the IATTC SAC 11-06 BET document
### R code writen by Haikun Xu (hkxu@iattc.org); LL data provided by Keisuke Satoh (kstu21@fra.affrc.go.jp) 
### Last modified ion 5/8/2020
#############################################################

Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS data/LL CPUE/"
setwd(Dir)

# load the Japanese longline catch effort dataset (updated on March 35, 2020)
load("C:/Users/hkxu/Desktop/LL/JPN/20200325MOU/CE/Data/MOU_CE_20200325.RData")

library(tidyverse) # version 1.2.1
library(VAST) # version 3.0.0
library(TMB) # version 1.7.14
library(IATTCassessment) # version 1.3.1

# data processing
data <- ce.d %>%
  filter(NGYO == 1, # commercial vessel
         ioc == 4, # EPO
         newCAL != -9999, # Remove the observations without vessel id
         YY > 1978 # Remove the observations prior to 1979
  ) %>%
  mutate(Year = (YY - 1975) * 4 + ceiling(MM / 3), # model year
         Catch_KG = bigeye / hooks * 1000) # bigeye

Data <- data.frame(
  "Year" = data$Year,
  "Catch_KG" = data$Catch_KG,
  "Lat" = data$Y,
  "Lon" = data$X - 360,
  "Vessel" = as.factor(data$newCAL), # vessel ID
  "AreaSwept_km2" = 1,
  "HBF" = data$NHBF,
  "Hooks" = data$hooks
)

Data <- Data %>% filter(HBF>=5,HBF<=25) %>%
  filter(!(Lat>10 & Lon<(-110))) # remove data in area 1

# The final data for the index standardization
# select the grids and vessels with at least 80 and 40 quarters of data, respectively
Data_Geostat <- data.frame(select_data(Data,0,40,0,80)) %>% filter(!(abs(Lat)<5&Lon>-110),Lat>-20)
summary(Data_Geostat)


#### VAST estimation
Version = "VAST_v4_0_0"

Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25
n_x = c(3, 20, 50, 100, 200)[5] # Number of stations
Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )

FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # include both spatial and spatiotemporal effects
RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) 
OverdispersionConfig = c("Delta1"=1, "Delta2"=1) # include vessel effects
ObsModel = c(1,3)

Options =  c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=1,
             "Calculate_evenness"=0, "Calculate_effective_area"=1, "Calculate_Cov_SE"=0,
             'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)


strata.limits <- data.frame('STRATA'=c("EPO"))

Region = "Other"

DateFile = paste0(Dir,"BET_vessel_Late_HBF_Area/")
dir.create(DateFile) # create a directory to save results

# generate the spatial grids for the standardization

lat_North <- Data_Geostat$Lat > 0
Data_Geostat_North <- Data_Geostat[lat_North,]
Extrapolation_List_North = make_extrapolation_info(Region=Region, zone = 12, strata.limits=strata.limits, observations_LL=Data_Geostat_North[,c('Lat','Lon')], grid_dim_km=c(20,20) )

lat_South <- Data_Geostat$Lat < 0
Data_Geostat_South <- Data_Geostat[lat_South,]
Extrapolation_List_South = make_extrapolation_info(Region=Region, zone = 12, strata.limits=strata.limits, observations_LL=Data_Geostat_South[,c('Lat','Lon')], grid_dim_km=c(20,20) )

Extrapolation_List_South$Data_Extrap$N_km <- Extrapolation_List_South$Data_Extrap$N_km - 10000

a_el <- rbind(Extrapolation_List_North$a_el,Extrapolation_List_South$a_el)
Data_Extrap <- rbind(Extrapolation_List_North$Data_Extrap,Extrapolation_List_South$Data_Extrap)
zone <- Extrapolation_List_North$zone
flip_around_dateline <- Extrapolation_List_North$flip_around_dateline
Area_km2_x <- c(Extrapolation_List_North$Area_km2_x,Extrapolation_List_South$Area_km2_x)

Extrapolation_List <- list("a_el" = a_el, "Data_Extrap" = Data_Extrap, "zone" = zone,
                           "flip_around_dateline" = flip_around_dateline,
                           "Area_km2_x" = Area_km2_x)

Spatial_List = make_spatial_info_EPO( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, DirPath=DateFile, Save_Results=FALSE )
plot_data(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile )

# Add knots to Data_Geostat
Data_Geostat_Grid <- unique(Data_Geostat[,c("Lat","Lon")])
Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i) %>% filter(Year>80,Year<180) # 1995 quarter1-2019 quarter 3

save(Data_Geostat,Data_Geostat_Grid,file=paste0(DateFile,"Late_Data.RData")) # save model inputs

cov <- cbind((Data_Geostat$HBF-mean(Data_Geostat$HBF))/sd(Data_Geostat$HBF)) # HBF's linear effect on catchability

# run the model
TmbData = VAST::Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "c_i"=rep(0,nrow(Data_Geostat)), "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options, "Q_ik"=cov)
TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method)
Obj = TmbList[["Obj"]]
Opt = TMBhelper::Optimize( obj=Obj, newtonsteps = 1,lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=TRUE)

#save model outputs
Report = Obj$report()
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile,"Save.RData"))

# encounter probability diagnostics
Enc_prob = SpatialDeltaGLMM::Check_encounter_prob( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)
# qq-plot diagnostics
Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, FileName_PP="Posterior_Predictive",
                              FileName_Phist="Posterior_Predictive-Histogram",
                              FileName_QQ="Q-Q_plot", FileName_Qhist="Q-Q_hist", DateFile=DateFile )

# Get region-specific settings for plots
MapDetails_List = make_map_info( "Region"=Region, "spatial_list"=Spatial_List, "Extrapolation_List"=Extrapolation_List )
# Decide which years to plot                                                   
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
# generate the index
Index = plot_biomass_index( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, use_biascorr=TRUE)
# plot range shift
Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), PlotDir=DateFile, Year_Set=Year_Set)