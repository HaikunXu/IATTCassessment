setwd("C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/YFT assessment/Data/DEL survey")

library(TMB)               # Can instead load library(TMBdebug)
library(VAST)
library(tidyverse)

Version = "VAST_v4_0_0"

Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25
n_x = c(10, 50, 100, 200, 400)[4] # Number of stations
Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )

FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) 
RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) 
OverdispersionConfig = c(1,1)   
ObsModel = c(1,0)

Options =  c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=1,
             "Calculate_evenness"=0, "Calculate_effective_area"=1, "Calculate_Cov_SE"=0,
             'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)

strata.limits <- data.frame('STRATA'=c("EPO"))

Region = "Other"

DateFile = paste0(getwd(),"/DEL_YFT75-Area/")
dir.create(DateFile)

Record = ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
save( Record, file=file.path(DateFile,"Record.RData"))
capture.output( Record, file=paste0(DateFile,"Record.txt"))
# n <- length(yft.frm$yft.retained)

Data_Geostat <- read.csv("CAE75-Area.csv")

Data_Geostat <- Data_Geostat %>% filter(Lat>=5) %>% mutate(Year2=ceiling(Year/4)+1974)

### plot data distribution
# wmap <- map_data("world")
# f <- ggplot() +
#   geom_point(aes(x = Lon,y = Lat), data = Data_Geostat,size=0.5,shape=15, color="red") +
#   # scale_color_distiller(palette = "RdBu",name = "Density") +
#   # scale_color_gradient2(low="blue", high="red", mid="green",name="Density",midpoint = (max(exp(den))+min(exp(den)))/2) +
#   geom_polygon(data=wmap,aes(long, lat, group = group),fill = "black",colour = "white",alpha = 1,lwd=0.5) +
#   coord_quickmap(ylim = c(5,35),xlim = c(-150,-75)) +
#   # geom_vline(xintercept = -110,size=1) +
#   # geom_hline(yintercept = -10,size=1) +
#   facet_wrap( ~ Year2, nrow=5) +
#   theme_bw(12)
# ggsave(filename = "Data2.png", f, dpi = 300, width = 20,height = 10)

Nominal <- Data_Geostat %>% group_by(Year) %>% summarise(sum(Catch_KG*days.fishing)/sum(days.fishing))
write.csv(Nominal,"Nominal Index.csv",row.names = FALSE)

Extrapolation_List = make_extrapolation_info(Region=Region, zone = 12, strata.limits=strata.limits, observations_LL=Data_Geostat[,c('Lat','Lon')], grid_dim_km=c(20,20) )


Spatial_List = make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, DirPath=DateFile, Save_Results=FALSE )
plot_data(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile )

# Add knots to Data_Geostat
Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i)

TmbData = VAST::Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, "c_i"=rep(0,nrow(Data_Geostat)), "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )

TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method)
Obj = TmbList[["Obj"]]

Opt = TMBhelper::Optimize( obj=Obj, newtonsteps = 1,lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=TRUE)


Report = Obj$report()
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile,"Save.RData"))
# save.image(paste0(DateFile,"Result.RData"))

Enc_prob = plot_encounter_diagnostic( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)

MapDetails_List = make_map_info( "Region"=Region, "spatial_list"=Spatial_List, "Extrapolation_List"=Extrapolation_List )

Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )
# Dens_xt = plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

Index = plot_biomass_index(DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, use_biascorr=TRUE )
plot_range_index(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), PlotDir=DateFile, Year_Set=Year_Set)


Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, FileName_PP="Posterior_Predictive",
                              FileName_Phist="Posterior_Predictive-Histogram",
                              FileName_QQ="Q-Q_plot", FileName_Qhist="Q-Q_hist", DateFile=DateFile )

# We then plot Pearson residuals.  If there are visible patterns (areas with consistently positive or negative residuals accross or within years) then this is an indication of the model "overshrinking" results towards the intercept, and model results should then be treated with caution.  
# ```{r plot_pearson_resid, message=FALSE, warning=FALSE, tidy=TRUE, linewidth=50}
Res <- plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)

# residuals <- SpatialDeltaGLMM:::plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)


save.image(paste0(DateFile,"All.RData"))


##### Residual plot
# 
wmap <- map_data("world")
den <- Res$Q1_xy[,Years2Include,drop=FALSE]
data <- den[MapDetails_List$PlotDF[,'x2i'],,drop=FALSE]
Which = which( MapDetails_List$PlotDF[,'Include']>0 )
den <- data[Which,]

Den <- data.frame("residual"=den[seq(1,length(den))], "Lat" = MapDetails_List$PlotDF[Which,'Lat'], "Lon" = MapDetails_List$PlotDF[Which,'Lon'],
                  "year" = rep(Year_Set,each=length(den)/length(Years2Include)))

Den$residual[Den$residual>1.5] <- 1.5
Den$residual[Den$residual<(-1.5)] <- -1.5

f <- ggplot() +
  geom_point(aes(x = Lon,y = Lat,color=(residual)), data = Den,size=1,shape=15) +
  scale_color_distiller(palette = "RdBu",name = "Encounter\nProbability") +
  # scale_color_gradient2(low="blue", high="red", mid="green",name="Density",midpoint = (max(exp(den))+min(exp(den)))/2) +
  geom_polygon(data=wmap,aes(long, lat, group = group),fill = "black",colour = "white",alpha = 1,lwd=0.5) +
  coord_quickmap(ylim = c(5,30),xlim = c(-150,-80)) +
  # geom_vline(xintercept = -110,size=1) +
  # geom_hline(yintercept = -10,size=1) +
  facet_wrap( ~ year) +
  theme_bw(12)
ggsave(filename = "Encounter.png", f, dpi = 300, width = 28,height = 16)


den <- Res$Q2_xy[,Years2Include,drop=FALSE]
data <- den[MapDetails_List$PlotDF[,'x2i'],,drop=FALSE]
Which = which( MapDetails_List$PlotDF[,'Include']>0 )
den <- data[Which,]

Den <- data.frame("residual"=den[seq(1,length(den))], "Lat" = MapDetails_List$PlotDF[Which,'Lat'], "Lon" = MapDetails_List$PlotDF[Which,'Lon'],
                  "year" = rep(Year_Set,each=length(den)/length(Years2Include)))

Den$residual[Den$residual>1.5] <- 1.5
Den$residual[Den$residual<(-1.5)] <- -1.5

f <- ggplot() +
  geom_point(aes(x = Lon,y = Lat,color=(residual)), data = Den,size=1,shape=15) +
  scale_color_distiller(palette = "RdBu",name = "Catch\nRate") +
  # scale_color_gradient2(low="blue", high="red", mid="green",name="Density",midpoint = (max(exp(den))+min(exp(den)))/2) +
  geom_polygon(data=wmap,aes(long, lat, group = group),fill = "black",colour = "white",alpha = 1,lwd=0.5) +
  coord_quickmap(ylim = c(5,30),xlim = c(-150,-80)) +
  # geom_vline(xintercept = -110,size=1) +
  # geom_hline(yintercept = -10,size=1) +
  facet_wrap( ~ year) +
  theme_bw(12)
ggsave(filename = "CatchRate.png", f, dpi = 300, width = 28,height = 16)


wmap <- map_data("world")
den <- log(Report$D_xcy[,1,Years2Include,drop=FALSE])
data <- den[MapDetails_List$PlotDF[,'x2i'],1,,drop=FALSE]
Which = which( MapDetails_List$PlotDF[,'Include']>0 )
den <- data[Which,1,]

Den <- data.frame("residual"=den[seq(1,length(den))], "Lat" = MapDetails_List$PlotDF[Which,'Lat'], "Lon" = MapDetails_List$PlotDF[Which,'Lon'],
                  "year" = rep(Year_Set,each=length(den)/length(Years2Include)))

# Den$residual[Den$residual>1.5] <- 1.5
# Den$residual[Den$residual<(-1.5)] <- -1.5

f <- ggplot() +
  geom_point(aes(x = Lon,y = Lat,color=(residual)), data = Den,size=1,shape=15) +
  scale_color_distiller(palette = "RdBu",name = "log(Density)") +
  # scale_color_gradient2(low="blue", high="red", mid="green",name="Density",midpoint = (max(exp(den))+min(exp(den)))/2) +
  geom_polygon(data=wmap,aes(long, lat, group = group),fill = "black",colour = "white",alpha = 1,lwd=0.5) +
  coord_quickmap(ylim = c(5,30),xlim = c(-150,-80)) +
  # geom_vline(xintercept = -110,size=1) +
  # geom_hline(yintercept = -10,size=1) +
  facet_wrap( ~ year) +
  theme_bw(12)
ggsave(filename = "Density.png", f, dpi = 300, width = 28,height = 16)
