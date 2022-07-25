#' Longline fisheries' length frequency
#' 
#' \code{ll_fisheries_lf_new} This function processes the raw LL fisheries' length frequency data into the format for Stock Assessment
#' 
#' @export

ll_fisheries_lf_new = function(JPN_size, Grid_Catch, Species, last_year, dir, minNsamp = 1, period, legend) {
  
  #### Compute sample size
  size_data0 <- JPN_size %>%
    mutate(X=X-360) %>%
    filter(species == ifelse(Species=="BET",4,5), # YFT
           NGYO == 1, # commercial vessels
           ioc == 4, # EPO
           YY > 1974,
           YY <= last_year,
           MM > 0,
           level == 4, # 1by1 data
           M_unit %in% c(6,7), # 1/2cm resolution data
           place == 13 | YY <= 2010) %>% # remove fishermen's LF during 2011-2014
    mutate(Year = (YY - 1975) * 4 + ceiling(MM / 3)) %>%
    group_by(X,Y) %>% mutate(N=length(unique(YY))) %>%
    filter(N>3) # remove rarely sampled grids
  
  size_data0$Area <- area_code(size_data0$Y, size_data0$X, Species)
  sample_size <- size_data0 %>% group_by(Area,Year) %>%
    summarise(n = n()) # sample size
  
  ggplot(data=sample_size) +
    geom_point(aes(x=Year,y=n,color=factor(Area))) +
    theme_bw(12) +
    ylab("Sample size")
  ggsave(filename = paste0(dir,"Sample size.png"), dpi = 300, width = 8, height = 5)
  
  
  ####
  # count data
  size_data <- JPN_size %>%
    filter(species == ifelse(Species=="BET",4,5), # YFT
           NGYO == 1, # commercial vessels
           ioc == 4, # EPO
           YY > 1974,
           YY <= last_year,
           MM > 0,
           level == 4, # 1by1 data
           M_unit %in% c(6,7), # 1/2cm resolution data
           place == 13 | YY <= 2010) %>% # remove fishermen's LF during 2011-2014
    mutate(
      CLS = ifelse(M_unit == 6, CLS - 1, CLS - 2),
      L = cut(
        CLS,
        breaks = c(20, seq(22, 198, 2), 400), # 20, 22, ......, 196, 198
        right = F,
        labels = seq(20, 198, 2)
      ),
      Lat = floor(Y/5)*5 + 2.5,
      Lon = floor((X-360)/5)*5 + 2.5,
      Year = (YY - 1975) * 4 + ceiling(MM / 3)) %>%
    group_by(X,Y) %>% mutate(N=length(unique(YY))) %>%
    filter(N>3) %>% # remove rarely sampled grids
    filter(is.na(L) == FALSE) %>%
    group_by(Year, Lat, Lon, L) %>% summarise(count = n()) %>% # count number of fish
    group_by(Year, Lat, Lon) %>% mutate(count_sum = sum(count)) %>%
    filter(count_sum>10) %>% # a strata needs to have more than 10 fish measured
    mutate(LF = count / count_sum) %>% # 1 by 1 LF
    select(Year, Lat, Lon, L, LF) %>%
    spread(L, LF, fill = 0) # spread length bins into column
  
  
  # combine LF and catch data
  size_catch_data <- size_data %>% 
    gather(names(size_data)[4:ncol(size_data)],"key"=Length,"value"=LF) %>% 
    mutate(Length = as.numeric(Length))
  
  
  # compute total 5by5 catch across countries
  Grid_Catch <- data.frame(Grid_Catch) %>% filter(SpeciesAbv==Species) %>%
    mutate(Year = (Yrr - 1975) * 4 + Quarter) %>% group_by(Year,Lat,Lon) %>%
    summarise(Number=sum(SumOfNumber,na.rm = TRUE)) # compute total catch in number
  
  # combine raised JPN 5by5 LF with gridded catch data
  data <- left_join(size_catch_data,Grid_Catch)
  data$Area <- area_code(data$Lat, data$Lon, Species)
  
  wmap <- ggplot2::map_data("world")
  ggplot() + geom_point(aes(x = Lon, y = Lat, color = factor(Area)), data = data, size = 6, shape = 15) + 
    geom_polygon(data = wmap, aes(long, lat, group = group), fill = "black", colour = "white", 
                 alpha = 1, lwd = 0.5) + coord_quickmap(ylim = c(-40, 40), xlim = c(-150, -70)) + theme_bw(8)
  
  ggsave(filename = paste0(dir,"Areas_LF.png"), dpi = 300, width = 5, height = 5)
  
  # raise LF by total catch for each LL area
  data_area <- data %>% group_by(Area,Year,Length) %>% summarise(LF_final=sum(LF*Number)) %>%
    group_by(Area,Year) %>% mutate(Tol_LF_final=sum(LF_final)) %>% filter(Tol_LF_final>0) %>%
    mutate(LF_final_ss=LF_final/Tol_LF_final) %>%
    select(Year,Area,Length,LF_final_ss) %>%
    spread(Length,LF_final_ss)
  
  # fill in the missing 2-cm length bins with 0
  for (i in seq(20,198,2)) {
    if(as.character(i) %in% names(data_area)==FALSE) data_area[[as.character(i)]] <- 0
  }
  
  # final length frequency output
  data_area_final <- data_area %>% gather(as.character(seq(20,198,2)), key = length, value = lf) %>%
    mutate(length=as.numeric(length)) %>% spread(length,lf) %>%
    arrange(Area,Year)
  
  # add sample size to the final length freqeuncy output
  data_area_final <- left_join(data_area_final,sample_size)
  
  # prepare and save the final length frequency output based on SS format
  F_LF_SS <- data.frame("Year"=data_area_final$Year,"Month"=1,"Fleet"=data_area_final$Area,
                        "sex"=0, "par"=0, "Nsamp" = data_area_final$n/100)
  F_LF_SS <- cbind(F_LF_SS,data_area_final[3:92],data_area_final[3:92]) # male and female LF
  F_LF_SS <- data.frame(F_LF_SS) %>% filter(Nsamp>=minNsamp) # remove LF with sample size < minNsamp
  print(paste0("!!! Using a minimal sample size threshold of ",minNsamp, " !!!"))
  write.csv(F_LF_SS,file = paste0(dir,"LF.csv"),row.names = FALSE) # save
  
  # check results
  # data_plot <- data_area_final %>% gather(as.character(seq(20,198,2)), key = length, value = lf) %>%
  #     mutate(length=as.numeric(length),period=ifelse(Year<81,"Early","Late")) %>% group_by(Area,length,period) %>%
  #     summarise(lf_mean=sum(lf*n)/sum(n))
  
  data_plot <- data_area_final %>% gather(as.character(seq(20,198,2)), key = length, value = lf) %>%
    mutate(Length=as.numeric(length),
           Period = cut(Year, breaks = c(-Inf, period, Inf), right = F, labels = legend)) %>%
    group_by(Period,Area,Length) %>%
    summarise(lf_mean=sum(lf*n)/sum(n))
  
  ggplot(data=data_plot) +
    geom_smooth(aes(x=Length,y=lf_mean,color=Period),span = 0.2,se = FALSE) +
    # geom_line(aes(x=length,y=lf_mean,color=factor(Area),linetype=period),alpha=0.25) +
    theme_bw(16) +
    facet_wrap(~Area) +
    ylab("Fishery LF")
  ggsave(filename = paste0(dir,"LL Fisheries LF.png"), dpi = 300, width = 15, height = 12)
  
  return(data_area_final)
}
