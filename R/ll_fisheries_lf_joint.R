#' Longline fisheries' length frequency
#' 
#' \code{ll_fisheries_lf_joint} This function processes the raw LL fisheries' length frequency data into the format for Stock Assessment
#' 
#' @export

ll_fisheries_lf_joint = function(JPN_size, KOR_size, Grid_Catch, Species, last_year, dir, period, legend, minNsamp = 0, Add_KOR = FALSE, LF_name = "LF", plot_map = FALSE) {
  
  ####
  # count data for JPN
  size_data <- JPN_size %>%
    filter(
      species == ifelse(Species == "BET", 4, 5), # YFT
      NGYO == 1, # commercial vessels
      ioc == 4, # EPO
      YY > 1974,
      YY <= last_year,
      MM > 0,
      level == 4, # 1by1 data
      M_unit %in% c(6, 7) # 1/2cm resolution data
      # place == 13 | YY <= 2010 # remove fishermen's LF during 2011-2014
    ) %>% 
    mutate(
      # CLS = ifelse(M_unit == 6, CLS - 1, CLS - 2),
      L = cut(
        CLS,
        breaks = c(seq(25, 195, 10), 245), # 20, 22, ......, 196, 198
        right = F,
        labels = seq(25, 195, 10)
      ),
      Lat = floor(Y / 5) * 5 + 2.5,
      Lon = floor((X - 360) / 5) * 5 + 2.5,
      Year = (YY - 1975) * 4 + ceiling(MM / 3)
    ) %>%
    filter(is.na(L) == FALSE) %>%
    group_by(Year, Lat, Lon, L) %>% summarise(count = n()) %>% # count number of fish
    group_by(Year, Lat, Lon) %>% mutate(count_sum = sum(count)) %>%
    filter(count_sum >= 15) %>% # a strata needs to have more than 20 fish measured
    data.frame()
  
  # count data for KOR
  if (Add_KOR == TRUE) {
    KOR_LF <- KOR_size %>%
      filter(Year <= last_year) %>%
      mutate(
        L = cut(
          Size - 1,
          breaks = c(seq(20, 198, 2), 500),
          right = F,
          labels = seq(20, 198, 2)
        ),
        Year = (Year - 1975) * 4 + ceiling(Quarter / 3),
        Lat = floor(Lat / 5) * 5 + 2.5,
        Lon = floor(Lon / 5) * 5 + 2.5
      ) %>%
      filter(is.na(L) == FALSE) %>%
      group_by(Year, Lat, Lon, L) %>%
      summarise(count = sum(Number)) %>%
      group_by(Year, Lat, Lon) %>%
      mutate(count_sum = sum(count)) %>%
      filter(count_sum >= 15) %>% # a strata needs to have more than 20 fish measured
      mutate(Flag = "KOR") %>%
      data.frame()
  }
  
  # combine JPN and KOR data
  if (Add_KOR == TRUE) {
    # if (catch_weighted == FALSE) {
    #   size_data$Flag = "JPN"
    #   size_data_a <- rbind(size_data, KOR_LF)
    #   size_data <- size_data_a %>%
    #     group_by(Year, Lat, Lon, L) %>%
    #     summarise(count = sum(count),
    #               count_sum = sum(count_sum))
    # }
    # else {
      size_data$Flag = "JPN"
      size_data_a <- rbind(size_data, KOR_LF)
      
      write.csv(size_data_a, file = paste0(dir, LF_name, "_grid_flag.csv"), row.names = FALSE) # save
      
      # compute proportional catch
      Grid_Catch_prop <- Grid_Catch %>% data.frame() %>%
        filter(SpeciesAbv == Species, FlagAbv %in% c("JPN", "KOR")) %>%
        mutate(Year = (Yrr - 1975) * 4 + Quarter) %>%
        select(Year, FlagAbv, Lat, Lon, SumOfNumber) %>%
        spread(FlagAbv, SumOfNumber, fill = 0) %>%
        gather(4:5, key = "Flag", value = "catch") %>%
        group_by(Year, Lat, Lon) %>%
        mutate(prop = catch / sum(catch)) %>%
        select(c(1:4, 6))
      
      size_data_a <- left_join(size_data_a, Grid_Catch_prop)
      
      size_data <- size_data_a %>%
        group_by(Year, Lat, Lon, L) %>%
        summarise(LF_raw = sum(count / count_sum * prop, na.rm = TRUE)) %>%
        group_by(Year, Lat, Lon) %>%
        mutate(LF=LF_raw/sum(LF_raw))
    # }
  } else {
    size_data$LF <- size_data$count / size_data$count_sum
    size_data_a <- size_data %>% mutate(Flag = "JPN")
  }
  
  size_data <- size_data %>%
    group_by(Lat, Lon) %>%
    mutate(nyear = length(unique(Year)))
    # filter(nyear > 3) # remove rarely sampled grids
  
  # compile and save gridded LF data
  grid_LF <-  size_data %>%
    # mutate(LF = count / count_sum) %>%
    select(Year, Lat, Lon, L, LF) %>%
    spread(L, LF, fill = 0)
  
  # fill in the missing 2-cm length bins with 0
  for (i in seq(25, 195, 10)) {
    if (as.character(i) %in% names(grid_LF) == FALSE)
      grid_LF[[as.character(i)]] <- 0
  }
  
  # final length frequency output
  grid_LF_final <-
    grid_LF %>% gather(as.character(seq(25, 195, 10)), key = L, value = lf) %>%
    mutate(L = floor(as.numeric(L)/10)*10) %>%
    group_by(Year,Lat,Lon,L) %>%
    summarise(LF=sum(lf))
  
  write.csv(grid_LF_final, file = paste0(dir, LF_name, "_grid.csv"), row.names = FALSE) # save
  
  #### Compute sample size
  sample_size <- size_data_a %>%
    mutate(Area = area_code(Lat, Lon, Species)) %>%
    group_by(Area, Year) %>%
    summarise(Nsamp = sum(count)) # sample size
  
  size_data_flag <- size_data_a %>%
    mutate(Area = area_code(Lat, Lon, Species), Year = Year/4 + 1974.875) %>%
    group_by(Area, Year, Flag) %>%
    summarise(Nsamp = sum(count)) # sample size by Flag
  
  ggplot(data = size_data_flag) +
    geom_col(aes(x = Year, y = Nsamp, fill = Flag)) +
    scale_shape_manual(values = c(3, 4)) + 
    facet_wrap( ~ Area, nrow = 6) +
    theme_bw(16) +
    ylab("Sample size")
  ggsave(
    filename = paste0(dir, "Sample size.png"),
    dpi = 300,
    width = 9,
    height = 10
  )
  ggsave(
    filename = paste0(dir, "Sample size.pdf"),
    dpi = 300,
    width = 9,
    height = 10
  )
  
  ### plot sample maps
  if(plot_map == TRUE) {
    wmap <- ggplot2::map_data("world")
    
    sample_grid <- size_data_a %>%
      mutate(Year = Year/4 + 1974.875) %>%
      filter(Year>2011)
    
    f <- ggplot() + geom_point(
      aes(x = Lon, y = Lat, color = Flag, shape = Flag),
      data = sample_grid, size = 2
    ) +
      scale_shape_manual(values = c(3, 4)) + 
      geom_polygon(
        data = wmap,
        aes(long, lat, group = group),
        fill = "black",
        colour = "white",
        alpha = 1,
        lwd = 0.5
      ) +
      coord_quickmap(ylim = c(-25, 10), xlim = c(-150,-75)) +
      facet_wrap( ~ Year, ncol = 4) +
      theme_bw(14)
    
    ggsave(
      filename = paste0(dir, Species, "_LF_Grids_Flag.png"),
      f,
      dpi = 300,
      width = 12,
      height = 15
    )
    ggsave(
      filename = paste0(dir, Species, "_LF_Grids_Flag.pdf"),
      f,
      dpi = 300,
      width = 12,
      height = 15
    )
  }
   
  # prepare LF in the format to be combined with catch data
  size_data_wide <- size_data %>%
    # mutate(LF = count / count_sum) %>%
    select(Year, Lat, Lon, L, LF) %>%
    spread(L, LF, fill = 0)
  
  size_catch_data <- size_data_wide %>%
    gather(names(size_data_wide)[4:ncol(size_data_wide)], "key" = Length, "value" = LF) %>%
    mutate(Length = as.numeric(Length))
  
  # plot the distribution of catch by decade
  Grid_Catch_plot <-
    Grid_Catch %>% filter(SpeciesAbv == Species, Yrr > 1974) %>%
    mutate(
      Flag = as.character(FlagAbv),
      Decade = paste0(round(Yrr / 10) * 10 - 5, "-", round(Yrr / 10) * 10 + 4),
      Flag = ifelse(Flag %in% c("JPN", "KOR", "TWN", "CHN"), Flag, "Others")
    ) %>%
    group_by(Flag, Decade, Lat, Lon) %>%
    summarise(Number = ifelse(
      sum(SumOfNumber, na.rm = TRUE) > 3e5,
      3e5,
      sum(SumOfNumber, na.rm = TRUE)
    ))
  
  wmap <- ggplot2::map_data("world")
  ggplot() + geom_point(
    aes(x = Lon, y = Lat, color = Number),
    data = Grid_Catch_plot,
    size = 4,
    shape = 15
  ) +
    geom_polygon(
      data = wmap,
      aes(long, lat, group = group),
      fill = "black",
      colour = "white",
      alpha = 1,
      lwd = 0.5
    ) +
    coord_quickmap(ylim = c(-40, 40), xlim = c(-150,-70)) + theme_bw(12) +
    facet_grid(Flag ~ Decade) +
    scale_color_distiller(palette = "Spectral", name = paste0(Species," catch"))
  
  ggsave(
    filename = paste0(dir, Species, "_Catch.png"),
    dpi = 300,
    width = 12,
    height = 8
  )
  
  # plot the proportion of bigeye catch
    Grid_Catch_plot2 <-
    Grid_Catch %>% filter(SpeciesAbv == Species, Yrr > 1978, Yrr < 2020, as.character(FlagAbv) %in% c("JPN", "KOR")) %>%
    mutate(Decade = ifelse(
      Yrr < 1994,
      "1979-1993",
      ifelse(Yrr > 2010, "2011-2019", "1994-2010")
    ), ) %>%
    group_by(Decade, Lat, Lon) %>%
    summarise(Catch = sum(SumOfNumber, na.rm = TRUE)) %>%
    group_by(Decade) %>%
    mutate(Catch2 = ifelse(Catch / sum(Catch) > 0.05, 0.05, Catch / sum(Catch)))
  
  ggplot() + geom_point(
    aes(x = Lon, y = Lat, color = Catch2 * 100),
    data = Grid_Catch_plot2,
    size = 4,
    shape = 15
  ) +
    geom_polygon(
      data = wmap,
      aes(long, lat, group = group),
      fill = "black",
      colour = "white",
      alpha = 1,
      lwd = 0.5
    ) +
    geom_segment(aes(
      x = Lon1,
      xend = Lon2,
      y = Lat1,
      yend = Lat2
    ),
    data = BET_LL,
    size = 1) +
    coord_quickmap(ylim = c(-40, 40), xlim = c(-150, -70)) + theme_bw(12) +
    facet_wrap(~ Decade) +
    scale_color_distiller(palette = "Spectral", name = paste0(Species, " catch (%)"))
  
  ggsave(
    filename = paste0(dir, Species, "_Catch_Prop.png"),
    dpi = 300,
    width = 10,
    height = 5
  )
  ggsave(
    filename = paste0(dir, Species, "_Catch_Prop.pdf"),
    dpi = 300,
    width = 10,
    height = 5
  )
  
  # plot the distribution of cpue by decade
  Grid_cpue_plot <-
    Grid_Catch %>% filter(SpeciesAbv == Species, Yrr > 1969, Yrr < 2020) %>%
    mutate(
      Flag = as.character(FlagAbv),
      Decade = paste0(floor(Yrr / 10) * 10, "-", floor(Yrr / 10) * 10 + 9),
      Flag = ifelse(Flag %in% c("JPN", "KOR", "TWN", "CHN"), Flag, "Others")
    ) %>%
    group_by(Flag, Decade, Lat, Lon) %>%
    summarise(CPUE = ifelse(
      sum(SumOfNumber, na.rm = TRUE) / sum(SumOfHooks) * 1000 > 20,
      20,
      sum(SumOfNumber, na.rm = TRUE) / sum(SumOfHooks) * 1000
    ))
  
  wmap <- ggplot2::map_data("world")
  ggplot() + geom_point(
    aes(x = Lon, y = Lat, color = CPUE),
    data = Grid_cpue_plot,
    size = 4,
    shape = 15
  ) +
    geom_polygon(
      data = wmap,
      aes(long, lat, group = group),
      fill = "black",
      colour = "white",
      alpha = 1,
      lwd = 0.5
    ) +
    coord_quickmap(ylim = c(-40, 40), xlim = c(-150,-70)) + theme_bw(12) +
    facet_grid(Flag ~ Decade) +
    scale_color_distiller(palette = "Spectral", name = paste0(Species," cpue"))
  
  ggsave(
    filename = paste0(dir, Species, "_CPUE.png"),
    dpi = 300,
    width = 12,
    height = 8
  )
  
  # compute total 5by5 catch across countries
  Grid_Catch_new <- Grid_Catch %>% data.frame() %>% filter(SpeciesAbv == Species) %>%
    mutate(Year = (Yrr - 1975) * 4 + Quarter) %>% group_by(Year, Lat, Lon) %>%
    summarise(Number = sum(SumOfNumber, na.rm = TRUE)) # compute total catch in number
  
  # combine LF with gridded catch data
  data <- left_join(size_catch_data, Grid_Catch_new)
  data$Area <- area_code(data$Lat, data$Lon, Species)
  
  ggplot() + geom_point(
    aes(x = Lon, y = Lat, color = factor(Area)),
    data = data,
    size = 6,
    shape = 15
  ) +
    geom_polygon(
      data = wmap,
      aes(long, lat, group = group),
      fill = "black",
      colour = "white",
      alpha = 1,
      lwd = 0.5
    ) + coord_quickmap(ylim = c(-40, 40), xlim = c(-150,-70)) + theme_bw(8)
  
  ggsave(
    filename = paste0(dir, "Areas_LF.png"),
    dpi = 300,
    width = 5,
    height = 5
  )
  
  # raise LF by total catch for each LL area
  data_area <-
    data %>% group_by(Area, Year, Length) %>% summarise(LF_final = sum(LF * Number)) %>%
    group_by(Area, Year) %>% mutate(Tol_LF_final = sum(LF_final)) %>% filter(Tol_LF_final > 0) %>%
    mutate(LF_final_ss = LF_final / Tol_LF_final) %>%
    select(Year, Area, Length, LF_final_ss) %>%
    spread(Length, LF_final_ss)
  
  # fill in the missing 2-cm length bins with 0
  for (i in seq(25, 195, 10)) {
    if (as.character(i) %in% names(data_area) == FALSE)
      data_area[[as.character(i)]] <- 0
  }
  
  # final length frequency output
  data_area_final <-
    data_area %>% gather(as.character(seq(25, 195, 10)), key = length, value = lf) %>%
    mutate(length = as.numeric(length)) %>% spread(length, lf) %>%
    arrange(Area, Year)
  
  # add sample size to the final length frequency output
  data_area_final <- left_join(data_area_final, sample_size)
  write.csv(sample_size, file = paste0(dir, "sample_size.csv"), row.names = FALSE) # save
  
  # prepare and save the final length frequency output based on SS format
  F_LF_SS <-
    data.frame(
      "Year" = data_area_final$Year,
      "Month" = 1,
      "Fleet" = data_area_final$Area,
      "sex" = 0,
      "par" = 0,
      "Nsamp" = data_area_final$Nsamp / 100
    )
  F_LF_SS <- cbind(F_LF_SS, data_area_final[3:20], data_area_final[3:20]) # male and female LF
  F_LF_SS <- data.frame(F_LF_SS) %>% filter(Nsamp >= minNsamp) # remove LF with sample size < minNsamp
  print(paste0("!!! Using a minimal sample size threshold of ", minNsamp, " !!!"))
  write.csv(F_LF_SS, file = paste0(dir, LF_name, ".csv"), row.names = FALSE) # save
  
  # check results
  # data_plot <- data_area_final %>% gather(as.character(seq(20,198,2)), key = length, value = lf) %>%
  #     mutate(length=as.numeric(length),period=ifelse(Year<81,"Early","Late")) %>% group_by(Area,length,period) %>%
  #     summarise(lf_mean=sum(lf*n)/sum(n))
  
  data_plot <-
    data_area_final %>% gather(as.character(seq(25, 195, 10)), key = length, value = lf) %>%
    mutate(
      Length = as.numeric(length),
      Period = cut(
        Year,
        breaks = c(-Inf, period, Inf),
        right = F,
        labels = legend
      )
    ) %>%
    group_by(Period, Area, Length) %>%
    summarise(lf_mean = sum(lf * Nsamp) / sum(Nsamp))
  
  ggplot(data = data_plot) +
    geom_smooth(aes(x = Length, y = lf_mean, color = Period),
                span = 0.25,
                se = FALSE) +
    theme_bw(16) +
    facet_wrap( ~ Area) +
    ylab("Fishery LF")
  ggsave(
    filename = paste0(dir, "LL Fisheries LF.png"),
    dpi = 300,
    width = 15,
    height = 12
  )
  ggsave(
    filename = paste0(dir, "LL Fisheries LF.pdf"),
    dpi = 300,
    width = 15,
    height = 12
  )
  
  return(data_area_final)
}