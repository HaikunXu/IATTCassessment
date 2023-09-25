#' Longline fisheries' length frequency
#' 
#' \code{ll_fisheries_lf_separate} This function processes the raw LL fisheries' length frequency data into the format for Stock Assessment
#' 
#' @export

ll_fisheries_lf_separate = function(JPN_size, KOR_size, Grid_Catch, Species, last_year, dir, period, legend, minNsamp = 0, LF_name = "LF", plot_map = FALSE, rounding = "low") {
  
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
      M_unit %in% c(6,7,8) # 1/2cm resolution data
      # place == 13 | YY <= 2010 # remove fishermen's LF during 2011-2014
    )
  
  if(rounding == "low") {
    JPN_LF <- size_data %>%
      mutate(
        # CLS = ifelse(M_unit == 6, CLS - 1, CLS - 2),
        L = cut(
          CLS,
          breaks = c(seq(60, 190, 10), 220),
          right = FALSE,
          labels = seq(60, 190, 10)
        ),
        Lat = floor(Y / 5) * 5 + 2.5,
        Lon = floor((X - 360) / 5) * 5 + 2.5,
        Year = (YY - 1975) * 4 + ceiling(MM / 3)
      ) %>%
      filter(is.na(L) == FALSE) %>%
      group_by(Year, Lat, Lon, L) %>% summarise(count = n()) %>% # count number of fish
      group_by(Year, Lat, Lon) %>% mutate(count_sum = sum(count)) %>%
      filter(count_sum >= 14) %>% # a strata needs to have more than 20 fish measured
      data.frame()
  }
    
  if(rounding == "high") {
    JPN_LF <- size_data %>%
      mutate(
        # CLS = ifelse(M_unit == 6, CLS - 1, CLS - 2),
        L = cut(
          CLS,
          breaks = c(seq(60, 190, 10), 220),
          right = TRUE,
          labels = seq(60, 190, 10)
        ),
        Lat = floor(Y / 5) * 5 + 2.5,
        Lon = floor((X - 360) / 5) * 5 + 2.5,
        Year = (YY - 1975) * 4 + ceiling(MM / 3)
      ) %>%
      filter(is.na(L) == FALSE) %>%
      group_by(Year, Lat, Lon, L) %>% summarise(count = n()) %>% # count number of fish
      group_by(Year, Lat, Lon) %>% mutate(count_sum = sum(count)) %>%
      filter(count_sum >= 14) %>% # a strata needs to have more than 20 fish measured
      data.frame()
  }
  
  # count data for KOR
  KOR_LF <- KOR_size %>%
    filter(Year <= last_year) %>%
    mutate(
      L = cut(
        Size - 1,
        breaks = c(seq(60, 190, 10), 220),
        right = F,
        labels = seq(60, 190, 10)
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
    filter(count_sum >= 14) %>% # a strata needs to have more than 20 fish measured
    mutate(Flag = "KOR") %>%
    data.frame()
  
  # sample-size
  JPN_size <- JPN_LF %>%
    mutate(Fleet = area_code(Lat, Lon, Species)) %>%
    group_by(Fleet, Year) %>%
    summarise(Nsamp = sum(count) / 100)
  
  KOR_size <- KOR_LF %>%
    mutate(Fleet = area_code(Lat, Lon, Species)) %>%
    group_by(Fleet, Year) %>%
    summarise(Nsamp = sum(count) / 100)
  
  # sample-weighted LF
  JPN <- JPN_LF %>%
    mutate(Fleet = area_code(Lat, Lon, Species),
           lf = count / count_sum) %>%
    group_by(Fleet, Year, L) %>%
    summarise(LF = sum(lf)) %>%
    group_by(Fleet, Year) %>%
    mutate(LF = LF / sum(LF)) %>%
    spread(L, LF, fill = 0)
  
  JPN <- left_join(JPN, JPN_size) %>%
    filter(Nsamp >= minNsamp)
  
  JPN_LF_SS <-
    data.frame(
      "Year" = JPN$Year,
      "Month" = 1,
      "Fleet" = JPN$Fleet,
      "sex" = 0,
      "par" = 0,
      "Nsamp" = JPN$Nsamp
    )
  
  JPN_LF_SS <- cbind(JPN_LF_SS, JPN[3:16], JPN[3:16]) # male and female LF
  write.csv(JPN_LF_SS, file = paste0(dir, "JPN-sample_weighted.csv"), row.names = FALSE)
  
  
  KOR <- KOR_LF %>%
    mutate(Fleet = area_code(Lat, Lon, Species),
           lf = count / count_sum) %>%
    group_by(Fleet, Year, L) %>%
    summarise(LF = sum(lf)) %>%
    group_by(Fleet, Year) %>%
    mutate(LF = LF / sum(LF)) %>%
    spread(L, LF, fill = 0)
  
  KOR <- left_join(KOR, KOR_size) %>%
    filter(Nsamp >= minNsamp)
  
  KOR_LF_SS <-
    data.frame(
      "Year" = KOR$Year,
      "Month" = 1,
      "Fleet" = KOR$Fleet + 5,
      "sex" = 0,
      "par" = 0,
      "Nsamp" = KOR$Nsamp
    )
  
  KOR_LF_SS <- cbind(KOR_LF_SS, KOR[3:16], KOR[3:16]) # male and female LF
  write.csv(KOR_LF_SS, file = paste0(dir, "KOR-sample_weighted.csv"), row.names = FALSE)

  
  # Catch data
  JPN_catch <- Grid_Catch %>% data.frame() %>%
    filter(SpeciesAbv == Species, FlagAbv %in% c("JPN")) %>%
    mutate(Year = (Yrr - 1975) * 4 + Quarter) %>%
    select(Year, Lat, Lon, SumOfNumber)
    
  KOR_catch <- Grid_Catch %>% data.frame() %>%
    filter(SpeciesAbv == Species, FlagAbv %in% c("KOR")) %>%
    mutate(Year = (Yrr - 1975) * 4 + Quarter) %>%
    select(Year, Lat, Lon, SumOfNumber)
  
  # catch-weighted LF
  JPN_LF <- JPN_LF %>%
    mutate(lf = count / count_sum)
  
  JPN <- left_join(JPN_catch, JPN_LF) %>%
    na.omit() %>%
    mutate(Fleet = area_code(Lat, Lon, Species)) %>%
    group_by(Fleet, Year, L) %>%
    summarise(LF=sum(SumOfNumber * lf)) %>%
    group_by(Fleet, Year) %>%
    mutate(LF = LF / sum(LF)) %>%
    spread(L, LF, fill = 0)
           
  JPN <- left_join(JPN, JPN_size) %>%
    filter(Nsamp >= minNsamp)
  
  JPN_LF_SS <-
    data.frame(
      "Year" = JPN$Year,
      "Month" = 1,
      "Fleet" = JPN$Fleet,
      "sex" = 0,
      "par" = 0,
      "Nsamp" = JPN$Nsamp
    )
  
  JPN_LF_SS <- cbind(JPN_LF_SS, JPN[3:16], JPN[3:16]) # male and female LF
  write.csv(JPN_LF_SS, file = paste0(dir, "JPN-catch_weighted.csv"), row.names = FALSE)
  
  
  KOR_LF <- KOR_LF %>%
    mutate(lf = count / count_sum)
  
  KOR <- left_join(KOR_catch, KOR_LF) %>%
    na.omit() %>%
    mutate(Fleet = area_code(Lat, Lon, Species)) %>%
    group_by(Fleet, Year, L) %>%
    summarise(LF=sum(SumOfNumber * lf)) %>%
    group_by(Fleet, Year) %>%
    mutate(LF = LF / sum(LF)) %>%
    spread(L, LF, fill = 0)
  
  KOR <- left_join(KOR, KOR_size) %>%
    filter(Nsamp >= minNsamp)
  
  KOR_LF_SS <-
    data.frame(
      "Year" = KOR$Year,
      "Month" = 1,
      "Fleet" = KOR$Fleet + 5,
      "sex" = 0,
      "par" = 0,
      "Nsamp" = KOR$Nsamp
    )
  
  KOR_LF_SS <- cbind(KOR_LF_SS, KOR[3:16], KOR[3:16]) # male and female LF
  write.csv(KOR_LF_SS, file = paste0(dir, "KOR-catch_weighted.csv"), row.names = FALSE)
}