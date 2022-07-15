#' Longline catch allocation
#' 
#' \code{ll_catch} This function processes the raw LL catch data into the format for Stock Assessment
#' 
#' @export

ll_catch = function(Grid_Catch, FSR_Catch, Species, last_year, dir) {
    # make sure area code starts from 1
    
    Grid_Catch$NewAreas <- area_code(Grid_Catch$Lat, Grid_Catch$Lon, Species = Species)
    data <- data.frame(Lat = Grid_Catch$Lat, Lon = Grid_Catch$Lon, Areas = as.factor(Grid_Catch$NewAreas))
    
    wmap <- ggplot2::map_data("world")
    ggplot() + geom_point(aes(x = Lon, y = Lat, color = Areas), data = data, size = 6, shape = 15) +
      geom_polygon(data = wmap, 
        aes(long, lat, group = group), fill = "black", colour = "white", alpha = 1, lwd = 0.5) + coord_quickmap(ylim = c(-40, 
        40), xlim = c(-150, -70)) + theme_bw(8)
    
    ggsave(filename = paste0(dir,"Areas.png"), dpi = 300, width = 5, height = 5)
    
    # specify the countries to be analyzed in the LL catch allocation
    Countries <- as.character(unique(Grid_Catch$FlagAbv))
    # Countries <- c('KOR')
    Special_Countires <- c("KOR", "TWN")
    
    n_areas = length(unique(Grid_Catch$NewAreas))
    
    save_all <- matrix(0, nrow = 0, ncol = 1 + n_areas * 2)  # save all the LL catch into this matrix
    
    
    for (c in 1:length(Countries)) {
        # deal with the allocation by country
        
        # reformat the gridded and FSR data into quarterly and annual values
        Grid_weight_quarterly <- Grid_Catch %>% filter(FlagAbv == Countries[c], SpeciesAbv == Species) %>% mutate(YQ = Yrr + 
            (Quarter - 1)/4) %>% group_by(YQ, NewAreas) %>% summarise(Tot_Weight_Sum = sum(SumOfWeight, na.rm = TRUE)) %>% 
            spread(key = NewAreas, value = Tot_Weight_Sum, fill = 0)
        
        Grid_number_quarterly <- Grid_Catch %>% filter(FlagAbv == Countries[c], SpeciesAbv == Species) %>% mutate(YQ = Yrr + 
            (Quarter - 1)/4) %>% group_by(YQ, NewAreas) %>% summarise(Tot_Number_Sum = sum(SumOfNumber, na.rm = TRUE)) %>% 
            spread(key = NewAreas, value = Tot_Number_Sum, fill = 0)
        
        Grid_number_annual <- Grid_Catch %>% filter(FlagAbv == Countries[c], SpeciesAbv == Species) %>% group_by(Yrr, 
            NewAreas) %>% summarise(Tot_Number_Sum = sum(SumOfNumber, na.rm = TRUE)) %>% group_by(Yrr) %>% mutate(EPO = sum(Tot_Number_Sum, 
            na.rm = TRUE)) %>% spread(key = NewAreas, value = Tot_Number_Sum, fill = 0)
        
        Grid_weight_annual <- Grid_Catch %>% filter(FlagAbv == Countries[c], SpeciesAbv == Species) %>% group_by(Yrr, 
            NewAreas) %>% summarise(Tot_Weight_Sum = sum(SumOfWeight, na.rm = TRUE)) %>% group_by(Yrr) %>% mutate(EPO = sum(Tot_Weight_Sum, 
            na.rm = TRUE)) %>% spread(key = NewAreas, value = Tot_Weight_Sum, fill = 0)
        
        # calculate the prop (in weight and number) by quarter by area
        Grid_prop_quarterly <- Grid_Catch %>% filter(FlagAbv == Countries[c], SpeciesAbv == Species) %>% group_by(Yrr, 
            Quarter, NewAreas) %>% summarise(Tot_Weight_Sum = sum(SumOfWeight, na.rm = TRUE)) %>% group_by(Yrr) %>% 
            mutate(Prop = Tot_Weight_Sum/sum(Tot_Weight_Sum, na.rm = TRUE)) %>% select(Yrr, Quarter, NewAreas, Prop) %>% 
            spread(key = NewAreas, value = Prop, fill = 0) %>% mutate(YQ = Yrr + (Quarter - 1)/4)
        
        Grid_prop_num_quarterly <- Grid_Catch %>% filter(FlagAbv == Countries[c], SpeciesAbv == Species) %>% group_by(Yrr, 
            Quarter, NewAreas) %>% summarise(Tot_Number_Sum = sum(SumOfNumber, na.rm = TRUE)) %>% group_by(Yrr) %>% 
            mutate(Prop = Tot_Number_Sum/sum(Tot_Number_Sum, na.rm = TRUE)) %>% select(Yrr, Quarter, NewAreas, Prop) %>% 
            spread(key = NewAreas, value = Prop, fill = 0) %>% mutate(YQ = Yrr + (Quarter - 1)/4)
        
        # if the data do not include some areas for the entire period, add 0 catch to those areas to make the format of
        # the dataset consistent among countries
        if (ncol(Grid_number_annual) < (2 + n_areas)) {
            for (a in 1:n_areas) {
                if (!toString(a) %in% names(Grid_number_annual)) {
                  Grid_number_annual[[toString(a)]] <- 0
                  Grid_number_quarterly[[toString(a)]] <- 0
                  Grid_weight_annual[[toString(a)]] <- 0
                  Grid_weight_quarterly[[toString(a)]] <- 0
                  Grid_prop_quarterly[[toString(a)]] <- 0
                  Grid_prop_num_quarterly[[toString(a)]] <- 0
                }
            }
        }
        
        # CHN has one PS catch data in 2001, assume it is LL catch
        FSR_annual <- FSR_Catch %>% filter(FlagAbv == Countries[c], SpeciesAbv == Species) %>% group_by(Year) %>% 
            summarise(mt = sum(mt))
        
        # flag = 0 (have gridded number data); 1 (gridded weight data < FSR, need to do allocation); 2 (gridded weight
        # data > FSR, use gridded weight data); 3 (no gridded number or weight data, need to do allowcation);
        
        allocation_flag <- rep(3, nrow(FSR_annual))
        flag_id <- FSR_annual$Year %in% Grid_number_annual$Yrr
        if (Countries[c] %in% Special_Countires) {
            allocation_flag[flag_id == TRUE] <- ifelse(Grid_number_annual$EPO > 0, ifelse(Grid_weight_annual$EPO > 
                0 & Grid_weight_annual$EPO < FSR_annual[which(FSR_annual$Year %in% Grid_weight_annual$Yrr), "mt"], 
                1, 0), ifelse(Grid_weight_annual$EPO > 0, ifelse(Grid_weight_annual$EPO < FSR_annual[which(FSR_annual$Year %in% 
                Grid_weight_annual$Yrr), "mt"], 1, 2), 3))
        } 
        else {
            allocation_flag[flag_id == TRUE] <- ifelse(Grid_number_annual$EPO > 0, 0, ifelse(Grid_weight_annual$EPO > 
                0, ifelse(Grid_weight_annual$EPO < FSR_annual[which(FSR_annual$Year %in% Grid_weight_annual$Yrr), 
                "mt"], 1, 2), 3))
        }
        # a quick look at the allocation flag vector plot(FSR_annual$Year,allocation_flag,main=Countries[c])
        print(Countries[c])
        print(FSR_annual$Year[flag_id])
        print(allocation_flag)
        
        # create a new data.frame to store the allocation values
        YQ = seq(FSR_annual$Year[1], last_year, 0.25)
        allocation <- data.frame(YQ = rep(YQ), Year = floor(YQ), Quarter = rep(1:4, length(YQ)/4))
        
        allocation_weight <- data.frame(matrix(NA, nrow = length(YQ), ncol = n_areas))
        names(allocation_weight) <- paste0("W", seq(1, n_areas))
        allocation <- cbind(allocation, allocation_weight)
        
        for (i in 1:length(FSR_annual$Year)) {
            # do the allocation for each year
            
            # print(FSR_annual$Year[i])
            year <- FSR_annual$Year[i]
  
            # if FSR does not exist for last year, use the previous year's value
            if (is.na(as.numeric(FSR_annual[which(FSR_annual$Year == year), "mt"])) == FALSE) 
                FSR <- as.numeric(FSR_annual[which(FSR_annual$Year == year), "mt"])
            else FSR <- as.numeric(FSR_annual[which(FSR_annual$Year == (year - 1)), "mt"])
            
            if (allocation_flag[i] == 1) {
                # allocate using FSR and weight prop by quarter by area
                allocation[which(allocation$Year == year), paste0("W", seq(1, n_areas))] <- FSR * Grid_prop_quarterly[which(allocation$Year == 
                  year), paste0("", seq(1, n_areas))]
            }
            
            if (allocation_flag[i] == 2) {
                # use the gridded weight data directly
                allocation[which(allocation$Year == year), paste0("W", seq(1, n_areas))] <- Grid_weight_quarterly[which(floor(Grid_weight_quarterly$YQ) == 
                  year), paste0("", seq(1, n_areas))]
            }
            
            if (allocation_flag[i] == 3) {
                # allocate using FSR and weight prop by quarter by area from the nearest year that has data
                if (sum(Grid_prop_quarterly[paste0("", seq(1, n_areas))]) > 0) {
                  # use the prop in weight for allocation
                  for (year_diff in 1:40) {
                    # this loop is used to find the reference year (year_new) for allocation
                    
                    if ((year - year_diff) %in% Grid_number_annual$Yrr) {
                      if (Grid_weight_annual[which(Grid_weight_annual$Yrr == (year - year_diff)), "EPO"] > 0) {
                        year_new <- year - year_diff
                        break
                      }
                    }
                    
                    if ((year + year_diff) %in% Grid_number_annual$Yrr) {
                      if (Grid_weight_annual[which(Grid_weight_annual$Yrr == (year + year_diff)), "EPO"] > 0) {
                        year_new <- year + year_diff
                        break
                      }
                    }
                  }
                  
                  prop <- Grid_prop_quarterly[which(Grid_prop_quarterly$Yrr == year_new), c("Quarter", paste0("", 
                    seq(1, n_areas)))]
                  prop_final <- matrix(0, nrow = 4, ncol = n_areas)
                  for (q in 1:4) {
                    # if no catch in some quarter, add 0 for those quarters to make dataset consistent among quarters
                    if (q %in% prop$Quarter) 
                      prop_final[q, ] <- data.matrix(prop[which(prop$Quarter == q), 2:(n_areas + 1)])
                  }
                  # print(year_new)
                  allocation[which(allocation$Year == year), paste0("W", seq(1, n_areas))] <- FSR * prop_final
                  
                }
              else {
                  # use the prop in number for allocation
                  for (year_diff in 1:40) {
                    if ((year - year_diff) %in% Grid_number_annual$Yrr) {
                      if (Grid_number_annual[which(Grid_number_annual$Yrr == (year - year_diff)), "EPO"] > 0) {
                        year_new <- year - year_diff
                        break
                      }
                    }
                    
                    if ((year + year_diff) %in% Grid_number_annual$Yrr) {
                      if (Grid_number_annual[which(Grid_number_annual$Yrr == (year + year_diff)), "EPO"] > 0) {
                        year_new <- year + year_diff
                        break
                      }
                    }
                    
                  }
                  prop_num <- Grid_prop_num_quarterly[which(Grid_prop_num_quarterly$Yrr == year_new), c("Quarter", 
                    paste0("", seq(1, n_areas)))]
                  prop_final <- matrix(0, nrow = 4, ncol = n_areas)
                  for (q in 1:4) {
                    if (q %in% prop_num$Quarter) 
                      prop_final[q, ] <- data.matrix(prop_num[which(prop_num$Quarter == q), 2:(n_areas + 1)])
                  }
                  # print(year_new)
                  allocation[which(allocation$Year == year), paste0("W", seq(1, n_areas))] <- FSR * prop_final
                  
                }
            }
        }
        
        save <- right_join(Grid_number_quarterly[, c("YQ", paste0("", seq(1, n_areas)))], allocation[, c(1, seq(4, 
            n_areas + 3))])
        
        # if no data is available in the last year, copy the data in the previous year
        if (sum(!is.na(save[seq(nrow(save) - 3, nrow(save)), 2:(1 + 2 * n_areas)])) == 0) {
            # print(Countries[c])
            save[seq(nrow(save) - 3, nrow(save)), 2:(1 + 2 * n_areas)] <- save[seq(nrow(save) - 3 - 4, nrow(save) - 
                4), 2:(1 + 2 * n_areas)]
        }
        
        # remove those numbers where weight allocation exist (see rule#2 in the word file)
        save[, paste0("", seq(1, n_areas))] <- save[, paste0("", seq(1, n_areas))] * ifelse(is.na(save[, paste0("W", 
            seq(1, n_areas))]), 1, NA)
        
        write.csv(save, paste0(dir, Countries[c], toString(last_year+0.25), ".csv"), row.names = FALSE)
        
        save_all <- rbind(save_all, data.matrix(save))
    }
    colnames(save_all) <- c("YQ", paste0("N", seq(1, n_areas)), paste0("W", seq(1, n_areas)))
    write.csv(save_all, paste0(dir, "save_all.csv"), row.names = FALSE)
    
    LL_Catch <- data.frame(save_all) %>% gather(c(paste0("N", seq(1, n_areas)), paste0("W", seq(1, n_areas))), key = "term", 
        value = "catch") %>% group_by(YQ, term) %>% summarise(tot_catch = sum(catch, na.rm = T)) %>% spread(key = term, 
        value = tot_catch) %>% filter(YQ >= 1975)
    
    
    ####################### Coastal countries
    Coastal_Countries <- as.character(unique(FSR_Catch$FlagAbv))[!as.character(unique(FSR_Catch$FlagAbv)) %in% Countries]
    Coastal_Countries <- sort(Coastal_Countries)
    
    if (Species == "BET") 
        Area_Flag <- c(6, 4, 4, 4, 6, 4, 4, 4, 4) # "CHL" "COL" "CRI" "ECU" "ESP" "HND" "PER" "PRT" "SLV"
    if (Species == "YFT") 
        Area_Flag <- c(1, 3, 3, 3, 3, 2, 2, 3, 3, 3, 2, 3, 3)
    
    print(Coastal_Countries)
    print(Area_Flag)
    
    Coastal_Catch <- matrix(0, nrow = nrow(LL_Catch), ncol = n_areas)
    Coastal_Catch <- cbind(floor(LL_Catch$YQ), Coastal_Catch)
    
    for (c in 1:length(Coastal_Countries)) {
        FSR_annual <- FSR_Catch %>% filter(FlagAbv == Coastal_Countries[c], SpeciesAbv == Species) %>% group_by(Year) %>% 
            summarise(mt = sum(mt))
        
        for (y in 1:nrow(FSR_annual)) {
            Coastal_Catch[which(Coastal_Catch[, 1] == FSR_annual$Year[y]), Area_Flag[c] + 1] <- Coastal_Catch[which(Coastal_Catch[, 
                1] == FSR_annual$Year[y]), Area_Flag[c] + 1] + FSR_annual$mt[y]/4
        }
    }
    
    if (sum(Coastal_Catch[(nrow(LL_Catch) - 3):nrow(LL_Catch), 2:(n_areas + 1)]) == 0) {
        Coastal_Catch[(nrow(LL_Catch) - 3):nrow(LL_Catch), 2:(n_areas + 1)] <- Coastal_Catch[(nrow(LL_Catch) - 3 - 
            4):(nrow(LL_Catch) - 4), 2:(n_areas + 1)]
    }
    
    write.csv(Coastal_Catch, paste0(dir, "Coastal_Catch.csv"), row.names = FALSE)
    
    # total catch
    LL_Catch[, paste0("W", seq(1, n_areas))] <- LL_Catch[, paste0("W", seq(1, n_areas))] + Coastal_Catch[, 2:(n_areas + 
        1)]
    
    LL_Catch_SS <- data.frame(LL_Catch) %>%
        gather(c(paste0("N", seq(1, n_areas)), paste0("W", seq(1, n_areas))), key = "Fishery",value = "Catch") %>%
        mutate(Year=(YQ-1975)*4+1,seas=1,Catch_CE=0.01) %>% select(Year,seas,Fishery,Catch,Catch_CE)
                                                                        
    write.csv(LL_Catch, paste0(dir, "LL_Catch.csv"), row.names = FALSE)
    write.csv(LL_Catch_SS, paste0(dir, "LL_Catch_SS.csv"), row.names = FALSE)
    
    LL_Catch_plot <- data.frame(LL_Catch) %>%
      gather(c(paste0("N", seq(1, n_areas)), paste0("W", seq(1, n_areas))), key = "Fishery",value = "Catch") %>%
      mutate(Unit = ifelse(substr(Fishery,1,1)=="N","Number","Weight"),
             Area = substr(Fishery,2,2))
    
    ggplot(data=LL_Catch_plot) +
      geom_line(aes(x=YQ,y=Catch,color=Area),alpha=0.25) +
      geom_smooth(aes(x=YQ,y=Catch,color=Area),span=0.25,se=FALSE) +
      facet_wrap(~Unit, scales="free_y", nrow=2) +
      theme_bw()
    ggsave(filename = paste0(dir,"Catches.png"), dpi = 300, width = 8, height = 8)
    
    return(LL_Catch)
}
