#' Longline fisheries' length freqeuncy
#' 
#' \code{ll_fisheries_lf} This function processes the raw LL fisheries' length freqeuncy data into the format for Stock Assessment
#' 
#' @export

ll_fisheries_lf = function(JPN_size, JPN_ce, Grid_Catch, Species, last_year, dir) {

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
               M_unit %in% c(6,7)) %>% #1/2cm resolution data
        mutate(Year = (YY - 1975) * 4 + ceiling(MM / 3))
    
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
               M_unit %in% c(6,7)) %>% #1/2cm resolution data
        mutate(
            L = cut(
                CLS,
                breaks = c(20, seq(22, 198, 2), 220), # 20, 22, ......, 196, 198
                right = F,
                labels = seq(20, 198, 2)
            ),
            Year = (YY - 1975) * 4 + ceiling(MM / 3)) %>%
        filter(is.na(L) == FALSE) %>%
        group_by(Year, X, Y, L) %>% summarise(count = n()) %>% # count number of fish
        group_by(Year, X, Y) %>% mutate(count_sum = sum(count)) %>%
        mutate(LF = count / count_sum) %>% # 1 by 1 LF
        select(Year, X, Y, L, LF) %>%
        spread(L,LF, fill = 0) # spread length bins into column
    
    # catch data
    catch_data <- JPN_ce %>%
        filter(NGYO == 1, # commercial vessels
               ioc == 4, # EPO
               YY > 1974,
               YY <= last_year,
               MM > 0,
               NHBF > 0) %>%
        mutate(Year=(YY-1975)*4+ceiling(MM/3)) %>%
        group_by(Year, X, Y) %>%
        summarise(catch=ifelse(Species=="BET",sum(bigeye),sum(yellowfin))) # total catch
    
    # combine LF and catch data
    size_catch_data <- left_join(size_data,catch_data) %>% 
        na.omit() %>%
        gather(names(size_data)[4:ncol(size_data)],"key"=Length,"value"=LF) %>% 
        mutate (Length = as.numeric(Length),
                X2 = cut(X,breaks = seq(210,290,5),labels = seq(212.5,287.5,5)), # 5 by 5 resolution
                Y2 = cut(Y,breaks = seq(-40,40,5),labels = seq(-37.5,37.5,5))) %>% # 5 by 5 resolution
        na.omit() %>%
        group_by(Year,X2,Y2,Length) %>%
        summarise(LF_Catch = sum(LF*catch)) %>% # JPN 1by1 raised to 5by5 using catch in number
        group_by(Year,X2,Y2) %>%
        mutate(Tot_Catch=sum(LF_Catch)) %>%
        mutate(LF=LF_Catch/sum(LF_Catch)) # JPN 5by5 LF
    
    size_catch_data$Lon <- as.numeric(levels(size_catch_data$X2))[size_catch_data$X2] - 360
    size_catch_data$Lat <- as.numeric(levels(size_catch_data$Y2))[size_catch_data$Y2]
    
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
    
    ggsave(filename = paste0(dir,"Areas.png"), dpi = 300, width = 5, height = 5)
    
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
    
    # prepare and save the final length freqeuncy output based on SS format
    F_LF_SS <- data.frame("Year"=data_area_final$Year,"Month"=1,"Fleet"=data_area_final$Area,
                          "sex"=0, "par"=0, "Nsamp" = data_area_final$n)
    F_LF_SS <- cbind(F_LF_SS,data_area_final[3:92],data_area_final[3:92]) # male and female LF
    write.csv(F_LF_SS,file = paste0(dir,"LF.csv"),row.names = FALSE) # save
    
    # check results
    data_plot <- data_area %>% gather(as.character(seq(20,198,2)), key = length, value = lf) %>%
        mutate(length=as.numeric(length)) %>% group_by(Area,length) %>%
        summarise(lf_mean=mean(lf))
    
    ggplot(data=data_plot) +
        geom_smooth(aes(x=length,y=lf_mean,color=factor(Area)),span = 0.2,se = FALSE) +
        geom_line(aes(x=length,y=lf_mean,color=factor(Area)),alpha=0.5) +
        theme_bw(12) +
        ylab("Average LF")
    ggsave(filename = paste0(dir,"LL Fisheries LF.png"), dpi = 300, width = 8, height = 5)
    
    return(data_area_final)
}
