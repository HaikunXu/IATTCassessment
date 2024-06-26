#' Plot retrospective pattern
#' 
#' \code{plot_retro} This function plot retrospective SB and R for the stock assessment report 
#' 
#' @export

plot_retro = function(SS_Dir, lyear, fyear, Save_Dir, title, figure_name = "Retro_SB", xlim, ylim) {
  # spawning biomass ratio
  for (i in 1:length(lyear)) {
    Myreplist = r4ss::SS_output(
      dir = SS_Dir[i],
      covar = F,
      forecast = FALSE,
      verbose = FALSE,
      printstats = FALSE
    )
    if (i == 1) {
      SBR <-
        data.frame(
          "Year" = 1975 + (Myreplist$timeseries$Yr - 1) / 4,
          "SB" = Myreplist$timeseries$SpawnBio,
          "SBR" = Myreplist$timeseries$SpawnBio/Myreplist$timeseries$SpawnBio[1],
          "R" = Myreplist$timeseries$Recruit_0,
          "Assess_Year" = lyear[i]
        )
    }
    else {
      SBR <-
        rbind(
          SBR,
          data.frame(
            "Year" = 1975 + (Myreplist$timeseries$Yr - 1) / 4,
            "SB" = Myreplist$timeseries$SpawnBio,
            "SBR" = Myreplist$timeseries$SpawnBio/Myreplist$timeseries$SpawnBio[1],
            "R" = Myreplist$timeseries$Recruit_0,
            "Assess_Year" = lyear[i]
          )
        )
    }
  }
  
  SBR <-
    SBR %>% mutate(
      label1 = ifelse(Year >= fyear[1] &
                        Year < (Assess_Year + 1), 1, 0),
      label2 = ifelse(Year == Assess_Year + 0.75, 1, 0)
    )
  SBR$Assess_Year <- as.factor(SBR$Assess_Year + 1)
  
  f1 <- ggplot(data = SBR %>% filter(label1 == 1)) +
    geom_line(aes(x = Year, y = SBR, color = Assess_Year)) +
    geom_point(
      aes(x = Year, y = SBR, color = Assess_Year),
      data = SBR %>% filter(label2 == 1),
      size = 3
    ) +
    # coord_cartesian(ylim = ylim, xlim = xlim) +
    labs(x = "", y = "", color = "Year") +
    theme_bw(20) + ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(
    f1,
    file = paste0(Save_Dir, figure_name, ".png"),
    width = 8,
    height = 6
  )
  return(SBR)
    
}
