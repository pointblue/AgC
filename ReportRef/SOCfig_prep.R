#Prep SOC% figure for LandStewardReports

#Begin graph
SOCfig<-ggplot(soc_df, aes(x = timepoint, y = org_c))+
  
  # Jittered black points
  geom_point(aes(color = LU),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.2, height = 0)
  ) +
  
  #lines connecting plot averages
  stat_summary(
    fun = mean,
    geom = "line",
    aes(color=plot_type, group=plot_type), 
    col="black",
    linewidth = 1 
  ) +
  
  # Mean + 95% CI (colored by plot_type)
  stat_summary(
    fun.data = function(x) {
      mean_x <- mean(x, na.rm = TRUE)
      se_x <- sd(x, na.rm = TRUE) / sqrt(length(x[!is.na(x)]))
      ci <- 1.96 * se_x
      data.frame(
        y = mean_x,
        ymin = mean_x - ci,
        ymax = mean_x + ci
      )
    },
    geom = "pointrange",
    aes(color = avgcol),
    size = .7, #size of dot
    linewidth = 1.5, #thickness of CI line
    shape = 16
  ) +
  
  # Labels, axes, theme
  labs(
    x = "",
    y = "Soil Organic Carbon %",
  ) +
  
  scale_color_manual(
    name = "",
    values = c(palette_plot_type, palette_LU),
    breaks = c("avg", "AgC", "range", "crop", "alley", "Row", "comparison"),
    labels = c(
      str_wrap("Plot average", width = 21),
      "Ag-C sampling point",
      str_wrap("RaCA rangeland sampling point", width=21),
      str_wrap("RaCA cropland sampling point", width=21),
      "Alley sampling point",
      "Row sampling point",
      "Ag-C comparison sampling point"
    )
  ) +
  
  #Custom x-axis labels
  scale_x_discrete(labels = c(
    setNames(c(tp_lookup$year_label, "2022-2024"), c(tp_lookup$timepoint, "comparison"))
  )) +
  
  # Text labels for means
  stat_summary(
    fun = mean,
    geom = "shadowtext", #requires package shadowtext
    bg.color = "black", #text outline color
    bg.r = 0.15,  #thickness of outline
    aes(
      label = sprintf("%.2f", after_stat(y)),
      group = timepoint
    ), #change decimal places printed by altering the digit
    color = "#FFD700", #text color
    hjust = 1.2,
    vjust = -1.2,
    fontface = "bold",
    size = 3.5
  )+
  
  # Facet by plot type
  facet_wrap(~plot_type, scales="free_x",
             labeller = as_labeller(c(
               `T` = "Treatment",
               C = "Control",
               raca = str_wrap(paste0("Reference: ", ecoregion), width=21),
               comparison = "Ag-C Comparison"
             ))
  ) +

  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold", size=12)
  )+
  
  expand_limits(y=0)
