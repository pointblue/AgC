#Prepping bulk density figure for LandStewardReports

#Begin graph
BDfig<-ggplot(bd_df, aes(x = timepoint, y = bulk_density))+
  
  #USDA Bulk Density Targets by Texture
  geom_rect(
    data = bd_bands,
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = ymin,
      ymax = ymax,
      fill = zone
    ),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  
  #Jittered black points
  geom_point(aes(color = LU),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.2, height = 0)
  ) +
  
  #Lines connecting plot averages
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = plot_type), 
    col="black",
    linewidth = 1 
  ) +
  
  #Mean + 95% CI (colored by plot_type)
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
    size = .8, #size of dot
    linewidth = 1.5, #thickness of CI line
    shape = 16
  ) +
  
  #Text labels for means
  stat_summary(
    fun = mean,
    geom = "shadowtext", #requires package shadowtext
    bg.color = "black", #text outline color
    bg.r = 0.15,  #thickness of outline
    aes(
      label = sprintf("%.2f", after_stat(y)), #change decimal places printed by altering the digit
      group = timepoint
    ), 
    color = "#FFD700", #text color
    hjust = 1.2,
    vjust = -1.2,
    fontface = "bold",
    size = 3.5
  )+
  
  scale_color_manual(
    name = "",
    values = c(palette_plot_type, palette_LU),
    breaks = c("avg", "AgC", "range", "crop", "alley", "Row", "comparison"),
    labels = c(
      str_wrap("Plot average", width = 21),
      "Project sampling point",
      str_wrap("RaCA rangeland sampling point", width=21),
      str_wrap("RaCA cropland sampling point", width=21),
      "Alley sampling point",
      "Row sampling point",
      "Ag-C comparison sampling point"
    )
  ) +
  
  #Legend settings for target bands
  scale_fill_manual(
    name = str_wrap(paste0("Target values for ", avg_tx_info$full_name, " soils"), width=21),
    values = c(
      "Ideal" = "#2C6E6F",
      "May affect root growth" = "#C9971A",
      "Restricts root growth" = "#D26A4A"
    )) +
  
  #Change legend order    
  guides(
    color = guide_legend(order = 1),
    fill  = guide_legend(order = 2)
  )+
  
  #Labels, axes, theme
  labs(
    x = "",
    y = "Soil Bulk Density (g/cm3)",
  ) +
  
  #Custom x-axis labels
  scale_x_discrete(labels = c(
    setNames(c(tp_lookup$year_label, compyearrangelabel), c(tp_lookup$timepoint, "comparison"))
  )) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold", size=12)
  )+
  
  #Make sure the y-axis always shows 0
  expand_limits(y=0) +
  
  # Facet by plot type
  facet_wrap(~plot_type, scales="free_x",
             labeller = as_labeller(c(
               `T` = "Treatment",
               C = "Control",
               raca = str_wrap(paste0("Reference: ", ecoregion), width=21),
               comparison = "Ag-C Comparison"
             ))
  ) 
