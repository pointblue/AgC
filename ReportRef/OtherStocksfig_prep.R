#Prep other stocks figure for LandStewardReports

#get rid of raca values for this graph
stocks_df_forallpools<-stocks_df%>%filter(!plot_type %in% "raca")

#Define the desired order of indicators
stocks_df_forallpools$Stocks_Indicator <- factor(stocks_df_forallpools$Stocks_Indicator, levels = c("org_c_stocks", "inorg_c_stocks", "maoc_stocks", "poc_stocks", "all_stocks"))

#define labels for the facets (stocks indicators)
indicator_labels <- c(
  "org_c_stocks" = "Soil organic carbon",
  "inorg_c_stocks" = "Soil inorganic carbon",
  "maoc_stocks"  = "Mineral-assocaited organic carbon",
  "poc_stocks"   = "Particulate organic carbon",
  "all_stocks" = "Carbon pools combined"
)

stocks_df_forallpools <- stocks_df_forallpools %>%
  mutate(
    timepoint_label = ifelse(is.na(timepoint), "Reference", timepoint),
    plot_time = interaction(plot_type, timepoint_label, sep = " | ")
  ) %>%
  mutate(
    # convert to factor with desired order based on plot_type
    plot_time = factor(
      plot_time,
      levels = unique(plot_time[order(plot_type, timepoint_label)])
    )
  )


OtherStocksfig<-ggplot(stocks_df_forallpools, aes(x = plot_time, y = Tons.Acre)) +
  
  # Jittered black points
  geom_point(aes(color = LU),
             alpha = 0.8,
             size = 3,
             position = position_jitter(width = 0.1, height = 0)
  ) +
  
  #lines connecting plot averages
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = plot_type), 
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
    aes(color = plot_type),
    size = 0.7, #size of dot
    linewidth = 1.5, #thickness of CI line
    shape = 16
  ) +
  # Text labels for means
  stat_summary(
    fun = mean,
    geom = "shadowtext", #requires package shadowtext
    bg.color = "black", #text outline color
    bg.r = 0.15,  #thickness of outline
    aes(label = sprintf("%.2f", after_stat(y))), #change decimal places printed by altering the digit
    color = "#FFD700", #text color
    hjust = 1.1,
    vjust = -1.2,
    fontface = "bold",
    size = 3.54
  )+
  labs(
    x = "",
    y = "Carbon Stored (tons/acre)",
  ) +
  scale_color_manual(
    name = "",
    values = c(palette_plot_type, palette_LU),
    breaks = c("T", "C", "raca", "AgC", "range", "crop"),
    labels = c(
      str_wrap("Treated site average", width = 21),
      str_wrap("Control site average", width = 21),
      str_wrap(paste0(ecoregion, " average"), width = 21),
      "AgC project sampling point",
      "RaCA rangeland sampling point",
      "RaCA cropland sampling point"
    )
  ) +
  # Custom x-axis labels
  scale_x_discrete(labels = c(
    "C | T0" = "T0",
    "C | T1" = "T1",
    "T | T0" = "T0",
    "T | T1" = "T1"
  )) +
  theme_minimal()+
  theme(
    legend.position = "bottom"
  )+
  facet_wrap(~Stocks_Indicator, labeller = labeller(Stocks_Indicator = indicator_labels))