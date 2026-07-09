#Prepping Slakes figure for LandStewardReports

#Prepping ratio benchmark values

Index_benchmarks <- data.frame(
  LU = c("conv_ag", "mature_restored"),
  Ratio = c(0.30, 0.56)
)


#start fig
Slakesfig<-ggplot(Slakes_df, aes(x = timepoint, y = Slakes_index))+
  
  geom_hline(
    data = Index_benchmarks,
    aes(yintercept = Ratio, color = LU),
    inherit.aes = FALSE,
    linewidth = 1,
    linetype = "dashed"
  )+
  
  
  
  #Jittered black points
  geom_point(aes(color = LU),
             alpha = 0.7,
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
    aes(color = plot_type),
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
  
  #Legend settings for sampling points
  scale_color_manual(
    name = "",
    values = c(palette_plot_type, 
               palette_PLFA,
               "conv_ag"  = "#C9971A",
               "mature_restored"   = "#A67FA4"),
    breaks = c(
      "T", "C", "AgC",
      "conv_ag", "mature_restored"
    ),
    labels = c(
      str_wrap("Treated site average", width = 21),
      str_wrap("Control site average", width = 21),
      "Ag-C sampling point",
      "Convential cropland",
      "Mature restored habitat"
    )
  ) +
  
  
  #Labels, axes, theme
  labs(
    x = "",
    y = "Aggregate stability index",
  ) +
  
  #Custom x-axis labels
  scale_x_discrete(labels = c(
    setNames(c(tp_lookup$year_label, compyearrangelabel), c(tp_lookup$timepoint, "comparison"))
  )) +
  
  theme_minimal()+
  
  #Remove facet strip labels
  theme(strip.text.x = element_blank(),
        strip.text.y = element_text(
          #angle = 0,
          size=13,
          face="bold"
        )
  )+
  
  #Make sure the y-axis always shows 0
  expand_limits(y=0) +
  
  #Facet by plot type
  facet_wrap(
    ~plot_type, 
    scales="free_x"
  )
