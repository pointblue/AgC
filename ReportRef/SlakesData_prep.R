#Prepping slakes data for LandStewardReports

#prep slakes dataframe
Slakes_df <- PointLevel %>% #reformats so the dataframe is long with respect to the carbon stock pool
  select (sample_id, timepoint, plot_type, Slakes_index)%>%
  mutate(
    LU="AgC"
  )

#Define the desired order of x-axis values
Slakes_df$plot_type <- factor(Slakes_df$plot_type, levels = c("T", "C"))
Slakes_df$timepoint <- factor(Slakes_df$timepoint, levels = c("T0","T1","T2", "T3"))
