#Prepping PLFA data for LandStewardReports

#prep PLFA dataframe
PLFA_df <- PointLevel %>% #reformats so the dataframe is long with respect to the carbon stock pool
  select (sample_id, timepoint, plot_type, Functional.Group.Diversity.Index.ng.g, Fungi.Bacteria.ng.g, Total.Living.Microbial.Biomass.ng.g)%>%
  mutate(
    LU="AgC"
    )

#Define the desired order of x-axis values
PLFA_df$plot_type <- factor(PLFA_df$plot_type, levels = c("T", "C"))
PLFA_df$timepoint <- factor(PLFA_df$timepoint, levels = c("T0","T1","T2", "T3"))