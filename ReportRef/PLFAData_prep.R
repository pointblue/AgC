#Prepping PLFA data for LandStewardReports

#prep PLFA dataframe
PLFA_df <- PointLevel %>% #reformats so the dataframe is long with respect to the carbon stock pool
  select (sample_id, timepoint, plot_type, fung_bio, bact_bio)%>%
  mutate(
    LU="AgC",
    Ratio =  fung_bio / bact_bio,
    total_bio = fung_bio+bact_bio
    )

#Define the desired order of x-axis values
PLFA_df$plot_type <- factor(PLFA_df$plot_type, levels = c("T", "C"))
PLFA_df$timepoint <- factor(PLFA_df$timepoint, levels = c("T0","T1","T2"))
