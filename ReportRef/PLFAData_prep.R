#Prepping PLFA data for LandStewardReports

#prep PLFA dataframe
PLFA_df <- PointLevel %>% #reformats so the dataframe is long with respect to the carbon stock pool
  select (sample_id, timepoint, plot_type, fung_bio, bact_bio)%>%
      pivot_longer(
        cols = c(fung_bio, bact_bio),  # columns to reshape
        names_to = "PLFA",             # new column for the names
        values_to = "biomass"          # new column for the values
      )%>%
  mutate(LU="AgC")
 # )%>%
#filter(LU %in% c("AgC",   params$raca_filter)) #filter out values that don't match the correct land use type


#Define the desired order of x-axis values
PLFA_df$plot_type <- factor(PLFA_df$plot_type, levels = c("T", "C"))
PLFA_df$timepoint <- factor(PLFA_df$timepoint, levels = c("T0","T1","T2"))
