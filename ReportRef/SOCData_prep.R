#Prepping SOC% data for LandStewardReports

#prep soc dataframe
soc_df <- PointLevel %>% #reformats so the dataframe is long with respect to the carbon stock pool
  select (sample_id, timepoint, plot_type, org_c)%>%
  mutate(LU="AgC")%>%
  bind_rows( #bind project stocks df to the raca dataset
    raca_data %>%
      mutate(
        sample_id = rcasiteid,
        org_c = SOC_perc,
        plot_type = "raca",
        timepoint = "Reference (2010)",
        LU=LU,
        .keep = "none"   #drop all other columns
      )
  )%>%
  filter(LU %in% c("AgC",   params$raca_filter)) #filter out values that don't match the correct land use type


#Define the desired order of x-axis values
soc_df$plot_type <- factor(soc_df$plot_type, levels = c("T", "C", "raca"))
soc_df$timepoint <- factor(soc_df$timepoint, levels = c("T0","T1","T2","Reference (2010)"))