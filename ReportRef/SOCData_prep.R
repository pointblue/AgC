#Prepping SOC% data for LandStewardReports

#prep soc dataframe
soc_df <- PointLevel %>% #reformats so the dataframe is long with respect to the carbon stock pool
  {
    #if there is no value for position, "AgC" will distinguish from RaCA crop or range
    if (all(is.na(.$position))) {
      mutate(.,LU = "AgC")
    } else {
      mutate(., LU = position)
    }
  }%>%
  select(sample_id, timepoint, plot_type, org_c, LU) %>%
  bind_rows( #bind project stocks df to the raca dataset
    ComparisonData %>%
      select(sample_id, plot_type, org_c)%>%
      mutate(
        plot_type = "comparison",
        LU="comparison",
        timepoint="comparison"
        #.keep = "none"   #drop all other columns
      )
    )%>%
  filter(LU %in% c("AgC", "Row", "alley", "comparison", params$raca_filter))%>% #filter out values that don't match the correct land use type
  mutate(avgcol = "avg") #adding this column is necessary to get one legend entry for all average values across trt, ctrl, and raca


#Define the desired order of x-axis values
soc_df$plot_type <- factor(soc_df$plot_type, levels = c("T", "C", "comparison"))
soc_df$timepoint <- factor(soc_df$timepoint, levels = c("T0","T1","T2","2010", "comparison"))
