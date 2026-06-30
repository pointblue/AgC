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
  {
    if (params$project_name %in% c("ENGL.24.SC", "KELA.24.SC", "HEPU.23.SC", "NAPA.24.SC", "SHRA.24.SC", 
                                   "JPQM.18.SC", "JPPS.10.SC", "JPPN.18.SC", "JPFA.14.SC", "JPNV.14.SC", "JPNC.14.SC", "JPBO.14.SC")) {
      bind_rows(
        .,
        ComparisonData %>%
          select(sample_id, plot_type, org_c) %>%
          mutate(
            plot_type = "comparison",
            LU = "comparison",
            timepoint = "comparison"
          )
      )
    } else {
      bind_rows(
        .,
        raca_data %>%
          transmute(
            sample_id = rcasiteid,
            org_c = SOC_perc,
            plot_type = "raca",
            timepoint = "2010",
            LU = LU
          )
      )
    }
  } %>%
  filter(LU %in% c("AgC", "Row", "alley", "comparison", params$raca_filter))%>% #filter out values that don't match the correct land use type
  mutate(avgcol = "avg") #adding this column is necessary to get one legend entry for all average values across trt, ctrl, and raca


#Define the desired order of x-axis values
soc_df$plot_type <- factor(soc_df$plot_type, levels = c("T", "C", "comparison", "raca"))
soc_df$timepoint <- factor(soc_df$timepoint, levels = c("T0","T1","T2","2010", "comparison"))
