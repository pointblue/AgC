#Prepping bulk density data for LandStewardReports

#prep bd dataframe
bd_df <- PointLevel %>% #reformats so the dataframe is long with respect to the carbon stock pool
  {
    #if there is no value for position, "AgC" will distinguish from RaCA crop or range
    if (all(is.na(.$position))) {
      mutate(.,LU = "AgC")
    } else {
      mutate(., LU = position)
    }
  } %>%
  select(sample_id, timepoint, plot_type, bulk_density, LU) %>%
  {
    if (params$project_name %in% c("ENGL.24.SC", "KELA.24.SC", "HEPU.23.SC", "NAPA.24.SC", "SHRA.24.SC")) {
      bind_rows(
        .,
        ComparisonData %>%
          select(sample_id, plot_type, bulk_density) %>%
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
            org_c = BD,
            plot_type = "raca",
            timepoint = "2010",
            LU = LU
          )
      )
    }
  } %>%
  filter(LU %in% c("AgC", "Row", "alley", "comparison", params$raca_filter))%>% #filter out values that don't match the correct land use type
  mutate(avgcol = "avg") #adding this column is necessary to get one legend entry for all average values across trt, ctrl, and raca


#prep NRCS categories based on texture class
texture <- PointLevel %>% 
  rename(SAND=sand, SILT=silt, CLAY=clay) %>%
  filter(!is.na(CLAY)) %>%
  filter(!is.na(SAND)) %>%
  filter(!is.na(SILT))

texture_avg <- texture %>%
  summarise(
    SAND = mean(SAND, na.rm = TRUE),
    SILT = mean(SILT, na.rm = TRUE),
    CLAY = mean(CLAY, na.rm = TRUE)
  )

texture_avg$USDA_texture <- TT.points.in.classes(
  tri.data = texture_avg[, c("SAND", "SILT", "CLAY")],
  class.sys = "USDA.TT"
)

av_tx_abv <- colnames(texture_avg$USDA_texture)[texture_avg$USDA_texture == 1]
avg_tx_info<-get_texture_info(av_tx_abv)

#prep threshhold bands dataframe
non_ref<-unique(bd_df$plot_type)
non_ref <- non_ref[non_ref != "raca" & non_ref != "comparison"] #this step is necessary so the bands dont appear in the raca facet (incorrect comparison by texture)
bd_bands <- data.frame(
  plot_type = rep(non_ref, each = 3),
  zone = rep(c(
    "Ideal",
    "May affect root growth",
    "Restricts root growth"
  ), times = length(non_ref)),
  ymin = rep(c(
    min(bd_df$bulk_density, na.rm = TRUE),
    avg_tx_info$threshold$ideal,
    avg_tx_info$threshold$restrict
  ), times = length(non_ref)),
  ymax = rep(c(
    avg_tx_info$threshold$ideal,
    avg_tx_info$threshold$restrict,
    max(bd_df$bulk_density, na.rm = TRUE)
  ), times = length(non_ref))
)

#Define the desired order of facets (needs to be done for both datasets)
bd_df$plot_type <- factor(bd_df$plot_type, levels = c("T", "C", "comparison"))
bd_bands$plot_type <- factor(bd_bands$plot_type, levels = levels(bd_df$plot_type))

