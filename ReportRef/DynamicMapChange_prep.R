#Prepping Change Dynamic Map for LandSteward Reports

#join first and last (need to drop geometry)
DataMap.proj_joined <- DataMap.proj_lastT %>%
  st_drop_geometry() %>%
  left_join(
    DataMap.proj_firstT %>% st_drop_geometry(),
    by = "sample_id",
    suffix = c(".last", ".first")
  )

#Identify indicators where lastT is not all NA
valid_inds <- dynamicmap.inds[
  sapply(dynamicmap.inds, function(col) {
    !all(is.na(DataMap.proj_joined[[paste0(col, ".last")]]))
  })
]
valid_last_cols  <- paste0(valid_inds, ".last")
valid_first_cols <- paste0(valid_inds, ".first")

#Compute differences using the .last columns and .first columns
DataMap.proj_change <- DataMap.proj_joined %>%
  mutate(
    across(
      .cols = all_of(valid_last_cols),
      .fns  = ~ . - DataMap.proj_joined[[sub(".last$", ".first", cur_column())]],
      .names = "{sub('.last$', '', .col)}"
    )
  ) %>%
  select(sample_id, all_of(valid_inds)) %>%
  
  # Restore geometry
  left_join(
    DataMap.proj_lastT %>% select(sample_id, geometry),
    by = "sample_id"
  ) %>%
  st_as_sf() %>%
  dplyr::filter(!(is.na(org_c))) #this step removes any rows for samples that weren't measured at both timepoints to avoid errors (a la Engler ranch)

#Set up a new color scheme that makes sense for showing directional change
pal_change <- colorRampPalette(c("#3B4CC0", "white", "#B40426"))  


#Prepare popup for the change map
legends_names_change <- setNames( #edit legend names
  paste0(legends_names, " change"),
  names(legends_names)
) 
df_forpopup_change <- DataMap.proj_change %>%
  mutate(across(any_of(valid_inds), ~round(.x, 2))) %>%
  rename_with(~ legends_names_change[.x], .cols = any_of(names(legends_names_change)))

popupcols_change <- setdiff(names(df_forpopup_change), c("sample_id", "geometry", "plot_type"))
df_forpopup_firstT$popup_html <- apply(df_forpopup_change, 1, function(r)
  build_popup_html(r, popupcols_change))

#Create map for first timepoint
DynamicMap2 <- BaseMap
overlay_groups <- c()

for (i in seq_along(valid_inds)) {
  indicator <- valid_inds[i]
  legend_name <- ifelse(indicator %in% names(legends_names_change),
                        legends_names_change[indicator],
                        indicator)
  
  #symmetric domain for showing change
  rng <- range(DataMap.proj_change[[indicator]], na.rm = TRUE)
  max_abs <- max(abs(rng))
  domainz <- c(-max_abs, max_abs)
  pal_ind <- colorNumeric(
    palette = pal_change(100),
    domain  = domainz,
    na.color = "gray"
  )
  
  DynamicMap2 <- DynamicMap2 %>%
    addCircleMarkers(
      data = DataMap.proj_change,
      color = ~pal_ind(get(indicator)),
      opacity = 1,
      radius = 3,
      group = legend_name,
      label = DataMap.proj_change$sample_id,
      popup = df_forpopup_firstT$popup_html
    ) %>%
    addLegend(
      pal = pal_ind,
      values = DataMap.proj_change[[indicator]],
      title = legend_name,
      group = legend_name
    )
  
  overlay_groups <- c(overlay_groups, legend_name)
}

DynamicMap2 <- DynamicMap2 %>%
  addLayersControl(
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(setdiff(overlay_groups, "SOC % change"))  
