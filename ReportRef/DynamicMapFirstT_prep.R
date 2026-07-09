#Prepping FirstT Dynamic Map for LandSteward Reports

#Create a vector of indicators to include in the dynamic map
#I will need to think about this harder once we start generating reports for projects with depth increments
#But a simple approach is sufficient for now
  #if (exists("proj.indicators.SSC.stocks") && "bulk_density" %in% proj.indicators.SSC.stocks && params$stocks==TRUE) {
  #  stockscolumns<-grep("_stocks$", proj.indicators.SSC.stocks, value = TRUE) #gather the names of the stocks columns
  #  proj.indicators.depth<-proj.indicators.SSC.stocks[!proj.indicators.SSC.stocks %in% c("AHB", "HRB", "AWB", "WRB", "Ksat")] #CHECK! irrelevant for now but will be important later...get the rest of the project indicators that can be reported not as stocks...this matters for depth increment stuff
  #  dynamicmap.inds <- c(stockscolumns, proj.indicators.depth)
  #} else {
  #  proj.indicators.depth<-proj.indicators.SSC[!proj.indicators.SSC %in% c("AHB", "HRB", "AWB", "WRB", "Ksat")] #CHECK! irrelevant for now but will be important later...get the rest of the project indicators that can be reported not as stocks...this matters for depth increment stuff
  #  dynamicmap.inds <- proj.indicators.depth
  #}

if (exists("proj.indicators.SSC.stocks") && "bulk_density" %in% proj.indicators.SSC.stocks && params$stocks==TRUE) {
  stockscolumns<-grep("_stocks$", proj.indicators.SSC.stocks, value = TRUE) #gather the names of the stocks columns
  dynamicmap.inds <- c(stockscolumns, proj.indicators.SSC.stocks)
} else {
  dynamicmap.inds <- proj.indicators.SSC
}

#Define a fixed order for the Ag-C indicators layers to appear and apply it to dynamicmap.inds
fixed_order <- c("org_c", "maoc", "poc", "inorg_c", "org_c_stocks", "maoc_stocks", "poc_stocks", "inorg_c_stocks", "bulk_density", "ph", "sand", "silt", "clay")
dynamicmap.inds <- c(intersect(fixed_order, dynamicmap.inds), setdiff(dynamicmap.inds, fixed_order))

#Define labels for legend and popups
Means.Pivot2<-Means.Pivot1%>%
  filter(Indicator != "all_stocks")%>%
  mutate(legend = paste0(Acronym,ifelse(is.na(Units), "", paste0(" ", Units))))
legends_names <- setNames(Means.Pivot2$legend, Means.Pivot2$Indicator)
legends_names["sample_id"]<-"Sample ID" #handling this separately

#Define a helper function to create custom HTML popups for each sampling point (from chatGPT)
build_popup_html <- function(row, cols) {
  vals <- row[cols]                   # select the relevant columns
  vals <- vals[!is.na(vals)]          # remove NAs
  vals <- lapply(vals, as.character)  # convert all to character
  if(length(vals) == 0) return("")    # in case all values are NA
  paste0(
    "<table>",
    paste0("<tr><td><b>", names(vals), "</b></td><td>", vals, "</td></tr>", collapse=""),
    "</table>"
  )
}

#Define color palettes
base_colors <- rainbow(length(dynamicmap.inds)) #create a rainbow palette from which to create color ramps
pal <- colorFactor(c("white", "black"), c("T", "C")) #Plot border color mapping

#Create the basemap that will apply to all following maps
BaseMap <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(
    data = st_zm(border),
    fillOpacity = 0,
    color = ~pal(plot_type),
    opacity = 1
  ) %>%
  addLegend(
    pal = pal,
    values = border$plot_type,
    title = "Plot Type",
    position = "topleft",
    labFormat = function(type, cuts, p) {
      c("Treatment", "Control")[match(cuts, c("T", "C"))]
    }
  )

#Establish a master data frames for points
DataMap.proj <- PointLevels_joined %>%
  select(sample_id, plot_type, timepoint, all_of(dynamicmap.inds)) %>%
  left_join(
    points %>%
      rename(sample_id = name) %>%
      select(sample_id, geometry),
    by = "sample_id"
  ) %>%
  st_as_sf()

#From the master, create a dataframe for the first and last timepoints
DataMap.proj_firstT<-DataMap.proj%>%
  mutate(last_digit = as.numeric(substr(timepoint, nchar(timepoint), nchar(timepoint)))) %>%
  filter(last_digit == min(last_digit))%>%
  select(-last_digit)
DataMap.proj_lastT<-DataMap.proj%>%
  mutate(last_digit = as.numeric(substr(timepoint, nchar(timepoint), nchar(timepoint)))) %>%
  filter(last_digit == max(last_digit))%>%
  select(-last_digit)

#Prepare popup for the first timepoint
df_forpopup_firstT <- DataMap.proj_firstT %>%
  mutate(across(any_of(dynamicmap.inds), ~round(.x, 2))) %>%
  rename_with(~ legends_names[.x], .cols = any_of(names(legends_names)))
popupcols_firstT <- setdiff(names(df_forpopup_firstT), c("sample_id", "geometry", "plot_type"))
df_forpopup_firstT$popup_html <- apply(df_forpopup_firstT, 1, function(r)
  build_popup_html(r, popupcols_firstT))

#Create map for first timepoint
DynamicMap <- BaseMap
overlay_groups <- c()

for (i in seq_along(dynamicmap.inds)) {
  indicator <- dynamicmap.inds[i]
  legend_name <- ifelse(indicator %in% names(legends_names),
                        legends_names[indicator],
                        indicator)
  
  domainz <- if (all(is.na(DataMap.proj_firstT[[indicator]]))) c(0, 1) else DataMap.proj_firstT[[indicator]]
  
  pal_ind <- colorNumeric(
    palette = colorRampPalette(c("white", base_colors[i]))(100),
    domain = domainz
  )
  
  DynamicMap <- DynamicMap %>%
    addCircleMarkers(
      data = DataMap.proj_firstT,
      color = ~pal_ind(get(indicator)),
      opacity = 1,
      radius = 3,
      group = legend_name,
      label = DataMap.proj_firstT$sample_id,
      popup = df_forpopup_firstT$popup_html
    ) %>%
    addLegend(
      pal = pal_ind,
      values = DataMap.proj_firstT[[indicator]],
      title = legend_name,
      group = legend_name
    )
  
  overlay_groups <- c(overlay_groups, legend_name)
}

DynamicMap <- DynamicMap %>%
  addLayersControl(
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(setdiff(overlay_groups, "SOC %"))
