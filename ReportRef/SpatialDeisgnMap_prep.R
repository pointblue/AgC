#Spatial Design Map prep for LandStewardReports


#prep data
border <- st_transform(border, crs = 4326)
points <- st_transform(points, crs = 4326)
points$labels <- substr(points$name, nchar(points$name) - 1, nchar(points$name))


#prep plot labels to include acreage
plot_acreage<-border %>%
  st_transform(5070) %>%                 # equal-area CRS
  mutate(acres = as.numeric(st_area(.)) * 0.000247105) %>%
  st_drop_geometry() %>%
  group_by(plot_type) %>%
  summarise(acres = sum(acres), .groups = "drop")

legend_labels <- setNames(
  paste0(
    c("Treatment", "Control"),
    "<br><span style='font-size: 13px; padding-left: 2em;'>",
    round(
      plot_acreage$acres[
        match(c("T","C"), plot_acreage$plot_type)
      ],
      1
    ),
    " ac</span>"
  ),
  c("T", "C")
)

#prep palette
pal <- colorFactor(c("white","black"), c("T", "C"))


#start map
SpatialDesignMapFinal<-
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data = st_zm(border),
              fillOpacity = 0,
              color = ~ pal(plot_type),
              opacity=1)%>%
  addLegend(
    pal = pal, 
    values = border$plot_type,
    title = "Plot Type",
    position = "topleft",
    labFormat = function(type, cuts, p) {
      legend_labels[cuts]
    }
  )%>%
  addLabelOnlyMarkers(data=points,
                      label = points$labels, 
                      labelOptions = labelOptions(
                        noHide = TRUE, 
                        direction = "center",
                        offset = c(0, 0),
                        style = list(
                          "font-weight" = "bold", 
                          "padding" = "1px", 
                          "border-radius" = "4px"
                        )))
