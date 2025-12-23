#Spatial Design Map prep for LandStewardReports

border <- st_transform(border, crs = 4326)
points <- st_transform(points, crs = 4326)
points$labels <- substr(points$name, nchar(points$name) - 1, nchar(points$name))

pal <- colorFactor(c("white","black"), c("T", "C"))

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
    position = "bottomleft",
    labFormat = function(type, cuts, p) { 
      c("Treatment", "Control")[match(cuts, c("T", "C"))]
    })%>%
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