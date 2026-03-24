#Prepping RaCA data for LandSteward Reports

# ---- Raca prep ----
raca_call<-reg_baseline(border, max.depth.cent)
raca_data<-raca_call[[1]]%>%left_join(raca_call[[2]]%>%select(-LU), by="rcasiteid")
ecoregion<-raca_call[[3]]

raca_data$LU[raca_data$LU == "C"] <- "crop"
raca_data$LU[raca_data$LU == "R"] <- "range"


# ---- Ag-C Comparison Data Prep ----


#Current criteria for CO2F projects: rangeland compost sites with T0

ComparisonData<-list_dfs_point[which.max(as.Date(gsub("\\D","", list_dfs_point), format = "%Y%m%d"))]%>%
  read.csv(na.strings=c("", "na", "NA", "nd", "ND", "-"))%>%
  mutate(plot_type = substr(sample_id, 12, 12))%>% #add the plot_type column
  filter(str_detect(project_id, ".SC"))%>%
  filter(timepoint=="T0")%>%
  filter(project_id != params$project_name)
