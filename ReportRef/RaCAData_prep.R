#Prepping RaCA data for LandSteward Reports

# ---- Raca prep ----
raca_call<-reg_baseline(border, max.depth.cent)
raca_data<-raca_call[[1]]%>%left_join(raca_call[[2]]%>%select(-LU), by="rcasiteid")
ecoregion<-raca_call[[3]]

raca_data$LU[raca_data$LU == "C"] <- "crop"
raca_data$LU[raca_data$LU == "R"] <- "range"


# ---- Ag-C Comparison Data Prep ----




if (params$project_name %in% c("ENGL.24.SC", "KELA.24.SC", "HEPU.23.SC", "NAPA.24.SC", "SHRA.24.SC", "STOR.24.SC")){
ComparisonData<-list_dfs_point[which.max(as.Date(gsub("\\D","", list_dfs_point), format = "%Y%m%d"))]%>%
  read.csv(na.strings=c("", "na", "NA", "nd", "ND", "-"))%>%
  mutate(plot_type = substr(sample_id, 12, 12))%>%
  #Criteria for CO2F projects: rangeland compost sites with T0
  filter(str_detect(project_id, ".SC"))%>% 
  filter(timepoint=="T0")%>%
  filter(project_id != params$project_name)
} else if (params$project_name %in% c("JPQM.18.SC", "JPPS.10.SC", "JPPN.18.SC", "JPFA.14.SC", "JPNV.14.SC", "JPNC.14.SC", "JPBO.14.SC")) {
  ComparisonData<-list_dfs_point[which.max(as.Date(gsub("\\D","", list_dfs_point), format = "%Y%m%d"))]%>%
    read.csv(na.strings=c("", "na", "NA", "nd", "ND", "-"))%>%
    mutate(plot_type = substr(sample_id, 12, 12))%>%
    #Criteria for JPV projects: other JPV plots
    filter(project_id %in% c("JPQM.18.SC", "JPPS.10.SC", "JPPN.18.SC", "JPFA.14.SC", "JPNV.14.SC", "JPNC.14.SC", "JPBO.14.SC"))%>%
    filter(project_id != params$project_name)
}else {
  ComparisonData<-NULL
}

if (!is.null(ComparisonData)){
  compyearrangelabel<-paste0(first(sort(format(as.POSIXct(ComparisonData$sample_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%Y"))), "-",
  last(sort(format(as.POSIXct(ComparisonData$sample_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%Y"))))
}

