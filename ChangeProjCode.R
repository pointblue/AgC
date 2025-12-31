#Script to change project codes
#Author: Avalon Cook
#Created 12/31/2025, Updated 12/31/2025

# ---- Change project code ----

source('functions.R')

old_code<-"MAAN.27.RP"
new_code<-"MAAN.25.SC"

## ---- Lab Raw ----

  dir<-"Z:/Soils Team/AgC Data"
  lab_file<-fetch_lab_file(dir, old_code)
  lab_raw<-read.csv(lab_file, check.names = FALSE) 
  
  #CHECK! this ward file had a weird anomoly with the column name for sample ID
  lab_new <- lab_raw %>%
    mutate(
      Sample.ID.2 = gsub(old_code, new_code, Sample.ID.2)
    )
  write.csv(lab_new, lab_file)

## ---- Point-Level Master Dataframe ----
  
  # Import current master database
  master_df_list <- list.files(paste(data_dir,"Master Datasheets","PointLevel", sep="/"), pattern = "\\.csv$", full.names = TRUE) #list all the CSVs in folder
  df_current <- read.csv(master_df_list[which.max(as.Date(gsub("\\D","", master_df_list), format = "%Y%m%d"))]) #this indexing patterns makes sure we're using the most recent master datasheet
  df_current$sample_date<-as.Date(df_current$sample_date) #make sure the date column is in date format
  
  for (col in c("sample_id", "project_id")) {
    if (col %in% names(df_current)) {
      df_current[[col]] <- gsub(old_code, new_code, df_current[[col]])
    }
  }
  
  write.csv(df_current, master_df_list[which.max(as.Date(gsub("\\D","", master_df_list), format = "%Y%m%d"))])

## ---- Field-Level Master Dataframe ----  
  #fill in

## ---- Project Design Master Dataframe ----   
  #fill in
  
## ---- Project Design Master Dataframe ----
  #fill in
  
## ---- Spatial ---- 
  dir<-"Z:/Soils Team/AgC Data/Raw Data/Spatial Data/ZippedShapefiles"
  
  border<-read_spatial(dir, paste0(old_code, "_border.zip"))
  
  for (col in c("name", "proj_name")) {
    if (col %in% names(border)) {
      border[[col]] <- gsub(old_code, new_code, border[[col]])
    }
  }
  write_zipped_shp(border, dir, paste0(new_code, "_border"))
  
  points<-read_spatial(dir, paste0(old_code, "_pointsfinal.zip"))
  for (col in c("name", "proj_name")) {
    if (col %in% names(points)) {
      points[[col]] <- gsub(old_code, new_code, points[[col]])
    }
  }
  write_zipped_shp(points, dir, paste0(new_code,"_pointsfinal"))

# Manual entry
  #search and replace in Ag-C data entry
  #whats the best move for archive?