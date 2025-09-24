# Title: AgCDataCompile.R
# Author: Lisa Eash
# Date created: 20250402
# Date updated: 20250924
# Purpose: Main script for compiling ag-c master database

# Load packages
source('packages.R')

# Load functions
source('functions.R')

# Define data directory
data_dir<-("Z:/Soils Team/AgC Data/")

# Define path for Ag C data entry spreadsheet - you should have sharepoint synced to your onedrive so that it can be accessed on remote desktop
agc_data_entry <-"C:/Users/leash/OneDrive - Point Blue/PointBlue Programs - Shared Soils Program/Ag-C/Internal Ag-C Projects/AgCDataEntry.xlsx"

## ---- Import/clean lab and tap field data ----

# Lab soils data
  #Note: a warning message will appear if there are column names that are not yet included in our master datasheet
lab_clean <- clean_lab_df(data_path = data_dir, 
                          lab = "Ward", #Options: "Cquester", "Ward"
                          file_name = "Ward_data_20250409.csv")  #optional- can specify if you know the file name and/or are not working with the most recent lab data

# TAP field data
  #Note: a warning message will appear if there is no volume calculated for bulk density but there are some data in the BD.Vol/BD.Depth columns
tap_clean <- clean_tap_df(agc_data_entry)

## ---- Merge lab_clean and tap_clean dataframes ----
df <- lab_clean %>%
  left_join(tap_clean, by = c("sample_id","b_depth","e_depth","year")) %>%  # Merge lab and tap field data
  mutate(
    texture_name = coalesce(texture_name.x,texture_name.y),
    ph = coalesce(ph.x, ph.y),
    soil_moisture = coalesce(soil_moisture.x, soil_moisture.y),
    dry_soil_g = coalesce(dry_soil_g.x, dry_soil_g.y),
    rocks_g = coalesce(rocks_g.x,rocks_g.y)
  ) %>%
  select(-ends_with(c(".y",".x"))) %>%
  mutate(across(c(total_n:cec_na_perc), as.numeric))

#Check for sample_ids not found in tap_clean
df[is.na(df$project_id),]

## ---- Bulk density and biomass calculations ----

# Bulk density
df <- df %>%
  mutate(bulk_density = if_else( 
    is.na(bulk_density),   
    dry_soil_g / vol_cm3, # If NA, calculate as Dry.Mass / Volume
    bulk_density
  ))

## ---- Fill in all identifying columns ----

# Add coordinates

# Store target depths and measured depths
df <- df %>%
  mutate(target_depth = paste(b_depth, e_depth, sep="_")) %>%
  select(-c(b_depth,e_depth)) %>%
  rename(b_depth = b_depth_meas,
         e_depth = e_depth_meas)

## ---- QA/QC of full dataset ----

# Fill total_c if it's NA and org_c & inorg_c are both present
df$total_c[is.na(df$total_c) & !is.na(df$org_c) & !is.na(df$inorg_c)] <- 
  df$org_c[is.na(df$total_c) & !is.na(df$org_c) & !is.na(df$inorg_c)] + 
  df$inorg_c[is.na(df$total_c) & !is.na(df$org_c) & !is.na(df$inorg_c)]

# Fill org_c if it's NA and total_c & inorg_c are both present
df$org_c[is.na(df$org_c) & !is.na(df$total_c) & !is.na(df$inorg_c)] <- 
  df$total_c[is.na(df$org_c) & !is.na(df$total_c) & !is.na(df$inorg_c)] - 
  df$inorg_c[is.na(df$org_c) & !is.na(df$total_c) & !is.na(df$inorg_c)]

# Fill inorg_c if it's NA and total_c & org_c are both present
df$inorg_c[is.na(df$inorg_c) & !is.na(df$total_c) & !is.na(df$org_c)] <- 
  df$total_c[is.na(df$inorg_c) & !is.na(df$total_c) & !is.na(df$org_c)] - 
  df$org_c[is.na(df$inorg_c) & !is.na(df$total_c) & !is.na(df$org_c)]

# Make sure all samples have identifying info, total_c or org_c value, and bulk_density value
df[is.na(df$project_id),]
df[is.na(df$total_c) & is.na(df$org_c),]
df[is.na(df$bulk_density),]

# Check for values out of range
out_of_range(df, "bulk_density", 0.5, 1.8) #Bulk density between 0.5 and 1.8 g/cm3
out_of_range(df, "org_c", 0.1, 20) #total c %
out_of_range(df, "ph", 4, 9) #pH

#Org + inorg c = total c
df %>%
  filter(lab_name != "Ward") %>%
  mutate(inorg_c = replace_na(inorg_c, 0)) %>%
  filter(inorg_c + org_c != total_c)

#sand silt clay close to 100
df %>%
  filter(sand + silt + clay < 99 | sand + silt + clay > 101)

## ---- Bind to most recent point-level master database ----

# Select only columns needed for master database
final_cols <- read.csv("point_db_metadata.csv") #Metadata file for master point-level database
df <- df[,final_cols$column_name]

# Import current master database
master_df_list <- list.files(paste(data_dir,"Master Datasheets","PointLevel", sep="/"), pattern = "\\.csv$", full.names = TRUE) #list all the CSVs in folder
df_current <- read.csv(master_df_list[which.max(as.Date(gsub("\\D","", master_df_list), format = "%Y%m%d"))]) #this indexing patterns makes sure we're using the most recent master datasheet

# Add any new columns
cols_to_add <- setdiff(colnames(df),colnames(df_current))
df_current[,cols_to_add] <- NA
df_current <- df_current[,final_cols$column_name]

# Add rows 
master_df <- rbind(df_current, df)

# Change NA values to empty cells
master_df[is.na(master_df)] <- ""

# Save
write.csv(master_df, paste0(data_dir, "/Master Datasheets/PointLevel/PointLevel_Master_Datasheet_",  Sys.Date(), ".csv"), row.names=FALSE)

## ---- Import/clean management data from jotform ----

# Import and format ACTION management questionnaire
ACTION<-read.csv("C:/Users/leash/OneDrive - Point Blue/Documents/CropC/ACTION_Management_Questionnaire_Export_2025.08.csv")

# Import latest jotform submission
man_df_list <- list.files(paste(data_dir,"Raw Data","Management Data", sep="/"), pattern = "\\.csv$", full.names = TRUE)
man <- read.csv(man_df_list[which.max(as.Date(gsub("\\D","", man_df_list), format = "%Y%m%d"))])

# Import column names map 
field_col_names <- read.csv("jotform_column_names.csv")
field_names<- field_col_names$new_name

# Import latest field-level database
field_df_list <- list.files(paste(data_dir,"Master Datasheets","FieldLevel", sep="/"), pattern = "\\.csv$", full.names = TRUE) #list all the CSVs in folder
field_df_current <- read.csv(field_df_list[which.max(as.Date(gsub("\\D","", field_df_list), format = "%Y%m%d"))]) #this indexing patterns makes sure we're using the most recent master datasheet

# Rename columns in jotform df and remove NA columns
non_na_cols <- !sapply(man, function(col) all(is.na(col)))
man_clean <- man[, non_na_cols]
field_names <- field_names[non_na_cols]
colnames(man_clean) <- field_names

# Remove columns marked "remove"
man_clean <- man_clean[,!grepl("remove",colnames(man_clean))]

# Create project_name based on farm name, practice year, and conservation practice
prac_abbr <- read.csv("practice_abbreviations.csv") #Check abbreviations with Avalon
practice_label<-prac_abbr[prac_abbr$Practice==man_clean[1,]$cons_practice,]$Abbreviation
man_clean$project_name <- paste(toupper(str_sub(man_clean$farm_name,1,4)),str_sub(man_clean$practice_year,3,4),
                                toupper(practice_label),sep=".")
man_clean <- man_clean[,!names(man_clean)=="farm_name"]

# Bind jotform submission to master field-level database and save
missing_cols <- setdiff(colnames(field_df_current), colnames(man_clean))
man_clean[missing_cols] <- NA
man_clean <- man_clean[colnames(field_df_current)] # Reorder columns to match master df
field_df <- rbind(field_df_current, man_clean) # Append to original df
write.csv(field_df, paste0(data_dir, "/Master Datasheets/FieldLevel/FieldLevel_Master_Datasheet_",  Sys.Date(), ".csv"))

## ---- Store project design info ----

# Define projects of interest
projects <- unique(df$project_id) #use this function if you want to select projects with new lab data processed above, or define as projects <- c("PRJ1","PRJ2",etc)

# Run project design summary function
pd_new <- proj_design(projects)

# Write new project design df
write.csv(pd_new, paste0(data_dir, "/Master Datasheets/ProjectDesign/ProjectDesign_Master_Datasheet_",  Sys.Date(), ".csv"), row.names=FALSE)
