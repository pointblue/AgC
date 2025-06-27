# Title: AgCDataCompile.R
# Author: Lisa Eash
# Date created: 20250402
# Date updated: 20250626
# Purpose: Main script for compiling ag-c master database

# Load packages
source('packages.R')

# Load functions
source('functions.R')

# Authorize google drive access
gs4_auth() #this line will initiate a prompt in your console and take you to your browser to approve access to your Google Drive

# Define data directory
data_dir<-("Z:/Soils Team/AgC Data/")

# Define path for Ag C data entry spreadsheet - you should have sharepoint synced to your onedrive so that it can be accessed on remote desktop
agc_data_entry <-"C:/Users/leash/OneDrive - Point Blue/PointBlue Programs - Shared Soils Program/Soils Program Active Research/Ag-C/Internal Ag-C Projects/AgCDataEntry.xlsx"

## ---- Import/clean lab and tap field data ----

# Lab soils data
  #Note: a warning message will appear if there are column names that are not yet included in our master datasheet
lab_clean <- clean_lab_df(data_path = data_dir, 
                          lab = "Ward", #Options: "Cquester", "Ward"
                          file_name = "Ward_data_20250313.csv")  #optional- can specify if you know the file name and/or are not working with the most recent lab data

# TAP field data
  #Note: a warning message will appear if there is no volume calculated for bulk density but there are some data in the BD.Vol/BD.Depth columns
tap_clean <- read_excel(agc_data_entry, sheet="Soils", col_names=TRUE,
                     na = c("NA", "na", "ND", "nd", "-", "--","", " ")) %>%
  clean_tap_df()

## ---- Merge lab_clean and tap_clean dataframes ----
df <- lab_clean %>%
  left_join(tap_clean, by = c("sample_id","b_depth","e_depth")) %>%  # Merge lab and tap field data
  mutate(
    texture_name = coalesce(texture_name.x,texture_name.y),
    ph = coalesce(ph.x, ph.y),
    soil_moisture = coalesce(soil_moisture.x, soil_moisture.y),
    dry_soil_g = coalesce(dry_soil_g.x, dry_soil_g.y)
  ) %>%
  select(-ends_with(c(".y",".x")))

## ---- Bulk density and biomass calculations ----

# Bulk density
df <- df %>%
  mutate(bulk_density = if_else( 
    is.na(bulk_density),   
    dry_soil_g / vol_cm3, # If NA, calculate as Dry.Mass / Volume
    NA
  ))

# Biomass calculations

## ---- QA/QC of full dataset ----

out_of_range(df, "bulk_density", 0.9, 1.8) #Bulk density between 0.9 and 1.8 g/cm3
out_of_range(df, "total_c", 0.1, 20) #total c %
out_of_range(df, "ph", 4, 9) #pH

#Org + inorg c = total c
df %>%
  mutate(inorg_c = replace_na(inorg_c, 0)) %>%
  filter(inorg_c + org_c != total_c)

#sand silt clay close to 100
df %>%
  filter(sand + silt + clay < 99 | sand + silt + clay > 101)

## ---- Fill in identifying columns and bind to most recent point-level master database ----

# Select only columns needed for master database

# Import current master database
master_df_list <- list.files(paste(data_dir,"Master Datasheets","PointLevel", sep="/"), pattern = "\\.csv$", full.names = TRUE) #list all the CSVs in folder
df_current <- read.csv(master_df_list[which.max(as.Date(gsub("\\D","", master_df_list), format = "%Y%m%d"))]) #this indexing patterns makes sure we're using the most recent master datasheet

#Add rows and save 
master_df <- rbind(df_current, df)
write.csv(master_df, paste0(data_dir, "/Master Datasheets/PointLevel/Master_Datasheet_",  Sys.Date(), ".csv"))

## ---- Import/clean management data from jotform ----

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


