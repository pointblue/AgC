# Title: AgCDataCompile.R
# Author: Lisa Eash
# Date created: 20250402
# Date updated: 20250604
# Purpose: Main script for compiling ag-c master database

# Load packages
source('packages.R')

# Load functions
source('functions.R')

# Authorize google drive access
gs4_auth() #this line will initiate a prompt in your console and take you to your browser to approve access to your Google Drive

# Define data directory
data_dir<-("Z:/Soils Team/AgC Data/")

## ---- Import/clean lab and tap field data ----

# Lab soils data
  #Note: a warning message will appear if there are column names that are not yet included in our master datasheet
lab_clean <- clean_lab_df(data_path = data_dir, 
                          lab = "Cquester", #Options: "Cquester", "Ward"
                          file_name = NA)  #optional- can specify if you know the file name and/or are not working with the most recent lab data

# TAP field data
  #Note: a warning message will appear if there is no volume calculated for bulk density but there are some data in the BD.Vol/BD.Depth columns
tap_clean <- read_excel("TAP Data Entry.xlsx", sheet="DigitizePointData", #Need to get access to read in through google drive
                     na = c("NA", "na", "-", "", " "), col_types = "text") %>%
  clean_tap_df()

## ---- Bulk density and biomass calculations ----

# Bulk density
df <- lab_clean %>%
  left_join(tap_clean, by = "sample_id") %>%  # Merge lab and tap field data
  mutate(bulk_density = if_else( 
    is.na(bulk_density),   
    dry_soil_g / vol_cm3, # If NA, calculate as Dry.Mass / Volume
    NA
  ))

# Biomass calculations

## ---- QA/QC of full dataset ----

out_of_range(df, "bulk_density", 0.9, 1.8) #Bulk density between 0.9 and 1.8 g/cm3
out_of_range(df, "total_c", 0.1, 20) #total c %
out_of_range(df, "pH", 4, 9) #pH

#Org + inorg c = total c
df %>%
  mutate(inorg_c = replace_na(inorg_c, 0)) %>%
  filter(inorg_c + org_c != total_c)

#sand silt clay close to 100
df %>%
  filter(sand + silt + clay < 99 | sand + silt + clay > 101)

## ---- Fill in identifying columns and bind to most recent point-level master database ----

# Import current master database
master_df_list<-list.files(paste(data_dir,"Raw Data","Lab Data", sep="/"), pattern = "\\.csv$", full.names = TRUE) #list all the CSVs in folder
df_current <- read.csv(master_df_list[which.max(as.Date(gsub("\\D","", master_df_list), format = "%Y%m%d"))]) #this indexing patterns makes sure we're using the most recent master datasheet

#Add rows and save 
master_df <- rbind(df_current, df)
write.csv(master_df, paste0(data_dir, "/MasterDatasheets/ACTION_Master_Datasheet_",  Sys.Date(), ".csv"))

## ---- Import/clean management data from jotform ----

man <- read.csv("Jotform_example.csv")
col_names <- read.csv("jotform_column_names.csv")
colnames(man) <- col_names$new_name
head(man)
