# Title: AgCDataCompile.R
# Author: Lisa Eash
# Date created: 20250402
# Date updated: 20251202
# Purpose: Main script for compiling ag-c master database

# Load packages
source('packages.R')

# Load functions
source('functions.R')

# Define data directory
data_dir<-("Z:/Soils Team/AgC Data/")

# Define path for Ag C data entry spreadsheet - you should have sharepoint synced to your onedrive so that it can be accessed on remote desktop
#agc_data_entry <-"C:/Users/leash/OneDrive - Point Blue/PointBlue Programs - Shared Soils Program/Ag-C/Internal Ag-C Projects/AgCDataEntry.xlsx" #for lisa
agc_data_entry <- "C:/Users/acook-SEA/OneDrive - Point Blue/PointBlue Programs - Shared Soils Program/Ag-C/Internal Ag-C Projects/AgCDataEntry.xlsx" #for avalon

# identify vector of projects i.e. proj_of_int <- c("ABCD.24.PG","WXYZ.24.CC")
proj_of_int <- c("MAAN.26.SC", "MAAN.27.RP")

## ---- Import/clean tap biomass  data ---- 
tap_biomass <- clean_tap_biomass(agc_data_entry, proj_of_int)

## ---- Save biomass  data ---- 
for(df_name in names(tap_biomass)){
  bio_df <- tap_biomass[[df_name]]
  
  # Convert sample_date to timestamp for compatibility with FarmOS
  date_cols <- c("sample_date_ahb","sample_date_hrb","sample_date_awb")
  
  bio_df <- bio_df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(date_cols),
        ~ {
          x <- as.character(.x)
          x[x %in% c("", "NA")] <- NA
          as.POSIXct(
            paste(x, "12:00:00"),
            format = "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          )
        }
      )
    )
  
  # Change NA values to empty cells
  bio_export <- bio_df %>%
    dplyr::mutate(
      dplyr::across(
        everything(),
        ~ ifelse(is.na(.x), "", as.character(.x))
      )
    )
  
  # Write files 
  write.csv(bio_export, paste0(data_dir, "/Master Datasheets/Biomass/", df_name,"_datasheet_",  Sys.Date(), ".csv"), row.names=FALSE)
}


## ---- Import/clean lab and tap soils data ----

# Lab soils data - extracts all lab data in Z drive for defined projects
  #Note: a warning message will appear if there are column names that are not yet included in our master datasheet
lab_clean <- clean_lab_df(data_path = data_dir, 
                          projects = proj_of_int)

  # Check for duplicated sample id/year/end depth
  lab_clean[duplicated(lab_clean[, c("sample_id", "year", "e_depth")]), ]

# TAP soils data
  #Note: a warning message will appear if there is no volume calculated for bulk density but there are some data in the BD.Vol/BD.Depth columns
tap_soils <- clean_tap_soils(agc_data_entry, proj_of_int)

## ---- Merge lab_clean and tap_soils dataframes ----
lab_prep <- lab_clean %>%
  mutate(
    sample_id = str_replace_all(sample_id, " ", ""),
    prj = str_sub(sample_id, 1, 10),  # Extract project ID
    adoption_year = as.numeric(paste0(20,str_sub(sample_id, 6, 7))),  # Extract adoption year from sample_id
    year = as.numeric(year)
  ) %>%
  group_by(prj) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    first_year = min(year, na.rm = TRUE),
    start_rank = if_else(first_year <= first(adoption_year) + 1, 0, 1),
    year_rank = dense_rank(year) - 1,
    timepoint = paste0("T", year_rank + start_rank)
  ) %>%
  ungroup() %>%
  select(-prj, -adoption_year, -first_year, -start_rank, -year_rank)

lab_prep <- lab_prep %>%
  mutate(
    b_depth = as.character(b_depth),
    e_depth = as.character(e_depth)
  )
tap_soils <- tap_soils %>%
  mutate(
    b_depth = as.character(b_depth),
    e_depth = as.character(e_depth)
  )

if (sum(!is.na(lab_prep$b_depth)) > 0) {
  
  # rows WITH depth → join using depth
  df_depth <- lab_prep %>%
    filter(!is.na(b_depth)) %>%
    left_join(
      tap_soils,
      by = c("sample_id", "timepoint", "b_depth", "e_depth")
    )
  
} else {

  df_depth <- lab_prep %>%
    filter(!is.na(b_depth))
}

# rows WITHOUT depth → join without depth
df_nodepth <- lab_prep %>%
  filter(is.na(b_depth)) %>%
  left_join(
    tap_soils,
    by = c("sample_id", "timepoint")
  ) %>%
  mutate(    b_depth = coalesce(b_depth.x, b_depth.y),
             e_depth = coalesce(e_depth.x, e_depth.y)) %>%
  select(-c(b_depth.x,b_depth.y,e_depth.x,e_depth.y))

df <- bind_rows(df_depth, df_nodepth) %>%
  mutate(
    texture_name = coalesce(texture_name.x, texture_name.y),
    ph = coalesce(ph.x, ph.y),
    soil_moisture = coalesce(soil_moisture.x, soil_moisture.y),
    dry_soil_g = coalesce(dry_soil_g.x, dry_soil_g.y),
    rocks_g = coalesce(rocks_g.x, rocks_g.y)
  ) %>%
  mutate(ph_method = case_when(
    !is.na(ph.x) ~ "lab",
    is.na(ph.x) & !is.na(ph.y) ~ "field",
    TRUE ~ NA_character_
  )) %>%
  select(-ends_with(c(".x", ".y"))) %>%
  mutate(across(c(total_n:cec_na_perc), as.numeric))

#Check for sample_ids not found in tap_soils
df[is.na(df$project_id),]$sample_id #CHECK! Returns 

## ---- Bulk density and biomass calculations ----

# Bulk density
df <- df %>%
  mutate(
    bulk_density = coalesce(bulk_density, dry_soil_g / vol_cm3) # Calculate as Dry.Mass / Volume
  )

## ---- Fill in all identifying columns ----

# Add coordinates
projects <- unique(df$project_id[!is.na(df$project_id)])
samp_coords <- coord_extract(projects)
df <- df[,!names(df) %in% c("long","lat")] %>%
  left_join(samp_coords)

# Store target depths and measured depths
df <- df %>%
  mutate(target_depth = paste(b_depth, e_depth, sep="-")) %>%
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
df[is.na(df$project_id),]$sample_id
df[is.na(df$lat) | is.na(df$long),]
df[is.na(df$total_c) & is.na(df$org_c),]
df[is.na(df$bulk_density),]$sample_id

# Check for values out of range
out_of_range(df, "bulk_density", 0.5, 2.0) #Bulk density between 0.5 and 1.8 g/cm3
out_of_range(df[!is.na(df$org_c),], "org_c", 0.1, 20) #total c %
out_of_range(df[!is.na(df$ph),], "ph", 4, 9) #pH #CHECK returning NA values

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

# Convert sample_date to timestamp for compatibility with FarmOS
df<-df[!is.na(df$sample_date),]
df$sample_date <- as.character(as.POSIXct(paste(df$sample_date, "12:00:00"), tz = "UTC"))


# Import current master database
master_df_list <- list.files(paste(data_dir,"Master Datasheets","PointLevel", sep="/"), pattern = "\\.csv$", full.names = TRUE) #list all the CSVs in folder
df_current <- read.csv(master_df_list[which.max(as.Date(gsub("\\D","", master_df_list), format = "%Y%m%d"))]) #this indexing patterns makes sure we're using the most recent master datasheet
#df_current$sample_date<-as.Date(df_current$sample_date) #make sure the date column is in date format

# Add rows 
master_df <- rbind(df_current, df)

# Change NA values to empty cells
master_df <- master_df %>%
  mutate(
    across(1:7, ~ .),
    # For all other columns, convert to character and replace NA with ""
    across(8:ncol(master_df), ~ {
      x <- as.character(.)
      x[is.na(x)] <- ""
      x
    })
  )

# Save
write.csv(master_df, paste0(data_dir, "/Master Datasheets/PointLevel/PointLevel_Master_Datasheet_",  Sys.Date(), ".csv"), row.names=FALSE)

## ---- Import/clean management data from jotform ----

# Identify latest jotform submission (you can also directly specify file name if it's not the latest download)
man_df_list <- list.files(paste(data_dir,"Raw Data","Management Data", sep="/"), pattern = "\\.csv$", full.names = TRUE)
man_raw <- man_df_list[which.max(as.Date(gsub("\\D","", man_df_list), format = "%Y%m%d"))]

# Run function to clean management data
man_clean <- clean_management(man_raw) # specify file path

# Bind new management data to master field-level database and save
field_db_list <- list.files(paste(data_dir,"Master Datasheets","FieldLevel", sep="/"), pattern = "\\.csv$", full.names = TRUE)
field_db_current <- read.csv(field_db_list[which.max(as.Date(gsub("\\D","", field_db_list), format = "%Y%m%d"))])
field_df <- rbind(field_db_current, man_clean) # Append to original df
write.csv(field_df, paste0(data_dir, "/Master Datasheets/FieldLevel/FieldLevel_Master_Datasheet_",  Sys.Date(), ".csv"), row.names = FALSE)


## ---- Store project design info ----

# Define projects of interest
projects <- unique(df$project_id) #use this function if you want to select projects with new lab data processed above, or define as projects <- c("PRJ1","PRJ2",etc)

# Run project design summary function
pd_new <- proj_design(projects)

# Write new project design df
write.csv(pd_new, paste0(data_dir, "/Master Datasheets/ProjectDesign/ProjectDesign_Master_Datasheet_",  Sys.Date(), ".csv"), row.names=FALSE)
