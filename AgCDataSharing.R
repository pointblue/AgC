# Title: AgCDataSharing.R
# Author: Lisa Eash
# Date created: 20260313
# Purpose: Script for sharing ag-c data based on data-sharing permissions

# Load packages
source('packages.R')

# Load functions
source('functions.R')

# Define parameters 
  # Data directory for compiled datasheets
  data_dir<-("Z:/Soils Team/AgC Data/")
  
  # File path for DSA metadata
  cdrive <- "C:/Users/leash/OneDrive - Point Blue"
  DSA_metadata <- file.path(cdrive, "PointBlue Programs - Shared Soils Program","Ag-C",
                                 "Ag-C Database","AgC_DSA_metadata.xlsx")
  
  # File path for Shared Data 
  shared_data <- paste(cdrive, "PointBlue Programs - Shared Soils Program/Ag-C/Ag-C Database/Shared Data", sep = "/")
  
  # DSA level(s) to share
  levels<-c(1,2,3) 
  
  # Name of collaborating organization 
  org_name <- "GLDI"


# Run function to filter projects by DSA level - will return a list including a row of info required for the data sharing tracker and 
#   dataframes (PointLevel (soils data), FieldLevel (management data), PointBiomass (point-level biomass) and FieldBiomass (field-level biomass))

data_to_share <- filter_projects_DSA(data_path = data_dir,
                                     DSA_metadata_path = DSA_metadata,
                                     DSA_levels = levels,
                                     soils = "Y", # Select whether you want to include soils data, management data, and/or biomass data (options "Y", "N")
                                     biomass = "Y",
                                     management = "Y")
forDSAtracker <- data_to_share[[1]]
pointlevel_df <- data_to_share[[2]]
fieldlevel_df <- data_to_share[[3]]
pointbio_df <- data_to_share[[4]]
fieldbio_df <- data_to_share[[5]]

# Write shared dfs to password-protected sharepoint folder 
for (df_name in names(data_to_share)[2:5]) {
  
  df <- data_to_share[[df_name]]
  
  # skip NULL or NA-only objects
  if (is.null(df) || (is.atomic(df) && all(is.na(df)))) next
  
  # create filename
  file_name <- paste0(org_name,"_",df_name, "_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
  
  # full path
  out_path <- file.path(shared_data, file_name)
  
  # write csv directly to SharePoint-mapped folder
  write.csv(df, out_path, row.names = FALSE)
}

# Once the files are ready to share, run this code to update the data sharing tracker 

forDSAtracker[2:5] <- basename(forDSAtracker[2:5])
row_DSAtracker <- c(format(Sys.Date(), "%Y-%m-%d"), org_name, forDSAtracker)
DSAtracker_current <- read.csv(file.path(shared_data, "data_sharing_tracker.csv"))
DSAtracker_current[nrow(DSAtracker_current)+1,]<-row_DSAtracker
write.csv(DSAtracker_current, file.path(shared_data, "data_sharing_tracker.csv"), row.names = FALSE)
