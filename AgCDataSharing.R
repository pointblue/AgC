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
  DSA_metadata <- file.path("C:/Users/leash/OneDrive - Point Blue", "PointBlue Programs - Shared Soils Program","Ag-C",
                                 "Ag-C Database","AgC_DSA_metadata.xlsx")
  
  # DSA level(s) to share
  levels<-c(1,2,3) 


# Run function to filter projects by DSA level - will return a list of dataframes (PointLevel (soils data), FieldLevel (management data),
#   PointBiomass (point-level biomass) and FieldBiomass (field-level biomass))

data_to_share <- filter_projects_DSA(data_path = data_dir,
                                     DSA_metadata_path = DSA_metadata,
                                     DSA_levels = levels,
                                     soils = "Y", # Select whether you want to include soils data, management data, and/or biomass data (options "Y", "N")
                                     biomass = "Y",
                                     management = "Y")
pointlevel_df <- data_to_share[[1]]
fieldlevel_df <- data_to_share[[2]]
pointbio_df <- data_to_share[[3]]
fieldbio_df <- data_to_share[[4]]

