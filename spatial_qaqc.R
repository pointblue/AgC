# Title: spatial_qaqc.R
# Author: Avalon Cook
# Date created: 20260323
# Date updated: 20260323
# Purpose: This script checks Ag-C spatial data to ensure it adheres to program-specific rules. 
            #Spatial data can be checked by reading in files from a specified directory, listing projects present in the Z drive, or 
#   1) All projects have a point object and a polygon object
#   3) All geometries are valid
#   4) No mulipart objects
#   4) All CRS = 4326
#   5) All point objects have the following columns: name, proj_name, plot_type
#     5.1) Strings in the name column follow the format AAAA.##.BB.T/C.## OR AAAA.##.BB.T/C.OS##
#     5.2) Strings in the name column do not repeat
#     5.3) Strings in the proj_name column all equal the first 10 characters of the name column AND mathces the first 10 characters of the file name
#     5.4) Strings in the plot_type column are either T or C and always equal the 12th character in the name column
#   6) All polygon objects have the following columns: proj_name, plot_type
#     6.1) Strings in the proj_name column Mathces the first 10 characters of the file name
#     6.2) The point object corresponding to the polygon object have all identical values in the proj_name column
#   7) All points intersect with a polygon
#   8) All points intersecting with the polygon have matching values in plot_type
#   9) Every unzipped shapefile has a required extension file: .shp, .shx, .dbf, and .prj

# ---- Setup ----
source('packages.R')
source('functions.R')

# ---- Define spatial files of interest ----

  ## ---- Method 1: Read in a file manually ----
  dir<-"C:/Users/acook-SEA/OneDrive - Point Blue/PointBlue Programs - Shared Soils Program/Ag-C/Internal Ag-C Projects/JPVFreestone/Spatial"
  border_file_name<-"JPPN.18.SC_border.zip"
  point_file_name<-"JPPN.18.SC_pointsintial.zip"
  border<-read_spatial(dir, border_file_name)
  points<-read_spatial(dir, point_file_name)

# ---- 1)  All projects have a point object and a polygon object
  