# Title: functions.R
# Author: Lisa Eash
# Date created: 20250402
# Date updated: 20251023
# Purpose: Define Ag-C data cleaning functions used in AgCDataCompile.R to:
#   1) clean_lab_df: Standardizes columns and units from incoming lab data (Ward, Cquester, OSU)
#   2) clean_tap_df: Renames columns and removes extra columns in TAP df
#   3) coord_extract: Extracts coordinates for sampling points from projects of interest
#   4) out_of_range: verify required columns and check for data outside expected ranges
#   5) proj_design: Extracts project design data required for inference score calculation from point level db
#   6) reg_baseline: Associates regional soil carbon baselines with field polygon for producer reports. Returns the 
#       mean, 95% confidence interval and n points for all RACA crop and rangeland points (excluding outliers) 
#       within the Level 3 ecoregion associated with the polygon you read in. The confidence interval is the
#       value +/- the mean, so for example if mean = 5 and ci = 2, the confidence interval would be from 3 to 7

## ---- clean_lab_df function ----

clean_lab_df <- function(data_path, #main data directory (Z:/Soils Program/AgC Data)
                         lab, #as of now, can be "Cquester" or "Ward" or "OSU"
                         file_name #optional- can specify if you know the file name and/or are not working with the most recent lab data
                         ){
  # Import latest csv
  if(is.na(file_name)==TRUE){
    list_dfs<-list.files(paste(data_path,"Raw Data","Lab Data", sep="/"), pattern = "\\.csv$", full.names = TRUE) #list all the CSVs in folder
    list_dfs<-list_dfs[grep(lab,list_dfs)]
    df_name <- list_dfs[which.max(as.Date(gsub("\\D","", list_dfs), format = "%Y%m%d"))] #this indexing patterns makes sure we're using the most recent master datasheet
  }else{
    df_name<-paste(data_path,"Raw Data","Lab Data", file_name, sep="/")
  }
  if(lab %in% c("Ward","Cquester")){
    lab_raw<-read.csv(df_name)
  }
  if(lab == "OSU"){
    lab_raw <- read_excel(df_name, sheet="Data", col_names=FALSE,
                          na = c("NA", "na", "ND", "nd", "-", "--","", " "))
    lab_raw <- lab_raw %>%
      slice(-1) %>%                                # Remove the first row
      rename_with(~ as.character(lab_raw[2, ])) %>%  # Set column names from second row
      slice(-1) %>%
      select(-c("Lab ID", "Dissolved C Fumigated","Dissolved C Non-Fumigated")) %>%
      as.data.frame
  }

  # Rename columns
  col_map <- read.csv("lab_column_names.csv")
  if(lab == "OSU"){
    rename_vec <- setNames(as.character(col_map$OSU), col_map$Column.Name)
    rename_vec <- gsub("\\.", "", rename_vec)
    lab_clean <- lab_raw %>%
      rename(!!!rename_vec[rename_vec %in% colnames(.)])
  }
  if(lab == "Cquester"){
    rename_vec <- setNames(as.character(col_map$Cquester), col_map$Column.Name)
    rename_vec <- gsub("\\.", "", rename_vec)
    lab_clean <- lab_raw %>%
      slice(-1) %>%
      setNames(gsub("\\.", "", names(.))) %>%
      rename(!!!rename_vec[rename_vec %in% colnames(.)])
  }
  if(lab == "Ward"){
    rename_vec <- setNames(as.character(col_map$Ward), col_map$Column.Name) #Define ward column map
    lab_clean <- lab_raw %>%
      select(-c(Kind.Of.Sample:Field.ID,Date.Recd,Date.Rept,Past.Crop)) %>% #Remove extra id columns
      select(where(~ !all(is.na(.)))) %>% # Remove columns with no data
      rename(!!!rename_vec[rename_vec %in% colnames(.)]) %>% # Rename remaining columns
      mutate(b_depth = round(b_depth*2.54,0), # Convert depths from in to cm 
             e_depth = round(e_depth*2.54,0))
    if("total_n" %in% colnames(lab_clean)){ # Convert ppm to percent
      lab_clean <- lab_clean %>%
        mutate(total_n = total_n/10000)
    }
    if("rocks_g" %in% colnames(lab_clean)){
      lab_clean <- lab_clean %>%
        mutate(rocks_g = ifelse(rocks_g == "< 0.01", 0, rocks_g))
    }
    if("coarse_g" %in% colnames(lab_clean)){
      lab_clean <- lab_clean %>%
        mutate(coarse_g = ifelse(coarse_g == "< 0.01", 0, coarse_g))
    }
  }
  
  #Define column names from cleaned df
  clean_col_names <- names(rename_vec)[names(rename_vec) %in% colnames(lab_clean)]
  clean_col_names <- clean_col_names[!duplicated(clean_col_names)]
  
  # Check for no unexpected columns in lab raw data
  if(length(clean_col_names) != ncol(lab_clean)) {
    message("Extra columns in lab df: ", paste(colnames(lab_clean)[!colnames(lab_clean) %in% names(rename_vec)]))
  }
  
  # Make sure columns are numeric, fill in NAs where blank
  lab_clean <- lab_clean %>%
    select(c(clean_col_names)) %>%
    mutate(across(everything(), ~ ifelse(. %in% c("-","--",""," ","NA","na"), NA,.))) %>%
    mutate(across(-c(clean_col_names[clean_col_names %in% c("sample_id","texture_name")]),
           as.numeric))
  
  # Define lab and C analysis method
  lab_clean$lab_name <- lab
  lab_clean$c_method <- ifelse(lab %in% c("Cquester","Ward"),"Dry Combustion",NA)
  
  #Add required columns that were not in raw lab data 
  cols_to_add <- col_map$Column.Name[!col_map$Column.Name %in% names(lab_clean)]
  lab_clean[,cols_to_add] <- NA
  lab_clean <- lab_clean %>%
    select(col_map$Column.Name)
  
  #Add year that data were reported from lab 
  lab_clean$year <- str_extract(df_name, "\\d{4}(?=\\d{4}\\.csv)")
  
  return(lab_clean)
}

## ---- clean_tap_df function ----

clean_tap_df <- function(agc_data_entry_path){
  #Read in soils and biomass data sheets
  tap_soils<-read_excel(agc_data_entry_path, sheet="Soils", col_names=TRUE,
             na = c("NA", "na", "ND", "nd", "-", "--","", " "))
  tap_bio<-read_excel(agc_data_entry_path, sheet="AbovegroundHerbaceousBiomass", col_names=TRUE,
                       na = c("NA", "na", "ND", "nd", "-", "--","", " "))
  tap_herb_root<-read_excel(agc_data_entry_path, sheet="HerbaceousRootBiomass", col_names=TRUE,
                      na = c("NA", "na", "ND", "nd", "-", "--","", " "))
  #Remove extra columns, define column types
  soils_clean <- tap_soils %>%
    slice(-1) %>% # remove unit row
    mutate(SamplingDate = case_when(
      !is.na(as.numeric(SamplingDate)) & !grepl("/", SamplingDate) ~ as.Date(as.numeric(SamplingDate), origin = "1899-12-30"),
      TRUE ~ as.Date(SamplingDate, format = "%m/%d/%Y")
    )) %>%
    mutate(across(c(BdepthTarget_cm:Edepth_cm,pH_infield), as.numeric),
           Bdepth_cm = round(Bdepth_cm,1),
           Edepth_cm = round(Edepth_cm,1)
           ) %>%
    filter(!is.null(PointID))%>% #filter out empty rows
    filter(!is.na(PointID))%>% #filter out empty rows
    mutate(across(c(Volume1_mL:Depth4_cm), as.numeric))
  
  bio_clean <- tap_bio %>%
    slice(-1) %>% # remove unit row
    mutate(SamplingDate = format(as.Date(as.numeric(SamplingDate), origin = "1899-12-30"), "%Y-%m-%d")) %>%
    filter(!is.null(PointID))%>% #filter out empty rows
    filter(!is.na(PointID))%>% #filter out empty rows
    mutate(across(c(Area_cm2:DryMass_g), as.numeric)) %>%
    select(PointID, Timepoint, SamplingDate, Area_cm2,DryMass_g)
  
  root_bio_clean <- tap_herb_root %>%
    slice(-1) %>% # remove unit row
    mutate(sample_date_hrb = as.Date(SamplingDate, format = "%m/%d/%Y")) %>%
    filter(!is.null(PointID))%>% #filter out empty rows
    filter(!is.na(PointID))%>% #filter out empty rows
    select(PointID, Timepoint, sample_date_hrb, BdepthTarget_cm,EdepthTarget_cm,
           CoreDiameter_cm, TinMassFine_g:DryMassCourse_g) %>%
    mutate(across(c(CoreDiameter_cm:DryMassCourse_g), as.numeric))
    
  
  #Determine bulk density method  
  tap_clean <- soils_clean %>%
    mutate(bd_method = case_when( 
      !is.na(Volume1_mL) | !is.na(Volume2_mL) | !is.na(Volume3_mL) | !is.na(Volume4_mL) ~ "Millet",
      !is.na(Depth1_cm) | !is.na(Depth2_cm) | !is.na(Depth3_cm) | !is.na(Depth4_cm) ~ "Ruler",
        TRUE ~ NA_character_  
    ))
  
  #Calculate volume 
  tap_clean <- tap_clean %>%
    mutate(vol_cm3 = case_when( #calculate sample volumes based on all possible scenarios
    !is.na(Volume3_mL) & !is.na(Volume4_mL) ~ (Volume3_mL + Volume4_mL) / 2,  # Case 1: vol3 and vol4 have values (first two measurements were off)
    !is.na(Volume1_mL) & !is.na(Volume2_mL) & is.na(Volume3_mL) & is.na(Volume4_mL) ~ (Volume1_mL + Volume2_mL) / 2,  # Case 2: vol1 and vol2 are present, none for 3 and 4 (first two measurements were good)
    is.na(Volume1_mL) & is.na(Volume2_mL) & is.na(Volume3_mL) & is.na(Volume4_mL) & # Case 3; No volume measurements are present -- depths must be used to calculate sample volume
      ( !is.na(Depth1_cm) | !is.na(Depth2_cm) | !is.na(Depth3_cm) | !is.na(Depth4_cm) ) ~
      rowMeans(select(., Depth1_cm, Depth2_cm, Depth3_cm, Depth4_cm), na.rm = TRUE) * pi * ((2*2.54) / 2)^2, 
    TRUE ~ NA_real_  # no volumes or depths are present; bulk density is not included
  ))
  
  #Make sure that all rows that have NA for volume have no Vol or Depth data
  na_rows <- tap_clean[is.na(tap_clean$vol_cm3),] %>%
    filter(if_all(c(Volume1_mL:Depth4_cm), ~ !is.na(.))) %>%
    select(PointID)
  if(nrow(na_rows) > 0) {
    message("Sample IDs with vol/depth input but no calculated volume:", paste(na_rows$Sample.ID))
  }
  
  #Calculate soil moisture and dry soil mass
  tap_clean <- tap_clean %>%
    mutate(across(c(WetMass_g, RocksRemovedMass_g:MoistureSubsDryMass_g), as.numeric)) %>%
    mutate(soil_moisture = (MoistureSubsWetMass_g-MoistureSubsDryMass_g)/MoistureSubsDryMass_g*100,
           dry_soil_g = (WetMass_g - RocksRemovedMass_g - RootsRemovedMass_g)*((100-soil_moisture)/100))
  
  #Calculate aboveground herb biomass
  bio_clean <- bio_clean %>%
    mutate(abh_bio = DryMass_g/1000/(Area_cm2/100000000) #calculate biomass in kg/ha
           ) %>%
    select(PointID,Timepoint,SamplingDate,abh_bio) %>%
    rename(sample_date_abh = SamplingDate) %>%
    group_by(PointID, Timepoint, sample_date_abh) %>%
    summarise(abh_bio = mean(abh_bio, na.rm = TRUE))
  
  #Calculate root herb biomass
  root_bio_clean <- root_bio_clean %>%
    mutate(core_area_m2 = pi*(CoreDiameter_cm/100/2)^2,
           coarse_roots_g = DryMassCourse_g - TinMassCourse_g,
           fine_roots_g = DryMassFine_g - TinMassFine_g) %>%
    mutate(hrb_fine = fine_roots_g/core_area_m2*10000/1000, #calculate herbaceous root biomass in kg/ha
           hrb_coarse = coarse_roots_g/core_area_m2*10000/1000,
           hrb_total = hrb_fine + hrb_coarse) %>%
    rename(e_depth_hrb = EdepthTarget_cm) %>%
    select(PointID, Timepoint, sample_date_hrb, e_depth_hrb, hrb_fine, hrb_coarse, hrb_total)
  
  #Bind sheets together
  tap_clean <- merge(tap_clean,bio_clean,by=c("PointID","Timepoint"),all.x=TRUE,all.y=TRUE)
  tap_clean <- merge(tap_clean,root_bio_clean,by=c("PointID","Timepoint"),all.x=TRUE,all.y=TRUE)
  
  #Select rows
  tap_clean <- tap_clean %>%
    rename(project_id = ProjectID,
           plot_type = PlotType,
           sample_id = PointID,
           protocol = Protocol,
           timepoint = Timepoint,
           sample_date = SamplingDate,
           b_depth = BdepthTarget_cm,
           e_depth = EdepthTarget_cm,
           b_depth_meas = Bdepth_cm,
           e_depth_meas = Edepth_cm,
           position = Position,
           texture_name = Texture_infield,
           ph = pH_infield,
           rocks_g = RocksRemovedMass_g) %>%
    select(c(project_id,sample_id,plot_type,protocol, timepoint, sample_date, b_depth, e_depth, 
             b_depth_meas,e_depth_meas,bd_method,position, texture_name, ph, soil_moisture, dry_soil_g,
             rocks_g, vol_cm3,sample_date_abh, abh_bio, sample_date_hrb, e_depth_hrb, hrb_fine, hrb_coarse, hrb_total)) %>%
    mutate(year = str_sub(sample_date, 1,4)) %>%
    filter(!is.na(project_id)) %>%
    as.data.frame()
    
  return(tap_clean)
}

## ---- Extract point coordinates for sampling points ----
coord_extract <- function(projects){
  # Create df to store results
  coord_df <- data.frame(
    sample_id = character(),
    lat = numeric(),
    long = numeric())
  # Loop through projects
  for(p in projects){
    
    # Define file paths
    zip_path <- paste0(data_dir,"Raw Data/Spatial Data/ZippedShapefiles/",
                       p,"_pointsfinal.zip")
    
    # Create a temporary directory to unzip files
    unzip_dir <- paste0(data_dir,"Raw Data/Spatial Data/temp_unzip/")
    dir.create(unzip_dir)
    
    # Unzip the shapefile
    unzip(zip_path, exdir = unzip_dir)
    
    # Read shapefile (automatically finds the .shp)
    shape_data <- st_read(paste0(unzip_dir,p,"_pointsfinal.shp"))
    
    # Extract geometries
    shape_coords <- shape_data %>%
      mutate(long = st_coordinates(geometry)[,1],
             lat = st_coordinates(geometry)[,2]) %>%
      st_drop_geometry() %>%
      rename(sample_id = name) %>%
      select(sample_id, long, lat)
    
    # Store data
    coord_df <- rbind(coord_df,shape_coords)
    
    # Delete temp folder where file was unzipped
    unlink(unzip_dir, recursive = TRUE)
  }
  return(coord_df)
}

## ---- QAQC function ----

out_of_range <- function(df, var, min, max){
  df %>% 
    filter(.data[[var]] < min | .data[[var]] > max | is.na(.data[[var]]))
}

## ---- Store project design info ----
proj_design <- function(projects){
  
  # Import latest project design df 
  pd_df_list <- list.files(paste(data_dir,"Master Datasheets","ProjectDesign", sep="/"), pattern = "\\.csv$", full.names = TRUE)
  pd_latest <- read.csv(pd_df_list[which.max(as.Date(gsub("\\D","", pd_df_list), format = "%Y%m%d"))])
  
  # Use latest pointlevel master df to populate appropriate columns
  pl_df_list <- list.files(paste(data_dir,"Master Datasheets","PointLevel", sep="/"), pattern = "\\.csv$", full.names = TRUE)
  pl_latest <- read.csv(pl_df_list[which.max(as.Date(gsub("\\D","", pl_df_list), format = "%Y%m%d"))]) %>%
    filter(project_id %in% projects)
  
  # define numeric columns 
  pl_latest <- pl_latest %>%
    mutate(across(5:10, as.character)) %>%
    mutate(across(where(~ is.logical(.) || is.integer(.)), as.numeric))
  
  # summarize to project design df 
  pd <- pl_latest %>%
    group_by(project_id) %>%
    nest() %>%
    mutate(
      indicators = map(data, ~ .x %>%
                         select(where(is.numeric)) %>%
                         keep(~ any(!is.na(.))) %>%
                         names()),
      control_site = map2_lgl(project_id, data, ~ any(str_detect(.y$sample_id, paste0(.x, "\\.C")))),
      control_baseline = map_lgl(data, ~ any(.x$timepoint == "T0")),
      soc_method = map(data, ~ unique(na.omit(.x$c_method))),
      bd_method = map(data, ~ unique(na.omit(.x$bd_method))),
      tx_method = map_chr(data, ~ case_when(
        any(!is.na(.x$sand)) ~ "hydrometer",
        any(!is.na(.x$texture_name)) ~ "feel",
        TRUE ~ NA_character_
      )),
      ph_method = map_chr(data, ~ case_when(
        any(.x$ph_method == "lab", na.rm = TRUE) ~ "lab",
        any(.x$ph_method == "field", na.rm = TRUE) ~ "field",
        TRUE ~ NA_character_
      )),
      soc_num_samples = map_int(data, ~ sum(!is.na(.x$org_c))),
      bd_num_samples = map_int(data, ~ sum(!is.na(.x$bulk_density))),
      tx_num_samples = map_int(data, ~ sum(!is.na(.x$sand))),
      sampling_depth = map_dbl(data, ~ max(as.numeric(str_extract(.x$target_depth, "(?<=_)\\d+")), na.rm = TRUE)),
      depth_increments = map_int(data, ~ n_distinct(.x$target_depth))
    ) %>%
    select(-data) %>% 
  as.data.frame()
  
  pd <- pd %>%
    mutate(across(where(is.list), ~ map_chr(., ~ paste(.x, collapse = ", "))))
  
  #Add columns that cannot be populated using the point-level master df 
  missing_cols <- setdiff(colnames(pd_latest), names(pd))
  pd[missing_cols] <- NA
  pd <- pd[, colnames(pd_latest)]
  
  #Bind to latest project design df
  pd_new <-rbind(pd_latest,pd)
}

## ---- Extract regional baseline for producer reports ----
reg_baseline <- function(polygon #Specify polygon of project
                         ){
  # Find centroid of polygon
  cent <- st_centroid(polygon)[1,]
  
  # Associate centroid of polygon with ecoregion
  ecoregions <- st_read("./Ecoregions/us_eco_l3.shp")
  ecoregions <- st_make_valid(st_transform(ecoregions, st_crs(cent)))
  region <- ecoregions[st_contains(ecoregions, cent, sparse = FALSE), ]
  region_name <-unique(region$US_L3NAME)
  
  # Pull RACA data for ecoregion of interest
  RACA_coords <- read.csv("./RaCA Data/RaCa_general_location.csv") %>% #imports RaCA point coords
    st_as_sf(coords = c("Gen_long", "Gen_lat"), crs = 4326)
  RACA_coords <- st_transform(RACA_coords, st_crs(region))
  RACA_coords <- RACA_coords[st_within(RACA_coords, region, sparse = FALSE), ] #select all points within ecoregion of interest
  RACA_samples <- read.csv("./RaCA Data/RaCA_samples.csv") %>% #Select only range and cropland points
    filter(LU %in% c("R","C")) %>%
    select(rcasiteid, sample.id, LU) %>%
    filter(rcasiteid %in% RACA_coords$RaCA_Id)
  RACA_soc <-read.csv("./RaCA Data/RaCA_SOC_pedons.csv") %>%
    filter(rcasiteid %in% RACA_samples$rcasiteid)
  #Remove outliers
  outliers <- boxplot.stats(RACA_soc$SOCstock30)$out
  RACA_soc_no_out <- RACA_soc[!RACA_soc$SOCstock30 %in% outliers, ]
  RACA_n <- nrow(RACA_soc_no_out) #defines number of sites for regional baseline
  RACA_mean <- mean(RACA_soc_no_out$SOCstock30, na.rm=TRUE)
  RACA_ci <- sd(RACA_soc_no_out$SOCstock30, na.rm=TRUE)/sqrt(RACA_n)*1.96 #creates 95% confidence interval
  RACA_res <- data.frame(ecoregion = region_name, mean = RACA_mean, ci95 = RACA_ci, n = RACA_n)
  return(list(data_points = RACA_soc_no_out, summary_stats = RACA_res))
  }

## ---- format text function ----
#takes in a vector of strings and formats it into a list sentence with oxford comma where relevant
format_list <- function(x) {
  n <- length(x)
  if (n == 0) {
    ""
  } else if (n == 1) {
    x
  } else if (n == 2) {
    paste(x, collapse = " and ")
  } else {
    paste0(
      paste(x[-n], collapse = ", "),
      ", and ",
      x[n]
    )
  }
}

## ---- Render HTML Reports ----
render_one_html <- function(project, raca_filter) {
  #Reports will write to Z:\Soils Team\AgC Data\RenderedReports
  
  rmarkdown::render(
    input = 'LandStewardReports.Rmd', #identify the markdown file that will be used to render the report
    output_file = paste0(project, '_Report_', Sys.Date(), '.html'), #ID the file path and naming pattern
    output_dir = "Z:/Soils Team/AgC Data/RenderedReports",
    params = list(project_name = project, raca_filter=raca_filter), #"project_name" references the name of the parameter in the YAML header; "project" represents the current throughput of the loop
    envir = parent.frame()
  )
}
#layering that so you can pass a vector of project names to render many reports at once
render_html_report <- function(projects, raca_filter){ #when projects is a project name or a vector of project names
  for(project in projects){
    render_one_html(project, raca_filter)
  }
}

## ---- Read GIS data to R ----
read_spatial <- function(dir, file_name) {
  #Read a GIS file into R, even if it's zipped
  #File extension on file_name is required
  
  if (!require(sf)) {
    stop("Package 'sf' is required for this function.")
  }
  
  if (!dir.exists(dir)) {
    stop(paste("Input directory does not exist:", dir))
  }
  
  if (!file.exists(file.path(dir, file_name))){
    stop("Input file does not exist in the input directory.")
  }
  
  if (grepl("\\.(zip|kmz)$", file_name, ignore.case = TRUE)) { #if it's a zipped shapefile for kmz, some extra steps are needed
    temp_folder1 <- tempfile()
    unzip(file.path(dir, file_name), exdir = temp_folder1)
    shp_temppath1<-list.files(temp_folder1, pattern = "(\\.shp$|\\.kml$)", full.names = TRUE)
    border<- read_sf(shp_temppath1)
    on.exit(unlink(temp_folder1, recursive = TRUE), add = TRUE) #wipe the temp folder for clean repeat use
  }else { #otherwise, just read in the file as an sf object
    border<- read_sf(file.path(dir, file_name))
  }
  
  return(st_make_valid(st_zm(border))) #remove z values and ensure valid geometry to avoid errors in processing
}

## ---- Write sf object to zipped shapefile ----
write_zipped_shp <- function (sfobj, dir, file_name, crs=4326){
  #Write an sf object to a zipped shapefile with no loose component parts
  #DO NOT ADD FILE EXTENSIONS TO file_name
  #You can specify a crs, but we default to 4326 for Ag-C storage
  
  if (!require(sf)) {
    stop("Package 'sf' is required for this function.")
  }
  if (!require(zip)) {
    stop("Package 'zip' is required for this function.")
  }
  
  # Export sf object to dir; insure intended crs
  write_sf(st_transform(sfobj, crs), paste0(file.path(dir, file_name), '.shp')) #get sfobj in intended crs before writing
  
  #zip components to intended directory
  exts <- c("shp", "shx", "dbf", "prj") 
  components_relative <- paste0(file_name, ".", exts)
  zip::zip(file.path(dir, paste0(file_name, ".zip")), components_relative, root=dir)
  
  #delete the components
  components_aboslute<-file.path(dir, paste0(file_name, ".", exts))
  for (each in components_aboslute){
    unlink(each)}
  
}

## ---- Write SF object to GPX ----
write_gpx <- function(sdesign, dir, file_name)
{
  st_write(sdesign, 
           paste0(dir, "/", file_name, ".GPX"), 
           driver = "GPX", 
           dataset_options="GPX_USE_EXTENSIONS=yes")
}

## ---- GRTS Sampling Design (not stratified) ----
AgC_GRTS <- function (polygon, proj_name = NA, sdensity, osdensity, plot_type_col="plot_type", buffer=5, mindis = 10, maxtry=20){
  #polygon is an sf object
  #proj_name is the project code. Required
  #sdensity=sampling density (doubled for T+C)
  #osdensity=oversample point density (doubled for T+C)
  #plot_type_col = column name in polygon that identifies plot types. If none, treatment is assumed.
  #buffer is in meters. How far inside the polygon do you want to buffer?
  #mindis = minimum distance between sampling points
  #maxtry = how many times do you want GRTS to try to accomplish the mindis? larger distances require more tries, especially when polygon is small
  
  if (!require(sf)) {
    stop("Package 'sf' is required for this function.")
  }
  if (!require(spsurvey)) {
    stop("Package 'spsurvey' is required for this function.")
  }
  if (!require(dplyr)) {
    stop("Package 'dplyr' is required for this function.")
  }
  if (!require(stringr)) {
    stop("Package 'stringr' is required for this function.")
  }
  
  if (is.na(proj_name)){
    proj_name <- polyon$proj_name[1]
  } else proj_name<-proj_name
  
  if(is.null(polygon[[plot_type_col]]) || 
     sum(!is.na(polygon[[plot_type_col]])) < 2) {
    GRTS_out<-polygon%>%st_transform(5070)%>%#polygon has to be in a projected CRS to run through GRTS
      st_buffer(dist=-buffer)%>%
      grts(n_base=sdensity, n_over=osdensity, mindis = mindis, maxtry=maxtry)
    
    GRTS_out$sites_base$name <- paste0(proj_name, ".T.", sprintf("%02d", 1:nrow(GRTS_out$sites_base)))
    GRTS_out$sites_over$name <- paste0(proj_name, ".T.", "OS", sprintf("%02d", 1:nrow(GRTS_out$sites_over)))
    
    SamplingDesign<-rbind(GRTS_out$sites_base, GRTS_out$sites_over)%>%
      mutate(proj_name = !!proj_name,
             plot_type = "T"
      )%>%
      select(name, plot_type, proj_name)
  }
  
  if(length(unique(polygon[[plot_type_col]]))==2){
    polygonT<-polygon%>%filter(.data[[plot_type_col]]=="T")
    polygonC<-polygon%>%filter(.data[[plot_type_col]]=="C")
    
    GRTS_out_T <-polygonT%>%st_transform(5070)%>%#polygon has to be in a projected CRS to run through GRTS
      st_buffer(dist=-buffer)%>%
      grts(n_base=sdensity, n_over=osdensity, mindis = mindis, maxtry=maxtry)
    GRTS_out_T$sites_base$name <- paste0(proj_name, ".T.", sprintf("%02d", 1:nrow(GRTS_out_T$sites_base)))
    GRTS_out_T$sites_over$name <- paste0(proj_name, ".T.", "OS", sprintf("%02d", 1:nrow(GRTS_out_T$sites_over)))
    
    
    GRTS_out_C <-polygonC%>%st_transform(5070)%>%#polygon has to be in a projected CRS to run through GRTS
      st_buffer(dist=-buffer)%>%
      grts(n_base=sdensity, n_over=osdensity, mindis = mindis, maxtry=maxtry)
    GRTS_out_C$sites_base$name <- paste0(proj_name, ".C.", sprintf("%02d", 1:nrow(GRTS_out_C$sites_base)))
    GRTS_out_C$sites_over$name <- paste0(proj_name, ".C.", "OS", sprintf("%02d", 1:nrow(GRTS_out_C$sites_over)))
    
    SamplingDesign<-rbind(GRTS_out_T$sites_base, GRTS_out_T$sites_over, GRTS_out_C$sites_base, GRTS_out_C$sites_over)%>%
      mutate(proj_name = !!proj_name,
             plot_type = substr(name, 12, 12)
      )%>%
      select(name, plot_type, proj_name)%>%st_transform(4326)
    
  }
  
  return(SamplingDesign%>%st_transform(4326))
  
}

## ---- Get soil taxonomy data ----
#This function allows you to look at soil taxonomy data at the taxonomic level of your choosing
#takes in an sf object polygon

soil_types <- function (polygon, level="series", collapse_series=TRUE, plot=TRUE) {
  #get soil types within a polygon at the desired level of taxonomy
  #level options: "series", "taxorder", "taxclname", "taxsuborder", "taxgrtgroup", "taxsubgrp"
  #polygon is an sf object, polygon or multipolygon should work
  #if level="series", you have the option to collapse data by removing slope distinctions - default true
  #running the function returns a dataframe and plots a map, but you can turn the plotting function off
  
  if (!require(sf)) {
    stop("Package 'sf' is required for this function.")
  }
  if (!require(spsurvey)) {
    stop("Package 'spsurvey' is required for this function.")
  }
  if (!require(dplyr)) {
    stop("Package 'dplyr' is required for this function.")
  }
  if (!require(stringr)) {
    stop("Package 'stringr' is required for this function.")
  }
  if (!require(soilDB)) {
    stop("Package 'soilDB' is required for this function.")
  }
  
  
  areasoils_spatial<-SDA_spatialQuery(border, geomIntersection = TRUE, geomAcres = TRUE, db="SSURGO", what=c('mupolygon')) #load in a dataframe with mukeys and polygon geometry
  
  if (level=="series"){
    areasoils_names<-SDA_spatialQuery(border, geomIntersection = TRUE, geomAcres = TRUE, db="SSURGO", what=c('mukey')) #Load in a dataframe that shows mukeys and names that intersect with our border polygon
    areasoils<- areasoils_spatial %>%
      left_join(areasoils_names, #join the names and spatial data by the mukey column
                by="mukey")
    
    if (collapse_series) {
      #if user designates, we can greatly simplify these maps by removing map units separated by slope categories or rockiness
      areasoils <- areasoils%>%
        filter(mukey!="459902") %>% #if present, remove the water class
        mutate(series_name = str_extract(muname, "^[^,]+"))%>% #extract the name of the series
        group_by(series_name)%>%
        dplyr::summarize(
          area_ac = sum(area_ac),
          series_name = first(series_name),
          geometry = st_union(geom), #merge all polygons that have the same series name
        )%>%
        st_make_valid()#avoid errors due to invalid geometry
      
      plot_col<-"series_name"}else{plot_col<-"muname"}
    
    if (plot) {
      areasoils[[plot_col]]<-str_wrap(areasoils[[plot_col]], 20) #
      plot(areasoils[plot_col], main = "Soil Series", key.pos = 1)
    }
    
  }
  
  if (level!="series"){
    areasoils_spatial <- areasoils_spatial%>%
      st_collection_extract() #extract geometry collections
    taxdata <- get_SDA_property(property=level, 
                                method = "Dominant Component (Category)", 
                                mukeys=as.numeric(areasoils_spatial$mukey)) 
    
    areasoils <- left_join(areasoils_spatial, taxdata, by="mukey") %>%
      group_by(.data[[level]]) %>%
      dplyr::summarize(
        area_ac = sum(area_ac),
        geometry = st_union(geom),
        .groups = 'drop') %>% st_as_sf() %>%
      st_make_valid
    if(plot){
      plot(areasoils[, level, drop = FALSE], main = "Soil taxonomy", key.pos = 1)
    }
  }
  return(areasoils)
  
}

## ---- Allocate sampling points to strata ----
#This function is used in the AgC_GRTS_strat()
#It is meant to solve the problem where rounded proportions do not always add up to the intended value
#Based on the "Hamilton method" or "Largest Remainder Method" with small modification to ensure no strata is empty
#heavily relied on chatgpt for this
#https://en.wikipedia.org/wiki/Quota_method
#https://stackoverflow.com/questions/13483430/how-to-make-rounded-percentages-add-up-to-100

strat_alloc <- function(x, target) {
  # scale to target
  scaled <- x / sum(x) * target
  
  # take floors, but ensure minimum of 1
  floored <- pmax(floor(scaled), 1)
  
  # calculate how many units we currently have
  current_sum <- sum(floored)
  diff <- round(target - current_sum)
  
  remainder <- scaled - floor(scaled)
  
  # If total < target: add to those with largest remainders
  if (diff > 0) {
    add_index <- order(remainder, decreasing = TRUE)[seq_len(diff)]
    floored[add_index] <- floored[add_index] + 1
  }
  # If total > target: subtract from those with smallest remainders,
  # but only where value > 1 to avoid zeros
  else if (diff < 0) {
    candidates <- which(floored > 1)
    sub_index <- candidates[order(remainder[candidates], decreasing = FALSE)[seq_len(abs(diff))]]
    floored[sub_index] <- floored[sub_index] - 1
  }
  
  floored
}

## ---- GRTS Sampling Design STRATIFIED ----
AgC_GRTS_strat <- function (polygon, soils, sdensity, osdensity=NULL, proj_name = NA, plot_type_col="plot_type", buffer=5, mindis = 10, maxtry=20){
  #polygon is an sf object
  #soils is an output from soil_types
  #proj_name is the project code. Required
  #sdensity=sampling density (doubled for T+C)
  #osdensity=oversample point density (doubled for T+C)
  #plot_type_col = column name in polygon that identifies plot types. If none, treatment is assumed.
  #buffer is in meters. How far inside the polygon do you want to buffer?
  #mindis = minimum distance between sampling points
  #maxtry = how many times do you want GRTS to try to accomplish the mindis? larger distances require more tries, especially when polygon is small
  
  if (!require(dplyr)) {
    stop("Package 'dplyr' is required for this function.")
  }
  if (!require(stringr)) {
    stop("Package 'stringr' is required for this function.")
  }
  if (!require(sf)) {
    stop("Package 'sf' is required for this function.")
  }
  if (!require(purrr)) {
    stop("Package 'purrr' is required for this function.")
  }
  if (!require(spsurvey)) {
    stop("Package 'spsurvey' is required for this function.")
  }
  
  if (is.na(proj_name)){
    proj_name <- polyon$proj_name[1]
  } else proj_name<-proj_name
  
  #Step1: Prep data
  
  polygon <- polygon %>%
    st_transform(5070) %>%
    group_by(.data[[plot_type_col]]) %>%
    summarise(proj_name = first(proj_name),
              .groups = "drop")
  
  strat_border <- st_transform(soils, 5070) %>% 
    st_intersection(polygon) %>% #intersect polygon and soils ensuring a projected CRS
    mutate(area_ac = as.numeric(st_area(geometry)/4046.856)) %>% #recalculate area
    group_by(.data[[plot_type_col]])%>%
    mutate(
      perc_area = as.numeric(area_ac/sum(area_ac)*100) #get percent area per strata
    )
  
  #This bit merges polygons that are very small <5% area
  small_polys <- strat_border %>% filter(perc_area < 5)
  large_polys <- strat_border %>% filter(perc_area >= 5)
  if (nrow(small_polys) > 0) {
    message("A soil type polygon covering <5% area was detected. It has been merged with the nearest larger polygon")
    nearest_idx <- st_nearest_feature(small_polys, large_polys)
    
    merged <- map2_dfr(seq_len(nrow(small_polys)), nearest_idx, function(i, j) {
      st_union(small_polys[i, ], large_polys[j, ]) %>%
        mutate(
          !!plot_type_col := large_polys[[plot_type_col]][j]
        )
    })
    
    
    # Drop the polygons that got merged and add the merged ones back
    strat_border <- bind_rows(
      large_polys[-nearest_idx, ],
      merged
    )
    
    # Recalculate area and perc_area after merging
    strat_border <- strat_border %>%
      mutate(area_ac = as.numeric(st_area(geometry) / 4046.856)) %>%
      group_by(.data[[plot_type_col]]) %>%
      mutate(perc_area = area_ac / sum(area_ac) * 100) %>%
      ungroup() %>%
      select(area_ac, proj_name, plot_type, geometry, perc_area)
  }
  
  if (any(dplyr::count(strat_border, .data[[plot_type_col]])$n > 3)){
    message("Warning: This polygon contains more than 3 strata, which increases the chances that smaller strata
            are undersampled if sdensity isn't sufficiently large. It also increases the chances that rounding
            errors in the sampling point allocation process will lead to a sum near, but unequal to, sdensity")
  }
  
  #Step2: Set up GRTS loop for T and T/C
  
  #setting up an empty dataframe to append rows to in the loop
  SamplingDesign <- st_sf( 
    name = character(),
    Lat = integer(),          
    Lon = character(),        
    geometry = st_sfc(crs=5070)
  )
  
  # ptype<-unique(strat_border[[plot_type_col]])[1] #troubleshooting line
  for (ptype in unique(strat_border[[plot_type_col]])) {
    
    #Isolate the current plot type
    strat_border.i<-strat_border%>%filter(.data[[plot_type_col]] == ptype)
    
    alloc.s<-strat_border.i$perc_area*sdensity/100
    alloc.os<-strat_border.i$perc_area*osdensity/100
    strat_border.i$sdens<-strat_alloc(alloc.s, target=sdensity)
    strat_border.i$osdens<-strat_alloc(alloc.os, target=osdensity)
    
    current_count_b<-0
    current_count_o<-0
    
    for (i in 1:nrow(strat_border.i)){
      
      #Isolate the current plot type
      stratum<-strat_border.i[i,]
      
      #call sampling density for the current stratum
      sdensity.i<-strat_border.i$sdens[i]
      osdensity.i<-strat_border.i$osdens[i]
      
      GRTS_out<-stratum%>%
        st_buffer(dist=-buffer)%>%
        grts(n_base=sdensity.i, n_over=osdensity.i, mindis = mindis, maxtry=maxtry)
      
      GRTS_out$sites_base$name<-paste0(proj_name, ".",ptype ,".", sprintf("%02d", (current_count_b+1):(current_count_b+nrow(GRTS_out$sites_base))))
      GRTS_out$sites_over$name<-paste0(proj_name, ".",ptype ,".", "OS", sprintf("%02d", (current_count_o+1):(current_count_o+nrow(GRTS_out$sites_over))))
      
      current_count_b <- as.numeric(str_sub(GRTS_out$sites_base$name[length(GRTS_out$sites_base$name)], -2, -1))
      current_count_o <- as.numeric(str_sub(GRTS_out$sites_base$name[length(GRTS_out$sites_base$name)], -2, -1))
      
      SamplingDesign.i<-rbind(GRTS_out$sites_base, GRTS_out$sites_over)%>%
        mutate(proj_name = !!proj_name,
               plot_type = ptype
        )%>%
        select(name, plot_type, proj_name)
      
      SamplingDesign<-rbind(SamplingDesign, SamplingDesign.i)%>%arrange(name)
      
    }
    
  }
  
  
  return(SamplingDesign%>%st_transform(4326))
  
}

## ---- Create PDF map of sampling design ----
design_map <- function(sdesign, border, dir, key, subtitle="Spatially balanced sampling", plot_type_col="plot_type"){
  
  if (!require(dplyr)) {
    stop("Package 'dplyr' is required for this function.")
  }
  if (!require(stringr)) {
    stop("Package 'stringr' is required for this function.")
  }
  if (!require(sf)) {
    stop("Package 'sf' is required for this function.")
  }
  if (!require(ggspatial)) {
    stop("Package 'ggspatial' is required for this function.")
  }
  if (!require(ggmap)) {
    stop("Package 'ggmap' is required for this function.")
  }
  
  #this bit determines whether we're dealing with an intial or final design based on whether any oversample points are present
  if(any(str_detect(sdesign$name, fixed("T.OS")))){
    title <- "Monitoring Design -- Initial"
    outname <- paste0(proj_name, "_initialdesignmap")
  } else {
    title <- "Monitoring Design -- Final"
    outname <- paste0(proj_name, "_finaldesignmap")
  }
  
  proj_name <- substr(sdesign$name[1], 1, 10)
  
  #Define the center of the project extent for calling the basemap (this works out better than using st_centroid()!)
  bb <- st_bbox(border)
  proj.center <- matrix(
    c(mean(c(bb["xmin"], bb["xmax"])), mean(c(bb["ymin"], bb["ymax"]))),
    nrow = 1,
    dimnames = list(NULL, c("X", "Y"))
  )
  
  #calibrating custom zoom levels for the basemap; cutoffs were determined by trial and error -- I think it's dialed in now
  Lat.range <- c(min(as.data.frame((st_coordinates(border)))$Y), max(as.data.frame((st_coordinates(border)))$Y))
  Lon.range <- c(min(as.data.frame((st_coordinates(border)))$X), max(as.data.frame((st_coordinates(border)))$X))
  width <- max(Lon.range)-min(Lon.range)
  length <- max(Lat.range)-min(Lat.range)
  square.size <- max(width, length)
  pract_zoom <- if (square.size <= 0.003) {17
  } else if (0.003 < square.size & square.size <= 0.005) {17
  } else if (0.005 < square.size & square.size <= 0.01) {16
  } else if (0.01 < square.size & square.size <= 0.03) {15
  } else if (0.03 < square.size & square.size <= 0.05) {14
  } else {13}
  
  
  register_google(key = key) #will populate with defined Google API key
  plot.base <- get_googlemap(center= proj.center, zoom = pract_zoom, maptype = 'satellite') #calls basemap from google database
  sdesign <- sdesign %>%
    mutate(pointtype = ifelse(grepl(".OS", name, fixed = TRUE), "oversample", "base"))
  labels<- str_sub(sdesign$name, start = nchar(sdesign$name) - 1, end = nchar(sdesign$name))
  sdesign$labels<-labels
  
  #defining more exact map boundaries for when the zoom level cutoffs aren't quite right; some trial and error in those adjusting coefficients
  #Not in use when coefficients = 1
  lat.lim.adjusted<- c(proj.center[2]-(square.size/2)*1.0, proj.center[2]+(square.size/2)*1.0)
  lon.lim.adjusted<- c(proj.center[1]-(square.size/2)*1.0, proj.center[1]+(square.size/2)*1.0)
  
  pdf(paste0(dir, "/", outname, ".pdf"), paper="letter", width=7.5, height=10) #calling in pdf command to start the plot
  
  if (length(unique(border[[plot_type_col]]))>1){
    print(
      map<- ggmap(plot.base, extent = 'device')+
        geom_sf(data=border, aes(color = .data[[plot_type_col]]), fill=NA, linewidth = .8, inherit.aes = FALSE) + 
        scale_color_manual(name='Plot Type', values=c("C"="white","T"="red"), labels=c("C"='Control', "T"='Treatment'))+
        geom_sf_label(data=sdesign, aes(label=labels, fill=pointtype), fontface = "bold", label.padding = unit(.1, "lines"), size = 3, inherit.aes = FALSE)+
        scale_fill_manual(name='Sampling Locations', values = c('yellow','white'), labels=c('Base Point', 'Oversample'))+
        theme(legend.position='bottom', 
              legend.box = "vertical", 
              legend.background = element_rect(fill = "#f5f5f2", color = 'black'),
              plot.title = element_text(hjust = 0.5))+
        annotation_scale(unit_category='imperial', location = 'tl')+
        annotation_north_arrow(location = "br", height = unit(0.3, "in"), width = unit(0.3, "in"), pad_y = unit(0.2, 'in'), style=north_arrow_orienteering())+
        ggtitle(paste0(proj_name, " ", title), subtitle=subtitle)+
        scale_x_continuous(limits = lon.lim.adjusted)+ #these parameters help with the default zoom level is too far out
        scale_y_continuous(limits = lat.lim.adjusted)
    )
    
    
  } else {
    print(
      map<- ggmap(plot.base, extent = 'device')+
        geom_sf(data=border, color = "white", fill=NA, linewidth = .8, inherit.aes = FALSE) + 
        geom_sf_label(data=sdesign, aes(label=labels, fill=pointtype), fontface = "bold", label.padding = unit(.1, "lines"), size = 3, inherit.aes = FALSE)+
        scale_fill_manual(name='Sampling Locations', values = c('yellow','white'), labels=c('Base Point', 'Oversample'))+
        theme(legend.position='bottom', 
              legend.box = "vertical", 
              legend.background = element_rect(fill = "#f5f5f2", color = 'black'),
              plot.title = element_text(hjust = 0.5))+
        annotation_scale(unit_category='imperial', location = 'tl')+
        annotation_north_arrow(location = "br", height = unit(0.3, "in"), width = unit(0.3, "in"), pad_y = unit(0.2, 'in'), style=north_arrow_orienteering())+
        ggtitle(paste0(proj_name, " ", title), subtitle=subtitle)
    )
  }
  
  dev.off() #creates the file
  
}

## ---- Finalize sampling design ----
Finalize_Spatial <- function(initialdesign, nochange=FALSE, sharepoint_path=NA, rejected=NA, replaced=NA){
  #This function finalizes a sampling design by deleting unused oversample points and replacing rejected basepoints
  #It then saves the final design to the Z drive as a zipped shapefile. 
  
  if (!require(sf)) {
    stop("Package 'sf' is required for this function.")
  }
  
  if (!require(readxl)&!is.na(sharepoint_path)) {
    stop("Package 'readxl' is required for this function.")
  }
  
  #Simplest case: no oversamples were used
  if (nochange==TRUE){
  bprej<-c()
  osused<-c()
  osunused <- initialdesign[grepl(".OS\\d{2}", initialdesign$name), ]$name
  }
  
  #Establish the project id from the initial design
  proj_id<-substr(first(initialdesign$name), 1, 10)
  
  #Establish point names in different categories
    #When reading in data from sharepoint
    if(is.na(rejected)&is.na(replaced)&!is.na(sharepoint_path)){
    
      #Read in AgCDataEntry.xlsx
      tap_data<-read_excel(sharepoint_path, sheet = "Soils")    
    
      #Filter by project and which points need to be modified
      df.rejected<- tap_data %>%  
        filter(
          ProjectID == proj_id,
          Rejected == "y")
      
      #categorize
      bprej<-df.rejected$PointID
      osused<-paste0(substr(df.rejected$PointID, 1, 13), df.rejected$Replaced.with)
      osunused <- setdiff(initialdesign[grepl(".OS\\d{2}", initialdesign$name), ]$name, osused)
    }
    
    #When entering manually
    if(!is.na(rejected)&!is.na(replaced)&is.na(sharepoint_path)){
    bprej<-paste0(proj_id, ".", rejected)
    osused<-paste0(proj_id, ".", replaced)
    osunused<-setdiff(initialdesign[grepl(".OS\\d{2}", initialdesign$name), ]$name, osused)
    }
  
  #Remove unused oversample points
  finaldesign <- initialdesign[!initialdesign$name %in% osunused, ]
  
  #Remove rejected base points
  finaldesign <- finaldesign[!finaldesign$name %in% bprej, ]
  
  #Rename used oversample points
  rename_mapping <-setNames(bprej, osused)
  finaldesign$name <- ifelse(finaldesign$name %in% names(rename_mapping),
                                      rename_mapping[finaldesign$name],
                             finaldesign$name)
  message("Success. Make sure final shapefiles are saved in the Z drive. You can use write_zipped_shp()")
  
  return(finaldesign)

}
