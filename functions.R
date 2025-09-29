# Title: functions.R
# Author: Lisa Eash
# Date created: 20250402
# Date updated: 20250924
# Purpose: Define Ag-C data cleaning functions used in AgCDataCompile.R to:
#   1) clean_lab_df: Standardizes columns and units from incoming lab data (Ward, Cquester)
#   2) clean_tap_df: Renames columns and removes extra columns in TAP df
#   3) coord_extract: Extracts coordinates for sampling points from projects of interest
#   4) out_of_range: verify required columns and check for data outside expected ranges
#   5) proj_design: Extracts project design data required for inference score calculation from point level db
#   6) reg_baseline: Associates regional soil carbon baselines (mean and 95% conf. interval) with field polygon for producer reports

## ---- clean_lab_df function ----

clean_lab_df <- function(data_path, #main data directory (Z:/Soils Program/AgC Data)
                         lab, #as of now, can be "Cquester" or "Ward"
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
  lab_raw<-read.csv(df_name)

  # Rename columns
  col_map <- read.csv("lab_column_names.csv")
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
    select(SampleID)
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
  ecoregions <- st_transform(ecoregions, st_crs(cent))
  region <- ecoregions[st_contains(ecoregions, cent, sparse = FALSE), ]
  
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
  RACA_res <- data.frame(mean = RACA_mean, ci95 = RACA_ci, n = RACA_n)
  return(RACA_res)
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
#Reports will write to
#Z:\Soils Team\AgC Data\RenderedReports
render_one_html <- function(project) {
  rmarkdown::render(
    input = 'LandStewardReports.Rmd', #identify the markdown file that will be used to render the report
    output_file = paste0(project, '_Report_', Sys.Date(), '.html'), #ID the file path and naming pattern
    output_dir = "Z:/Soils Team/AgC Data/RenderedReports",
    params = list(project_name = project), #"project_name" references the name of the parameter in the YAML header; "project" represents the current throughput of the loop
    envir = parent.frame()
  )
}
#layering that so you can pass a vector of project names to render many reports at once
render_html_report <- function(projects){ #when projects is a project name or a vector of project names
  for(project in projects){
    render_one_html(project)
  }
}