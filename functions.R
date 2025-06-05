# Title: functions.R
# Author: Lisa Eash
# Date created: 20250402
# Date updated: 20250604
# Purpose: Define Ag-C data cleaning functions used in AgCDataCompile.R to:
#   1) clean_lab_df: Standardizes columns and units from incoming lab data (Ward, Cquester)
#   2) clean_tap_df: Renames columns and removes extra columns in TAP df
#   3) out_of_range: verify required columns and check for data outside expected ranges

## ---- clean_lab_df function ----

clean_lab_df <- function(data_path, #main data directory (Z:/Soils Team/AgC Data)
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
  col_map <- read.csv("Lab Column Names.csv")
  if(lab == "Cquester"){
    rename_vec <- setNames(as.character(col_map$Cquester), col_map$Column.Name)
    rename_vec <- rename_vec[rename_vec %in% colnames(lab_raw)]
    lab_clean <- lab_raw %>%
      slice(-1) %>%
      rename(!!!rename_vec)
  }
  if(lab == "Ward"){
    rename_vec <- setNames(as.character(col_map$Ward), col_map$Column.Name)
    rename_vec <- rename_vec[rename_vec %in% colnames(lab_raw)]
    lab_clean <- lab_raw %>%
      select(where(~ !all(is.na(.)))) %>%
      select(match("Sample ID", names(lab_raw)):last_col()) %>%
      rename(!!!rename_vec)
  }
  
  # Make sure columns are numeric, fill in NAs where blank
  lab_clean <- lab_clean %>%
    mutate(across(everything(), ~ ifelse(. %in% c("-","--",""," ","NA","na"), NA,.))) %>%
    mutate(across(-c(sample_id), as.numeric))
  
  # Check for no unexpected columns in lab raw data
  if(length(rename_vec) != ncol(lab_clean)) {
    message("Extra columns in lab df: ", paste(colnames(lab_clean)[!colnames(lab_clean) %in% names(rename_vec)]))
  }
  
  # Define C analysis method based on lab
  lab_clean$c_method <- ifelse(lab %in% c("Cquester","Ward"),"Dry Combustion",NA)
  
  #Add required columns that were not in raw lab data 
  cols_to_add <- col_map$Column.Name[!col_map$Column.Name %in% names(lab_clean)]
  lab_clean[,cols_to_add] <- NA
  lab_clean <- lab_clean %>%
    select(col_map$Column.Name)
  
  return(lab_clean)
}

## ---- clean_tap_df function ----

clean_tap_df <- function(tap_df){
  
  #Remove extra columns, define column types
  tap_clean <- tap_df %>%
    slice(-c(1,2)) %>% # remove example and unit rows
    select(Sample.ID, Sampling.date:BD.Depth4,Field.notes) %>%
    mutate(Sampling.date = format(as.Date(as.numeric(Sampling.date), origin = "1899-12-30"), "%Y-%m-%d")) %>%
    mutate(across(everything(), ~ ifelse(. %in% c("-","--",""," ","NA","na"), NA,.))) %>%
    filter(!is.null(Sample.ID))%>% #filter out empty rows
    filter(!is.na(Sample.ID))%>% #filter out empty rows
    mutate(across(c(BD.Vol1:BD.Depth4), as.numeric))
  
  #Determine bulk density method  
  tap_clean <- tap_clean %>%
    mutate(bd_method = case_when( 
      !is.na(BD.Vol1) | !is.na(BD.Vol2) | !is.na(BD.Vol3) | !is.na(BD.Vol4) ~ "Millet",
      !is.na(BD.Depth1) | !is.na(BD.Depth2) | !is.na(BD.Depth3) | !is.na(BD.Depth4) ~ "Core Depth",
        TRUE ~ NA_character_  
    ))
  
  #Calculate volume 
  tap_clean <- tap_clean %>%
    mutate(Volume_cm3 = case_when( #calculate sample volumes based on all possible scenarios
    !is.na(BD.Vol3) & !is.na(BD.Vol4) ~ (BD.Vol3 + BD.Vol4) / 2,  # Case 1: vol3 and vol4 have values (first two measurements were off)
    !is.na(BD.Vol1) & !is.na(BD.Vol2) & is.na(BD.Vol3) & is.na(BD.Vol4) ~ (BD.Vol1 + BD.Vol2) / 2,  # Case 2: vol1 and vol2 are present, none for 3 and 4 (first two measurements were good)
    is.na(BD.Vol1) & is.na(BD.Vol2) & is.na(BD.Vol3) & is.na(BD.Vol4) & 
      ( !is.na(BD.Depth1) | !is.na(BD.Depth2) | !is.na(BD.Depth3) | !is.na(BD.Depth4) ) ~ rowMeans(select(., BD.Depth1, BD.Depth2, BD.Depth3, BD.Depth4), na.rm = TRUE) * pi * ((2*2.54) / 2)^2, #Case 3; No volume measurements are present -- depths must be used to calculate sample volume
    TRUE ~ NA_real_  # no volumes or depths are present; bulk density is not included
  ))
  
  #Make sure that all rows that have NA for volume have no VOl or Depth data
  na_rows <- tap_clean[is.na(tap_clean$Volume_cm3),] %>%
    filter(if_all(c(BD.Vol1:BD.Depth4), ~ !is.na(.))) %>%
    select(Sample.ID)
  if(nrow(na_rows) > 0) {
    message("Sample IDs with vol/depth input but no calculated volume:", paste(na_rows$Sample.ID))
  }
  
  #Select rows
  tap_clean <- tap_clean %>%
    rename(sample_id = Sample.ID,
           sample_date = Sampling.date,
           vol_cm3 = Volume_cm3,
           notes = Field.notes) %>%
    as.data.frame()
    
  return(tap_clean)
}

## ---- QAQC function ----

out_of_range <- function(df, var, min, max){
  df %>% 
    filter(.data[[var]] < min | .data[[var]] > max | is.na(.data[[var]]))
}



