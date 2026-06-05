#Ag-C Reports Auxilary Data Prep

sharepoint_path<-"C:/Users/acook-SEA/OneDrive - Point Blue"

# ---- PLFA ----

  ## ---- Data check ----
  #Does PLFA data exist for this project?
  data_path<-"Z:/Soils Team/AgC Data/Raw Data/Lab Data/Biological"
  matching_files <- c() # Vector to store matches
  list_dfs<-list.files(path = data_path,pattern = "^Ward_biological.*\\.csv$",full.names = TRUE) #list all ward biological csvs
  for (fw in list_dfs) {
    df <- read.csv(fw)
    
    #detect any column name including "sample" and "id"
    matching_columns <- names(df)[
      grepl("sample", names(df), ignore.case = TRUE) &
        grepl("id", names(df), ignore.case = TRUE)
    ]
    
    #check each of those columns for a project ID match substring
    if (
      length(matching_columns) > 0 &&
      any(sapply(params$project_name, function(p) {
        any(sapply(matching_columns, function(col) {
          any(grepl(p, as.character(df[[col]]), fixed = TRUE))
        }))
      }))
    ) {
      matching_files <- c(matching_files, fw)
    }
  }
  
  ## ---- Read and clean ----
  #If so, pull it in, clean, and attach to PointLevel
  if (length(matching_files>0)){
    PLFA_raw<-read.csv(matching_files)
  
  #identify sample_id column and rename to match PointLevel
    ref_ids <- unique(as.character(PointLevel$sample_id))
    match_counts <- sapply(PLFA_raw, function(x) {
      sum(as.character(x) %in% ref_ids, na.rm = TRUE)
    })
    sample_id_col <- names(match_counts)[which.max(match_counts)]
    if (max(match_counts) > 0) {
      names(PLFA_raw)[names(PLFA_raw) == sample_id_col] <- "sample_id"
    } else {
      stop("Could not identify a sample_id column.")
    }
    
  #join relevant columns with PointLevel
    PointLevel<-PointLevel%>%
      left_join(PLFA_raw%>%
                  select(sample_id, `Total.Living.Microbial.Biomass.ng.g`, `Fungi.Bacteria.ng.g`, `Gram....Gram....ng.g`, 
                         `Predator.Prey.ng.g`, `Actinomycetes.ng.g...Biomass`, `Protozoan.ng.g...Biomass`, `Arbuscular.Mycorrhizal.ng.g`, 
                         `Saprophytes.ng.g`),
                  by="sample_id"
                )
    
    
    }
    
  
# ---- WHC ----
  ## ---- Data check ----
  #Does WHC data exist for this project?
  file<-file.path(sharepoint_path, "PointBlue Programs - Shared Soils Program/Ag-C/Internal Ag-C Projects", "AgC_Auxilliary_DataEntry.xlsx")
  WHC_df<-read_excel(file, sheet="WHC_Slakes", col_names=TRUE, na = c("NA", "na", "ND", "nd", "-", "--","", " "))%>%
    filter(ProjectID == params$project_name)
  
  ## ---- Read and clean
  #If so, pull it in, clean, and attach to PointLevel
  if (nrow(WHC_df>0)) {
    PointLevel<- PointLevel %>% left_join(
      WHC_df%>%mutate(
        WHC_VolAdded_mL = as.numeric(WHC_VolAdded_mL),
        WHC_VolCollected_mL= as.numeric(WHC_VolCollected_mL),
        WHC_mass_g = as.numeric(WHC_mass_g),
        WCH=(WHC_VolAdded_mL-WHC_VolCollected_mL)/WHC_mass_g*100)%>%
        select(PointID, Timepoint, BdepthTarget_cm, EdepthTarget_cm, WCH) #convert from a proportion to a percentage
      )
    ) #finish the join by sample_id, timepoint, and depth
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
