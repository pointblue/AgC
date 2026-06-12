#Ag-C Reports Auxilary Data Prep
sharepoint_path<-"C:/Users/acook-SEA/OneDrive - Point Blue"


# ---- Aux Data With Depth
  ## ---- PLFA ----
    ### ---- Data check ----
    #Does PLFA data exist for this project?
    data_path_bio<-"Z:/Soils Team/AgC Data/Raw Data/Lab Data/Biological"
    matching_files <- c() # Vector to store matches
    list_dfs<-list.files(path = data_path_bio,pattern = "^Ward_biological.*\\.csv$",full.names = TRUE) #list all ward biological csvs
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
    
    ### ---- Read and clean ----
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
    
    
  
  ## ---- WHC ----
    ### ---- Data check ----
    #Does WHC data exist for this project?
    file<-file.path(sharepoint_path, "PointBlue Programs - Shared Soils Program/Ag-C/Internal Ag-C Projects", "AgC_Auxilliary_DataEntry.xlsx")
    WHC_Slakes_df<-read_excel(file, sheet="WHC_Slakes", col_names=TRUE, na = c("NA", "na", "ND", "nd", "-", "--","", " "))%>%
      filter(ProjectID == params$project_name)%>%
      mutate(across(c(BdepthTarget_cm:Slakes_index), as.numeric))%>%mutate(target_depth=paste0(BdepthTarget_cm, "-", EdepthTarget_cm))
    
    ### ---- Read and clean ----
    #If so, pull it in, clean, and attach to PointLevel
    if (nrow(WHC_df)>0) {
      WHC_df<-WHC_Slakes_df %>%
        mutate(
          WHC = (WHC_VolAdded_mL - WHC_VolCollected_mL) / WHC_mass_g * 100 #do i need to convert to volumetric?
        ) %>%
        select(PointID, Timepoint, target_depth, WHC)
      
      
      PointLevel <- PointLevel %>%
        left_join(
          WHC_df, by = c(
            "sample_id" = "PointID",
            "timepoint" = "Timepoint",
            "target_depth" = "target_depth"
          )
        )
       }
    
    
  ## ---- Slakes ----
    # already read in the relevant dataframe, if present attach to PointLevel
    if(!is.na(any(WHC_Slakes_df$Slakes_index))){
      Slakes_df<-WHC_Slakes_df %>%select(PointID, Timepoint, target_depth, Slakes_index)
      PointLevel <- PointLevel %>%
        left_join(
          Slakes_df, by = c(
            "sample_id" = "PointID",
            "timepoint" = "Timepoint",
            "target_depth" = "target_depth"
          )
        )
    }

    
  
# ---- Aux Data with No Depth ----
#Use this section to create a dataframe for non-agc point level data that is not associated with a soil depth
    
#Setting up base structure for PointLevel_nd
PointLevel_nd<-data.frame(
  "sample_id" = character(),
  "timepoint" = character()
)
    
  ## ---- Saturated hydraulic conductivity (Data from AgC_Auxilliary_Data) ----
    ### ---- Data check ----  
    file<-file.path(sharepoint_path, "PointBlue Programs - Shared Soils Program/Ag-C/Internal Ag-C Projects", "AgC_Auxilliary_DataEntry.xlsx")
    DRI_df<-read_excel(file, sheet="DRI", col_names=TRUE, na = c("NA", "na", "ND", "nd", "-", "--","", " "))%>%
      filter(ProjectID == params$project_name)
    
    ### ---- perform calcs ----
  
    if (nrow(DRI_df)>0){
      
      #define functions for calculating Ksat
      find_flat_tail <- function(time, y, min_window = 2) {
        
        n <- length(y)
        
        best_start <- n - min_window + 1
        best_score <- Inf
        
        for (start in 1:(n - min_window + 1)) {
          
          idx <- start:n
          
          x <- time[idx]
          yy <- y[idx]
          
          # remove NA pairs safely
          ok <- complete.cases(x, yy)
          x <- x[ok]
          yy <- yy[ok]
          
          if (length(yy) < min_window) next
          if (length(unique(x)) < 2) next  # avoid singular lm
          
          slope <- tryCatch(
            coef(lm(yy ~ x))[2],
            error = function(e) NA_real_
          )
          
          # skip invalid slopes
          if (is.na(slope)) next
          
          score <- abs(slope)
          
          if (score < best_score) {
            best_score <- score
            best_start <- start
          }
        }
        
        best_start:n
      }
      
      calc_Ksat <- function(time, rate, roc) {
        
        rate <- na.omit(rate)
        roc  <- na.omit(roc)
        
        n <- length(rate)
        
        if (n == 0) return(NA_real_)
        
        final_rate <- rate[n]
        final_roc  <- roc[length(roc)]
        
        # 1. perfect stabilization
        if (!is.na(final_roc) && final_roc == 0) {
          return(final_rate)
        }
        
        # 2. all negative ROC
        if (all(roc < 0, na.rm = TRUE)) {
          return(final_rate)
        }
        
        # 3. flat tail method
        idx <- find_flat_tail(time, rate)
        
        return(mean(rate[idx], na.rm = TRUE))
      }
      
      DRI_df<-DRI_df%>%
        select(PointID, Timepoint, `StartTime_hr:mn`, `IntervalTime_hr:min`, RefillVolStart_mL, RefillVolEnd_mL)%>% 
        #alter so its joinable with PointLevel_nd
        rename("sample_id"=PointID, "timepoint"=Timepoint)%>%
        #Perform calcs
        mutate(
          `StartTime_hr:mn` = as.numeric(`StartTime_hr:mn`),
          `IntervalTime_hr:min`= as.numeric(`IntervalTime_hr:min`),
          RefillVolStart_mL = as.numeric(RefillVolStart_mL),
          RefillVolEnd_mL = as.numeric(RefillVolEnd_mL),
        )%>%
        group_by(sample_id, timepoint) %>%
        arrange(`IntervalTime_hr:min`, .by_group = TRUE) %>%
        mutate(IntLen_min = 
                 (`IntervalTime_hr:min` - coalesce(lag(`IntervalTime_hr:min`), `StartTime_hr:mn`)) * 1440,
               VolAdded_mL =  RefillVolStart_mL - RefillVolEnd_mL,
               InfilRate_cm.hr = round(VolAdded_mL / 176.7 / IntLen_min * 60, 1),
               TimeSinceFirst_min = (`IntervalTime_hr:min` - first(`IntervalTime_hr:min`)) *1440,
               TimeSinceStart_min = (`IntervalTime_hr:min` - `StartTime_hr:mn`) *1440,
               roc = round(((InfilRate_cm.hr - lag(InfilRate_cm.hr)) / lag(InfilRate_cm.hr))*100, 1)
        )%>%
        arrange(TimeSinceStart_min, .by_group = TRUE) %>%
        summarise(
          Ksat = calc_Ksat(
            time = TimeSinceStart_min,
            rate = InfilRate_cm.hr,
            roc  = roc
          ),
          .groups = "drop"
        )
      
      
      PointLevel_nd<-PointLevel_nd%>%full_join(DRI_df, by=c("sample_id", "timepoint"))
      
    }
    
  ## ---- Bareground ----
    ### ---- Data check ----  
    file<-file.path(sharepoint_path, "PointBlue Programs - Shared Soils Program/Ag-C/Internal Ag-C Projects", "AgCDataEntry.xlsx")
    bareground_df<-read_excel(file, sheet="Soils", col_names=TRUE, na = c("NA", "na", "ND", "nd", "-", "--","", " "))%>%
      filter(ProjectID == params$project_name)%>%
      select(PointID, Timepoint, Bareground_prop)%>%
      mutate(as.numeric(Bareground_prop))

    ### ---- Join ----
    if(!any(is.na(bareground_df$Bareground_prop))){
      PointLevel_nd<-PointLevel_nd%>%left_join(bareground_df, by=c("sample_id"="PointID", "timepoint"="Timepoint"))
    }
    
    
  ## ---- VegVigor ----
    
  ## ---- Ground cover ----
    
  
