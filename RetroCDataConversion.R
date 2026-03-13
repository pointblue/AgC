# Title: RetroCDataConversion.R
# Author: Lisa Eash
# Date created: 20260313
# Date updated: 20260313
# Purpose: Script for converting retroc data to pointlevel database


# Import current master database
master_df_list <- list.files(paste(data_dir,"Master Datasheets","PointLevel", sep="/"), pattern = "\\.csv$", full.names = TRUE) #list all the CSVs in folder
df_current <- read.csv(master_df_list[which.max(as.Date(gsub("\\D","", master_df_list), format = "%Y%m%d"))]) #this indexing patterns makes sure we're using the most recent master datasheet
str(df_current    )

CSUdata<-read.csv(paste(data_dir,"Raw Data","Lab Data","CSU_data_20240916.csv",sep="/"))
head(CSUdata     )

#proj_tracker_file_path <- USER_DEFINED

proj_name_map <- read_excel(
  proj_tracker_file_path
) %>%
  filter(FundingEffort == "RetroC") %>%
  select(c(OldProjectCode,ProjectCode))

map_df <- proj_name_map %>%
  mutate(
    OldProjectCode = str_trim(as.character(OldProjectCode)),
    ProjectCode = str_trim(as.character(ProjectCode))
  )

df_csu <- CSUdata %>%
  mutate(
    ranch.pract = str_trim(as.character(ranch.pract)),
    
    # split ranch.pract into old project code + treatment
    OldProjectCode = str_sub(ranch.pract, 1, -2),
    trt = str_sub(ranch.pract, -1, -1),
    
    PointNumber = str_pad(as.character(PointNumber), width = 2, pad = "0")
  ) %>%
  left_join(map_df, by = "OldProjectCode") %>%
  mutate(
    project_id = ProjectCode,
    sample_id = paste(project_id, trt, PointNumber, sep = "."),
    
    sample_date = format(
      as.POSIXct(as.Date(as.character(SamplingDate), format = "%Y%m%d"), tz = "UTC") + 12 * 60 * 60,
      "%Y-%m-%d %H:%M:%S"
    ),
    
    target_depth = "0-30",
    b_depth = 0,
    e_depth = 30,
    b_depth_mic_c = NA,
    e_depth_mic_c = NA,
    b_depth_plfa = NA,
    e_depth_plfa = NA,
    position = NA,
    lab_name = "CSU",
    c_method = "Dry Combustion",
    ph_method = "lab",
    ph = pH,
    org_c = tsoc,
    pom = per_POM,
    maom = per_MAOM,
    sand = per_sand,
    silt = per_silt,
    clay = per_clay,
    total_n = t_n,
    bulk_density = BD_g.cm3
  ) %>%
  select(
    project_id, sample_id, sample_date, lat, long,
    target_depth, b_depth, e_depth,
    b_depth_mic_c, e_depth_mic_c,
    b_depth_plfa, e_depth_plfa,
    position, lab_name, c_method,
    total_n, org_c, pom, maom,
    sand, silt, clay,
    ph_method, ph, bulk_density
  )

# optional: order columns exactly like df_current where possible
df_csu <- df_csu %>%
  select(any_of(names(df_current)))

df_csu


missing_cols <- setdiff(names(df_current), names(df_csu))

df_csu[missing_cols] <- NA

# Remove any extra columns not in df_current (optional but safer)
df_csu <- df_csu[, names(df_current)]
df_csu$timepoint<-"T1"

# Now rbind works
df_all <- bind_rows(df_current, df_csu)
