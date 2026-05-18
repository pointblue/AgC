#Prepping the results summary table for LandStewardReports

#First for stocks
if("bulk_density" %in% proj.indicators.SSC && params$stocks){
my_order <- c("SOC", "SIC", "MAOC", "POC", "BD", "pH", "Sand", "Silt", "Clay") #CHECK add plant metrics when present

Means.Pivot <- Means.Pivot1 %>%
  filter(Indicator != "all_stocks")%>% #remove all stocks. dont need it in this table
  select(-Indicator_base, -Indicator) %>%
  select(Acronym, everything(), Units) %>%
  mutate(
    order_helper = match(Acronym, my_order),
    original_order = row_number()
  ) %>%
  arrange(is.na(order_helper), order_helper, original_order) %>%
  select(-order_helper, -original_order)%>%mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))

# setup dynamic headers
timepoints <- unique(Project.Means$timepoint)
plot_types <- unique(Project.Means$plot_type)

# Build expected column names to match order (timepoint | plot_type)
value_columns <- names(Means.Pivot1)[!names(Means.Pivot1) %in% c("Indicator", "Indicator_base", "Units", "Acronym")]

# Reorder columns
Means.Pivot <- Means.Pivot %>%
  select(Acronym, all_of(value_columns), Units)

# Lower header — show plot types only
flat_colnames <- c("Indicator", rep(plot_types, times = length(timepoints)), "Unit")

# Upper header (timepoints)
header_above <- c(" " = 1)  # blank for Indicator
for (tp in timepoints) {
  tp_label<-filter(tp_lookup, timepoint==tp)$year_label
  header_above <- c(header_above, setNames(length(plot_types), tp_label))
}
header_above <- c(header_above, " " = 1)  # blank for Unit

# --- create table ---
SummaryTable <- Means.Pivot %>%
  kbl(
    caption = "Plot means for all measured values",
    digits = 2,
    col.names = flat_colnames
  ) %>%
  add_header_above(header_above) %>%
  kable_styling(position = "center", full_width = FALSE)
}

#Then for non-stocks
if (!params$stocks){
  my_order <- c("SOC", "SIC", "MAOC", "POC", "BD", "pH", "Sand", "Silt", "Clay") #CHECK add plant metrics when present
  
  Means.Pivot <- Means.Pivot1 %>%
    filter(Indicator != "all_stocks")%>% #remove all stocks. dont need it in this table
    select(-Indicator_base, -Indicator) %>%
    select(Acronym, everything(), Units) %>%
    mutate(
      order_helper = match(Acronym, my_order),
      original_order = row_number()
    ) %>%
    arrange(is.na(order_helper), order_helper, original_order) %>%
    select(-order_helper, -original_order)%>%mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
  
  # setup dynamic headers
  timepoints <- unique(Project.Means$timepoint)
  plot_types <- unique(Project.Means$plot_type)
  
  # Build expected column names to match order (timepoint | plot_type)
  value_columns <- names(Means.Pivot1)[!names(Means.Pivot1) %in% c("Indicator", "Indicator_base", "Units", "Acronym")]
  
  # Reorder columns
  Means.Pivot <- Means.Pivot %>%
    select(Acronym, all_of(value_columns), Units)
  
  # Lower header — show plot types only
  flat_colnames <- c("Indicator", rep(plot_types, times = length(timepoints)), "Unit")
  
  # Upper header (timepoints)
  timepoints <- tp_lookup %>%
    arrange(year_label) %>%   # ensures smaller year comes first
    pull(timepoint)
  header_above <- c(" " = 1)  # blank for Indicator
  for (tp in timepoints) {
    tp_label<-filter(tp_lookup, timepoint==tp)$year_label
    header_above <- c(header_above, setNames(length(plot_types), tp_label))
  }
  header_above <- c(header_above, " " = 1)  # blank for Unit
  
  #CHECK! Catie asked me to remove total N as it will be confused with nitrates for producers:
  Means.Pivot<-Means.Pivot%>%filter(Acronym!="N")
  
  #This step fills in missing columnns with NA. "missing" being special cases where a C plot wasn't monitored at every timepoint
  timepoints <- sort(unique(as.character(Project.Means$timepoint)))
  plot_types <- c("C", "T")  # force C before T
  
  expected <- expand.grid(
    plot_type = plot_types,
    timepoint = timepoints
  )
  
  expected_cols <- paste(
    expected$timepoint,
    expected$plot_type,
    sep = " | "
  )
  expected_cols <- c("Acronym", paste(expected$timepoint, expected$plot_type, sep = " | "), "Units")
  missing_cols <- setdiff(expected_cols, names(Means.Pivot))
  Means.Pivot[missing_cols] <- NA
  Means.Pivot <- Means.Pivot %>%
    select(Acronym, all_of(expected_cols), Units)
  
  # --- create table ---
  SummaryTable <- Means.Pivot %>%
    kbl(
      caption = "Plot means for all measured values",
      digits = 2,
      col.names = flat_colnames
    ) %>%
    add_header_above(header_above) %>%
    kable_styling(position = "center", full_width = FALSE)
}