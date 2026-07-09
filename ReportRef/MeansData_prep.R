#Prepping project means for LandSteward Reports

#Starting the process for converting ppm to lbs/acre for relevant indicators
if ('lbs/acre' %in% proj.indicator.table$UnitsTable && "BD" %in% proj.indicator.table$Acronym){
  
  #identify indicators that need to be converted
  to_convert<-proj.indicator.table$CheckCol[
    !is.na(proj.indicator.table$UnitsTable) &
      proj.indicator.table$UnitsTable == "lbs/acre"
  ]  

  #dynamically create the new column names needed
  cols_lbs<-sub("_ppm$", "_lbs.acre", to_convert)
  
  #add them as empty columns to PointLevel
  PointLevel[cols_lbs] <- NA
  
  #populate values based on conversion
  
  PointLevel[cols_lbs] <- PointLevel[to_convert] * PointLevel$bulk_density * (PointLevel$e_depth-PointLevel$b_depth) * 0.224
  
  
  }


#First, if stocks are included
if ("bulk_density" %in% proj.indicators.SSC && params$stocks){
  Project.Means <- PointLevel %>%
    dplyr::group_by(plot_type, timepoint) %>%
    dplyr::summarise(across(all_of(proj.indicators.SSC.stocks), \(x) mean(x, na.rm = TRUE))) #find the mean for each indicator grouped by plot
  
  #pivot so that indicators are row names and plot type and depth increment combinations are column names
  Means.Pivot1<- Project.Means %>% pivot_longer(cols=all_of(proj.indicators.SSC.stocks), names_to ='Indicator') %>% arrange(timepoint)%>%
    pivot_wider(
      names_from = c("timepoint", "plot_type"),   # timepoint first, then plot_type
      values_from = value,
      names_sep = " | "                           #separator
    ) %>%
    left_join( #add the units column (CHECK,)
      proj.indicator.table %>% select(CheckCol, UnitsTable),
      by = c("Indicator" = "CheckCol")
    ) %>%
    mutate(
      Units = case_when(
        Indicator %in% c("sand", "silt", "clay") ~ "%",           #tx components will always be %
        str_ends(Indicator, "_stocks") ~ "tons/acre",             #stocks inds will always be tons/acre
        TRUE ~ UnitsTable                                         #the rest will be defined by the table
      )
    ) %>%
    select(-UnitsTable)%>% 
    mutate(
      # remove _stocks for matching if present
      Indicator_base = str_remove(Indicator, "_stocks$")
    ) %>%
    left_join(
      proj.indicator.table %>% select(Acronym, CheckCol),
      by = c("Indicator_base" = "CheckCol")
    ) %>%
    mutate(
      Acronym = case_when(
        Indicator_base == "sand" ~ "Sand",
        Indicator_base == "silt" ~ "Silt",
        Indicator_base == "clay" ~ "Clay",
        TRUE ~ Acronym                                # default mapping
      )
    )
}

#Next, if stocks are not included
if (!params$stocks){
  Project.Means <- PointLevels_joined %>%
    dplyr::group_by(plot_type, timepoint) %>%
    dplyr::summarise(across(all_of(proj.indicators.SSC), \(x) mean(x, na.rm = TRUE))) #find the mean for each indicator grouped by plot
  
  #pivot so that indicators are row names and plot type and depth increment combinations are column names
  Means.Pivot1<- Project.Means %>% pivot_longer(cols=all_of(proj.indicators.SSC), names_to ='Indicator') %>% arrange(timepoint)%>%
    pivot_wider(
      names_from = c("timepoint", "plot_type"),   # timepoint first, then plot_type
      values_from = value,
      names_sep = " | "                           #separator
    ) %>%
    left_join( #add the units column (CHECK,)
      proj.indicator.table %>% select(CheckCol, UnitsTable),
      by = c("Indicator" = "CheckCol")
    ) %>%
    mutate(
      Units = case_when(
        Indicator %in% c("sand", "silt", "clay") ~ "%",           #tx components will always be %
        TRUE ~ UnitsTable                                         #the rest will be defined by the table
      )
    ) %>%
    select(-UnitsTable)%>% 
    mutate(
      # remove _stocks for matching if present
      Indicator_base = str_remove(Indicator, "_stocks$")
    ) %>%
    left_join(
      proj.indicator.table %>% select(Acronym, CheckCol),
      by = c("Indicator_base" = "CheckCol")
    ) %>%
    mutate(
      Acronym = case_when(
        Indicator_base == "sand" ~ "Sand",
        Indicator_base == "silt" ~ "Silt",
        Indicator_base == "clay" ~ "Clay",
        TRUE ~ Acronym                                # default mapping
      )
    )
}
