#Prepping stocks data for LandStewardReports

#Calculating stocks on qualifying soil metrics
if ("bulk_density" %in% proj.indicators.SSC){ #CHECK this condition needs to be different when we get plant projects cause someone could have SOC% only but also have measured plants
  stocks.inds <- c("total_c", "org_c", "inorg_c", "maoc", "poc") #these are the columns that can be turned into stocks
  #CHECK! how best to handle this when we have plant stocks?
  
  #calculating stocks on soil metrics
  for(col in intersect(proj.indicators.SSC, stocks.inds)) {
    new_col <- paste0(col, "_stocks")  # new column name
    PointLevel[[new_col]] <- PointLevel[[col]]*PointLevel$bulk_density*PointLevel$e_depth*0.4462 #calculating stocks in US tons / acre (percent to decimal cancels out) 
  }
  
  proj.indicators.SSC.stocks<- c(proj.indicators.SSC, paste0(intersect(proj.indicators.SSC, stocks.inds), "_stocks")) #add those new column names to my list of indicator columns
  
  if (length(setdiff(intersect(proj.indicators.SSC, stocks.inds), c('poc_stocks', 'maoc_stocks')))>1){ #if the number of stocks indicators in this project (minus poc and maoc) are greater than one, create a summed all stocks column
    
    PointLevel <- PointLevel %>%
      mutate(
        all_stocks = rowSums(
          select(., ends_with("_stocks"), -any_of(c("maoc_stocks", "poc_stocks"))), #create a column for all pools combined, but make sure you're not double-counting poc and maoc, since org_c should cover that
          na.rm = TRUE
        )
      )
    proj.indicators.SSC.stocks<-c(proj.indicators.SSC.stocks, "all_stocks") #CHECK! I may have added this to the wrong vector previously. Need to see how this is used 
  }
  
}

stocks_df <- PointLevel %>% #reformats so the dataframe is long with respect to the carbon stock pool
  pivot_longer(
    cols = contains("_stocks"), # Select columns to pivot
    names_to = "Stocks_Indicator",          # Name for the new column
    values_to = "Tons.Acre"              # values to populate the new column
  ) %>%
  mutate(LU="AgC")%>%
  bind_rows( #bind project stocks df to the raca dataset
    raca_data %>%
      mutate(
        sample_id = rcasiteid,
        Tons.Acre = SOCstock*0.446092,
        plot_type = "raca",
        timepoint = "Reference (2010)",
        Stocks_Indicator = "org_c_stocks",
        LU=LU,
        .keep = "none"   # keep only these renamed columns
      )
  )%>%
  filter(LU %in% c("AgC",   params$raca_filter)) #filter out values that don't match the correct land use type

#SOC stocks graphic comparing to RaCa baselines
SOC_stocks_df<-stocks_df%>%
  filter(Stocks_Indicator=="org_c_stocks", #filter the stocks df for only SOC (including raca)
         LU %in% c("AgC",   params$raca_filter) #this includes raca values from whatever LU types were included in the render function
  ) 

#Define the desired order of x-axis values
SOC_stocks_df$plot_type <- factor(SOC_stocks_df$plot_type, levels = c("T", "C", "raca"))