#Land Steward Report Stats Development Script
#Created by AC on 11/30/2025

library(english)

#Calculate and verbalize raca-mean percentiles for the most recent timepoint in any presnt plot
  #This needs to be flexible to different indicators    
  raca_ecdf <- soc_df %>% 
    filter(plot_type == "raca") %>% 
    pull(org_c) %>% 
    ecdf()    
  
  raca_compare<-Project.Means%>%
    mutate(timepoint_digit = as.numeric(substr(timepoint, 2, 2)))%>%
    filter(timepoint_digit == max(timepoint_digit))%>%
    mutate(raca_percentile = round(raca_ecdf(org_c)*100, digits=0),
           plot_full = case_when(plot_type=="T"~"treatment", plot_type=="C"~"control"),
           text = case_when(
             
             raca_percentile>0 ~ paste0("The most recent mean value in the ", 
                         plot_full, 
                         " plot is in the ", 
                         english::ordinal(raca_percentile), 
                         " percentile of the comparison dataset for your ecoregion."
                         ),
           raca_percentile<=0 ~ paste0("The most recent mean value in the ", 
                                       plot_full, 
                                       " plot is less than any value in the comparison dataset for your ecoregion."
                                       )
            ) )
           
    percentile_scentence<-raca_compare%>%
    pull(text) %>%          # get vector of strings
    paste(collapse = " ")
    percentile_scentence

#One timepoint, treatment only
  #RaCa percentile of most recent mean
  percentile_scentence  
    
#Two or more timepoints, treatment only
  #RaCa percentile of most recent mean
  percentile_scentence

#One timepoint, treatment and control
  #RaCa percentile of most recent mean
  percentile_scentence 


#Two or more timepoints, treatment and control
  #RaCa percentile of most recent mean
  percentile_scentence 
    
    
  library(lme4)
  
  model <- lmer(org_c ~ plot_type * timepoint + (1 | sample_id), data = (soc_df%>%filter(plot_type!="raca")))
  anova(model)
