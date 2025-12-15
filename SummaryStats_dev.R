#Land Steward Report Stats Development Script
#Created by AC on 11/30/2025

library(english)
library(lme4)
library(emmeans)


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

#For all other stats, we need to look at either the only existing timepoint, or the first vs last timepoint
#Filting the dataframe accordingly
#This is only relevant for when we have 3 or more timepoints (we don't), but future-proofing now
  PointLevel_stats<-PointLevel%>%
  mutate(timepoint_digit = as.numeric(substr(timepoint, 2, 2)))%>%
  filter(timepoint_digit %in% c(min(timepoint_digit), max(timepoint_digit)))

#One timepoint, treatment only
  #RaCa percentile of most recent mean
  percentile_scentence
    
#Two or more timepoints, treatment only
  #RaCa percentile of most recent mean
  percentile_scentence
  
  #Time contrast
  model <- lmer(org_c ~ timepoint + (1 | sample_id), data = PointLevel_stats)
  emm <- emmeans(model, ~ timepoint)
  time_change <- contrast(emm,method = "pairwise") %>%
    as.data.frame()%>%
    mutate(
      text = case_when(
        p.value < 0.05 & estimate > 0 ~ "SOC% in the treated plot increased between the first and most recent monitoring timepoint. Though this suggests that the practice is positively affecting soil organic matter and fertility, it may also be a result of changes in climate variation across years.",
        p.value < 0.05 & estimate < 0 ~ "SOC% in the treated plot increased between the first and most recent monitoring timepoint. Without a control field, we are unable to tell whether this is due to changes in management or in climate variation across years.",
        TRUE                          ~ "SOC% did not meaningfully change between the two monitoring timepoints. Possible explanations for a lack of significant change include benefits acrruing more slowly than the monitoring timeline, general SOC% decreases in surrounding areas that didn't recieve conservation, and unexpected impacts of the conservation practice."
      )
    )%>%
    pull(text) %>%          # get vector of strings
    paste(collapse = " ")
  

#One timepoint, treatment and control
  #RaCa percentile of most recent mean
  percentile_scentence 

  #Treatment contrast
  model <- lm(org_c ~ plot_type, data = PointLevel_stats)
  anova(model)
  emm <- emmeans(model, ~ plot_type)
  treatment_contrast<-contrast(emm, method = "pairwise") %>%
    as.data.frame()%>%
    mutate(
      text = case_when(
        p.value < 0.05 & estimate > 0 ~ "At the current timetpoint, SOC% is higher in the treated plot than the control. This difference should be taken into consideration when looking at change in each plot over time.",
        p.value < 0.05 & estimate < 0 ~ "At the current timetpoint, SOC% is lower in the treated plot than the control. This difference should be taken into consideration when looking at change in each plot over time.",
        TRUE ~ "There is no significant difference between SOC% in the treated and control site at this time. This means that the sites are well-matched, making interpretation of future results straightforward."
      )
    )%>%
    pull(text) %>%          # get vector of strings
    paste(collapse = " ")
    


#Two or more timepoints, treatment and control
  #RaCa percentile of most recent mean
  percentile_scentence 
  
  #Plot-wise change
  model <- lmer(
    org_c ~ plot_type * timepoint + (1 | sample_id),
    data = PointLevel_stats
  )
  emm <- emmeans(model, ~ plot_type * timepoint)
  time_contrasts <- contrast(
    emm,
    by = "plot_type",
    method = "pairwise"
  ) %>%
    as.data.frame() %>%
    mutate(
      plot_full = case_when(plot_type=="T"~"treatment", plot_type=="C"~"control"),
      text = case_when(
        p.value < 0.05 & estimate > 0 ~ paste0("SOC% in the ", plot_full, " plot increased between the first and most recent monitoring timepoint."),
        p.value < 0.05 & estimate < 0 ~ paste0("SOC% in the ", plot_full, " plot decreased between the first and most recent monitoring timepoint."),
        TRUE                          ~ paste0("No significant change in percent SOC was detected in the", plot_full, "plot between the first and most recent monitoring timepoint.")
      )
    )%>%
    pull(text) %>%          # get vector of strings
    paste(collapse = " ")
  
  #Treatment effect
  baci <- contrast(emm, interaction = "pairwise") %>%
    as.data.frame() %>%
    mutate(
      text = case_when(
        p.value < 0.05 & estimate > 0 ~
          "SOC% at the treated site increased at a greater rate than the control site, implying a signficiant positive impact of the conservation practice.",
        
        p.value < 0.05 & estimate < 0 ~
          "SOC% at the treated site decreased at a greater rate than the control site, implying a signficiant negative impact of the conservation practice.",
        
        TRUE ~
          "Change was similar in the treatment and control plot, so no influence of the management intervention was detected. Possible explanations include benefits accruing more slowly than the monitoring timeline, or unexpected impacts of the conservation practice."
      )
    )%>%
    pull(text) %>%          # get vector of strings
    paste(collapse = " ")
  
