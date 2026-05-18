#Land Steward Report Stats Development Script
#Created by AC on 11/30/2025 updated 12/22/2025

library(english)
library(lme4)
library(emmeans)
library(pbkrtest)

# ---- Data prep ----

#For all stats, we need to look at either the only existing timepoint, or the first vs last timepoint
#This is only relevant for when we have 3 or more timepoints (we don't), but future-proofing now
if(length(unique(PointLevel$timepoint))>1){
PointLevel_stats<-PointLevel%>%
  mutate(timepoint_digit = readr::parse_number(timepoint))%>%
  filter(timepoint_digit %in% c(min(timepoint_digit), max(timepoint_digit)))%>%
  arrange(timepoint)%>%
  mutate(timepoint = factor(timepoint, levels = c(first(timepoint), last(timepoint))))%>%
  arrange(plot_type)
tp_first <- min(as.character(PointLevel_stats$timepoint))
tp_last  <- max(as.character(PointLevel_stats$timepoint))

#this section for handling a special case where there is a control at T1 but not at baseline
PointLevel_stats <-  PointLevel_stats %>%
  group_by(plot_type) %>%
  filter(all(c(tp_first, tp_last) %in% as.character(timepoint))) %>%
  ungroup()


} else {PointLevel_stats<-PointLevel%>%arrange(plot_type)}

# ---- SOC% ----

  ## ---- RACA Percentiles ----
  
  #Calculate and verbalize raca-mean percentiles for the most recent timepoint in any presnt plot
    raca_ecdf_soc <- soc_df %>% 
      filter(plot_type == "comparison") %>% 
      pull(org_c) %>% 
      ecdf()    
    
    raca_compare_soc<-Project.Means%>%
      mutate(timepoint_digit = readr::parse_number(timepoint))%>%
      filter(timepoint_digit == max(timepoint_digit))%>%
      mutate(raca_percentile_raw = raca_ecdf_soc(org_c) * 100,
             raca_percentile = case_when(
               raca_percentile_raw <= 0 ~ 0L,
               TRUE ~ floor(raca_percentile_raw)),
             plot_full = case_when(plot_type=="T"~"treatment", plot_type=="C"~"control"),
             text = case_when(
               
               raca_percentile > 0 & raca_percentile < 100 ~ paste0("The most recent mean value in the ", 
                           plot_full, 
                           " plot is in the **", 
                           english::ordinal(raca_percentile), 
                           " percentile** of the comparison dataset for your ecoregion."
                           ),
             raca_percentile<=0 ~ paste0("The most recent mean value in the ", 
                                         plot_full, 
                                         " plot is **less than any value** in the comparison dataset for your ecoregion."
                                         ),
             raca_percentile>=100 ~ paste0("The most recent mean value in the ", 
                                         plot_full, 
                                         " plot is **more than any value** in the comparison dataset for your ecoregion."
             )
              ) )
             
      percentile_scentence_soc<-raca_compare_soc%>%
      arrange(desc(plot_type))%>% #This makes it so the T sentence prints before the C sentence
      pull(text) %>%          # get vector of strings
      paste(collapse = " ")

  ## ---- SOC% stats ----
        
    ### ---- 1TP, T only ----
      
      #No stats
  
    ### ---- >1TP, T only ----
    
    #Time contrast
    if(length(unique(PointLevel_stats$timepoint))>1 & length(unique(PointLevel_stats$plot_type))==1){
    model <- lmer(org_c ~ timepoint + (1 | sample_id), data = PointLevel_stats)
    emm <- emmeans(model, ~ timepoint, ddf="Kenward-Roger")
    time_change_soc <- contrast(emm, method = setNames(list(c(-1, 1)),paste0(tp_last, " - ", tp_first))) %>%
      as.data.frame()%>%
      mutate(
        text = case_when(
          p.value < 0.05 & estimate > 0 ~ "SOC% in the treated plot **increased** between the first and most recent monitoring timepoint. Though it may seem the practice is increasing soil organic matter and fertility, it may also be a result of other environmental or management changes across years. A longer monitoring timeframe will strengthen our confidence in this result.",
          p.value < 0.05 & estimate < 0 ~ "SOC% in the treated plot **decreased** between the first and most recent monitoring timepoint. Without a control field, we are unable to tell whether this is due to environmental or management changes across years. A longer monitoring timeframe will strengthen our confidence in this result.",
          TRUE                          ~ "SOC% **did not change** between the two monitoring timepoints. Management either protected existing carbon, rather than increasing it, or high soil variability and/or a short timeframe made changes hard to detect."
        )
      )%>%
      pull(text) %>%          # get vector of strings
      paste(collapse = " ")
    }
  
    ### ---- 1TP, T&C ----
    
    #Treatment contrast
    if (length(unique(PointLevel_stats$plot_type))>1 & length(unique(PointLevel_stats$timepoint))==1){
    model <- lm(org_c ~ plot_type, data = PointLevel_stats)
    emm <- emmeans(model, ~ plot_type, ddf="Kenward-Roger")
    treatment_contrast_soc<-contrast(emm, method = setNames(list(c(-1, 1)), "T - C")) %>%
      as.data.frame()%>%
      mutate(
        text = case_when(
          p.value < 0.05 & estimate > 0 ~ "At the current timepoint, SOC% is **higher in the treated plot** than the control. This difference should be taken into consideration when looking at change in each plot over time.",
          p.value < 0.05 & estimate < 0 ~ "At the current timepoint, SOC% is **lower in the treated plot** than the control. This difference should be taken into consideration when looking at change in each plot over time.",
          TRUE ~ "There is **no significant difference** between SOC% in the treated and control site at this time. This means that the sites are well-matched."
        )
      )%>%
      pull(text) %>%          # get vector of strings
      paste(collapse = " ")
    }
  
  
    ### ---- >1TP, T&C ----
  
    #Plot-wise change over time  
    if(length(unique(PointLevel_stats$timepoint))>1 && length(unique(PointLevel_stats$plot_type))>1){
      
    model <- lmer(org_c ~ plot_type * timepoint + (1 | sample_id),data = PointLevel_stats)
    emm <- emmeans(model, ~ plot_type * timepoint, at=list(plot_type = c("C", "T"), timepoint = c(tp_first, tp_last)), ddf="Kenward-Roger")
    time_contrasts_soc <- contrast(emm, by = "plot_type", method = setNames(list(c(-1, 1)), paste0(tp_last, " - ", tp_first))) %>%
      as.data.frame() %>%
      mutate(
        plot_full = case_when(plot_type=="T"~"treatment", plot_type=="C"~"control"),
        condition = case_when(
          p.value < 0.05 & estimate > 0 ~ "increased",
          p.value < 0.05 & estimate < 0 ~ "decreased",
          TRUE ~ "no change"
        ))
    
    #statements for DIFFERENT direction of change in each plot
    if (length(unique(time_contrasts_soc$condition)) >1){
      
      #C decrease T increase
      if (all(time_contrasts_soc[time_contrasts_soc$plot_type == "T", ]$condition == "increased") & all(time_contrasts_soc[time_contrasts_soc$plot_type == "C", ]$condition == "decreased")) { 
        baci_soc<-"While SOC% at your **control site is decreasing**, SOC% at the **treatment site is increasing**. This indicates the management intervention is effectively protecting and enhancing soil organic matter, as expected."
      }
      
      #C steady T increase
      if (all(time_contrasts_soc[time_contrasts_soc$plot_type == "T", ]$condition == "increased") & all(time_contrasts_soc[time_contrasts_soc$plot_type == "C", ]$condition == "no change")) { 
        baci_soc<-"INSERT TEXT"
      }
      
      #C decrease T steady
      if (all(time_contrasts_soc[time_contrasts_soc$plot_type == "T", ]$condition == "no change") & all(time_contrasts_soc[time_contrasts_soc$plot_type == "C", ]$condition == "decreased")) { 
        baci_soc<-"INSERT TEXT"
      }
      
      #T decrease C increase
      if (all(time_contrasts_soc[time_contrasts_soc$plot_type == "T", ]$condition == "decreased") & all(time_contrasts_soc[time_contrasts_soc$plot_type == "C", ]$condition == "increased")) {
        baci_soc<-"While SOC% in the **treatment plot is declining**, SOC% in the **control plot is increasing**. This indicates an unexpected negative impact of the conservation practice or inherent differences between the treatment and control plots. Consider contacting the Point Blue Soils Team for further interpretation."
      }
      
      #T steady C increase
      if (all(time_contrasts_soc[time_contrasts_soc$plot_type == "T", ]$condition == "no change") & all(time_contrasts_soc[time_contrasts_soc$plot_type == "C", ]$condition == "increased")) {
        baci_soc<-"INSERT TEXT"
      }
      
      #T decrease C steady
      if (all(time_contrasts_soc[time_contrasts_soc$plot_type == "T", ]$condition == "decreased") & all(time_contrasts_soc[time_contrasts_soc$plot_type == "C", ]$condition == "no change")) {
        baci_soc<-"INSERT TEXT"
      }
    }else{
    
      #Statements for SAME DIRECTION OF CHANGE in each plot (testing for treatment effect)
      
      #Both plots steady (no further test needed, just text)  
      if (all(unique(time_contrasts_soc$condition) == "no change")){
        baci_soc<-"SOC% in both plots is **steady over time**. This tells us that general conditions have not altered SOC% over the monitoring timeline, and the impact of the conservation practice is not currently detectable. The adopted practice might still be beneficial, but its impact cannot be detected yet due to high soil variability and/or a short timeframe."
      }
      
      #Both plots decreasing
      if(all(unique(time_contrasts_soc$condition) == "decreased")){
      baci_soc <- contrast(emm, method = setNames(list(c(1, -1, -1, 1)), "BACI: (T1 - T0)_T - (T1 - T0)_C")) %>%
        as.data.frame() %>%
        mutate(
          text = case_when(
            p.value < 0.05 & estimate > 0 ~
              "SOC% at the treated site **declined less** relative to the control site, indicating a positive impact of the conservation practice.",
            
            p.value < 0.05 & estimate < 0 ~
              "SOC% at the treated site **decreased at a greater rate** than the control site, indicating a negative impact of the conservation practice.",
            
            TRUE ~
              "Decreases in SOC% were observed in your field but were **not due to practice changes**, since these decreases were observed in both the treatment and control sites. The adopted practice might still be beneficial in protecting existing soil carbon, but its impact cannot be detected yet due to high soil variability and/or a short timeframe."
          )
        )%>%
        pull(text) %>%          # get vector of strings
        paste(collapse = " ")
      }
      
      #Both plots increasing
      if (all(unique(time_contrasts_soc$condition) == "increased")){
        baci_soc <- contrast(emm, method = setNames(list(c(-1, 1, 1, -1)), "BACI: (T1 - T0)_T - (T1 - T0)_C")) %>%
          as.data.frame() %>%
          mutate(
            text = case_when(
              p.value < 0.05 & estimate > 0 ~
                "SOC% at the treated site **increased more** relative to the control site, indicating a positive impact of the conservation practice.",
              
              p.value < 0.05 & estimate < 0 ~
                "SOC% at the treated site **increased less** than the control site, indicating a negative impact of the conservation practice.",
              
              TRUE ~
                "Increases in SOC% were observed in your field but were **not due to practice changes**, since these increases were observed in both the treatment and control sites. The adopted practice might still be beneficial, but its impact cannot be detected yet due to high soil variability and/or a short timeframe.")
          )%>%
          pull(text) %>%          # get vector of strings
          paste(collapse = " ")
        }
      }
      }
      
    
