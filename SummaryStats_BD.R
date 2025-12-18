#Land Steward Report Stats Development Script
#Created by AC on 11/30/2025 updated 12/16/2025

library(english)
library(lme4)
library(emmeans)

# ---- Data prep ----

#For all stats, we need to look at either the only existing timepoint, or the first vs last timepoint
#This is only relevant for when we have 3 or more timepoints (we don't), but future-proofing now
if(length(unique(PointLevel$timepoint))>1){
  PointLevel_stats<-PointLevel%>%
    mutate(timepoint_digit = readr::parse_number(timepoint))%>%
    filter(timepoint_digit %in% c(min(timepoint_digit), max(timepoint_digit)))%>%
    arrange(timepoint)%>%
    mutate(timepoint = factor(timepoint, levels = c(first(timepoint), last(timepoint))))
  tp_first <- first(PointLevel_stats$timepoint)
  tp_last  <- last(PointLevel_stats$timepoint)
} else {PointLevel_stats<-PointLevel}

# ---- Bulk Density ----

## ---- RACA Percentiles ----

#Calculate and verbalize raca-mean percentiles for the most recent timepoint in any presnt plot
raca_ecdf_bd <- bd_df %>% 
  filter(plot_type == "raca") %>% 
  pull(bulk_density) %>% 
  ecdf()    

raca_compare_bd<-Project.Means%>%
  mutate(timepoint_digit = readr::parse_number(timepoint))%>%
  filter(timepoint_digit == max(timepoint_digit))%>%
  mutate(raca_percentile_raw = raca_ecdf_bd(bulk_density) * 100,
         raca_percentile = case_when(
           raca_percentile_raw <= 0 ~ 0L,
           TRUE ~ floor(raca_percentile_raw)),
         plot_full = case_when(plot_type=="T"~"treatment", plot_type=="C"~"control"),
         text = case_when(
           
           raca_percentile>0 ~ paste0("The most recent mean value in the ", 
                                      plot_full, 
                                      " plot is in the **", 
                                      english::ordinal(raca_percentile), 
                                      " percentile** of the comparison dataset for your ecoregion."
           ),
           raca_percentile<=0 ~ paste0("The most recent mean value in the ", 
                                       plot_full, 
                                       " plot is **less than any value** in the comparison dataset for your ecoregion."
           )
         ) )

percentile_scentence_bd<-raca_compare_bd%>%
  arrange(desc(plot_type))%>% #This makes it so the T sentence prints before the C sentence
  pull(text) %>%          # get vector of strings
  paste(collapse = " ")

## ---- BD stats ----

### ---- 1TP, T only ----

#No stats

### ---- >1TP, T only ----

#Time contrast
if(length(unique(PointLevel$timepoint))>1){
  model <- lmer(bulk_density ~ timepoint + (1 | sample_id), data = PointLevel_stats)
  emm <- emmeans(model, ~ timepoint)
  time_change_bd <- contrast(emm, method = setNames(list(c(-1, 1)),paste0(tp_last, " - ", tp_first))) %>%
    as.data.frame()%>%
    mutate(
      text = case_when(
        p.value < 0.05 & estimate > 0 ~ "Bulk density in the treated plot **increased** between the first and most recent monitoring timepoint. Without a control field, we are unable to tell whether this is due to changes in management or other untested factors across years.",
        p.value < 0.05 & estimate < 0 ~ "Bulk density in the treated plot **decreased** between the first and most recent monitoring timepoint. Though this suggests that the practice is positively affecting soil structure and compaction, it may also be a result of changes in untested factors across years.",
        TRUE                          ~ "Bulk density **did not meaningfully change** between the two monitoring timepoints. Possible explanations for a lack of significant change include benefits acrruing more slowly than the monitoring timeline, and unexpected impacts of the conservation practice."
      )
    )%>%
    pull(text) %>%          # get vector of strings
    paste(collapse = " ")
}


### ---- 1TP, T&C ----

#Treatment contrast
model <- lm(bulk_density ~ plot_type, data = PointLevel_stats)
anova(model)
emm <- emmeans(model, ~ plot_type)
treatment_contrast_bd<-contrast(emm, method = setNames(list(c(-1, 1)), "T - C")) %>%
  as.data.frame()%>%
  mutate(
    text = case_when(
      p.value < 0.05 & estimate > 0 ~ "At the current timepoint, bulk density is **higher in the treated plot** than the control. This difference should be taken into consideration when looking at change in each plot over time.",
      p.value < 0.05 & estimate < 0 ~ "At the current timepoint, bulk density is **lower in the treated plot** than the control. This difference should be taken into consideration when looking at change in each plot over time.",
      TRUE ~ "There is **no significant difference** between bulk density in the treated and control site at this time. This means that the sites are well-matched, making interpretation of future results straightforward."
    )
  )%>%
  pull(text) %>%          # get vector of strings
  paste(collapse = " ")



### ---- >1TP, T&C ----

#Plot-wise change
if(length(unique(PointLevel$timepoint))>1){
  model <- lmer(bulk_density ~ plot_type * timepoint + (1 | sample_id),data = PointLevel_stats)
  emm <- emmeans(model, ~ plot_type * timepoint, at=list(plot_type = c("C", "T"), timepoint = c(tp_first, tp_last)))
  time_contrasts_bd <- contrast(emm, by = "plot_type", method = setNames(list(c(-1, 1)), paste0(tp_last, " - ", tp_first))) %>%
    as.data.frame() %>%
    mutate(
      plot_full = case_when(plot_type=="T"~"treatment", plot_type=="C"~"control"),
      text = case_when(
        p.value < 0.05 & estimate > 0 ~ paste0("Bulk density in the ", plot_full, " plot **increased** between the first and most recent monitoring timepoint."),
        p.value < 0.05 & estimate < 0 ~ paste0("Bulk density in the ", plot_full, " plot **decreased** between the first and most recent monitoring timepoint."),
        TRUE                          ~ paste0("**No significant change** in bulk density was detected in the ", plot_full, " plot between the first and most recent monitoring timepoint.")
      )
    )%>%
    arrange(desc(plot_type))%>% #This makes it so the T sentence prints before the C sentence
    pull(text) %>%          # get vector of strings
    paste(collapse = " ")
  
  #Treatment effect
  baci_bd <- contrast(emm, method = setNames(list(c(-1, 1, 1, -1)), "BACI: (T1 - T0)_T - (T1 - T0)_C")) %>%
    as.data.frame() %>%
    mutate(
      text = case_when(
        p.value < 0.05 & estimate > 0 ~
          "Bulk density at the treated site **increased (or declined less)** relative to the control site, indicating a negative impact of the conservation practice on soil structure and compaction.",
        
        p.value < 0.05 & estimate < 0 ~
          "Bulk density at the treated site **decreased at a greater rate** than the control site, indicating a positive impact of the conservation practice on soil structure and compaction.",
        
        TRUE ~
          "Change was similar in the treatment and control plot, so **no influence of the management intervention** was detected. Possible explanations include benefits accruing more slowly than the monitoring timeline, or an outsized impact of other factors relative to the impact of the management practice."
      )
    )%>%
    pull(text) %>%          # get vector of strings
    paste(collapse = " ")
}

