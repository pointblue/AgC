#Land Steward Report Stats Development Script
#Created by AC on 11/30/2025 updated 12/16/2025

library(english)
library(lme4)
library(emmeans)
library(soiltexture)

# ---- Data prep ----

texture <- PointLevel_stats %>% 
  rename(SAND=sand, SILT=silt, CLAY=clay) %>%
  filter(!is.na(CLAY)) %>%
  filter(!is.na(SAND)) %>%
  filter(!is.na(SILT))%>%
  mutate(across(c(CLAY, SAND, SILT), as.numeric))

texture_avg <- texture %>%
  group_by(plot_type)%>%
  summarise(
    SAND = mean(SAND, na.rm = TRUE),
    SILT = mean(SILT, na.rm = TRUE),
    CLAY = mean(CLAY, na.rm = TRUE)
  )%>%filter(plot_type=="T")%>%as.data.frame()


#get average texture types for T and C plot
if(length(unique(PointLevel_stats$plot_type))>1){
texture_avg_list <- split(texture, texture$plot_type) %>%
  lapply(function(df){
    df_avg <- df %>%
      summarise(
        SAND = mean(SAND, na.rm = TRUE),
        SILT = mean(SILT, na.rm = TRUE),
        CLAY = mean(CLAY, na.rm = TRUE)
      )
    # Assign USDA texture
    df_avg$USDA_texture <- TT.points.in.classes(
      tri.data = df_avg,
      class.sys = "USDA.TT",
      PiC.type = "t"
    )
    df_avg
  })
}

avg_tx_t<-get_texture_info(texture_avg_list$T$USDA_texture)$full_name

#Write summary sentences
if("C" %in% names(texture_avg_list)){
avg_tx_c<-get_texture_info(texture_avg_list$C$USDA_texture)$full_name
  if(identical(avg_tx_t, avg_tx_c)){
  tx_avg_sentence<-paste0("The average texture type in both plots is **", avg_tx_t, "**. ")
    } else {tx_avg_sentence<-paste0("The average texture type is **", avg_tx_t, "** in the treated plot and **",avg_tx_c, "** in the control plot. ")
    }
} else{tx_avg_sentence<-paste0("The average texture type in the treated plot is **", avg_tx_t, "**. ")}

  
    ### ---- Comparing %Clay%Silt, T&C ----
    
  texture<-texture%>%mutate(
    siltclay=SILT+CLAY
  )

    #Treatment contrast
    if (length(unique(PointLevel_stats$plot_type))>1){
    model <- lm(siltclay ~ plot_type, data = texture)
    anova(model)
    emm <- emmeans(model, ~ plot_type)
    treatment_contrast_siltclay<-contrast(emm, method = setNames(list(c(-1, 1)), "T - C")) %>%
      as.data.frame()%>%
      mutate(
        text = case_when(
          p.value < 0.05 & estimate > 0 ~ "The treated plot has finer textured soils than the control. This difference can impact the way the two plots store carbon over time.",
          p.value < 0.05 & estimate < 0 ~ "The treated plot has courser textured soils than the control. This difference can impact the way the two plots store carbon over time.",
          TRUE ~ "There is **no significant difference** in fine particle content between the treated and control sites. This means that the sites are well-matched, making interpretation of future results straightforward."
        )
      )%>%
      pull(text) %>%          # get vector of strings
      paste(collapse = " ")
    } 
  