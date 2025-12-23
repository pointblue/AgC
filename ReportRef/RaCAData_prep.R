#Prepping RaCA data for LandSteward Reports

# ---- Raca prep ----
raca_call<-reg_baseline(border, max.depth.cent)
raca_data<-raca_call[[1]]%>%left_join(raca_call[[2]]%>%select(-LU), by="rcasiteid")
ecoregion<-raca_call[[3]]

raca_data$LU[raca_data$LU == "C"] <- "crop"
raca_data$LU[raca_data$LU == "R"] <- "range"