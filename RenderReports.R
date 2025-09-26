# ---- Rendering ACTION Reports ----
# Author: Avalon Cook
# Date created: 20241205
# Date updated: 20250926

###Reports will write to
#Z:\Soils Team\AgC Data\RenderedReports

## ---- 1. Set up ----

#Rendered reports are stored in ACTION > MonitoringSupport > ACTION_MonitroingData > LandStewardReports and are named by [PROJECTCODE]_Report_[DATE].html

#Ensure environment is clear
rm(list = ls())
#renv::restore() #not using renv?

#establish the path where ACTION data is stored
data_path<-("G:/Shared drives/Ag-C (ACTION)/MonitoringSupport/ACTION_MonitoringData") 

#write a function to render a report with markdown; it will save to LandStewardReports and to the TAP's project folder
render_one_html <- function(project) {
  rmarkdown::render(
    input = 'LandStewardReports.Rmd', #identify the markdown file that will be used to render the report; update this as we develop ACTION reports V2
    output_file = paste0(project, '_Report_', Sys.Date(), '.html'), #ID the file path and naming pattern
    output_dir = "Z:/Soils Team/AgC Data/RenderedReports",
    params = list(project_name = project), #"project" references the name of the parameter in the YAML header; "project" represents the current throughput of the loop
    envir = parent.frame()
  )
}


## ---- 2. Render One Report ----

#If you'd just like to render one report, change the function input to the project code, and run

render_one_html("JPNC") #for an HTML report with interactive maps


## ---- 4. Loop Render Reports ----

#establish a vector called new_projects, then run the following loop:
new_projects <- c()

#For HTML reports with dynamic maps
for (project in new_projects){
  render_one_html(project)
}

###Reports will write to
    #Z:\Soils Team\AgC Data\RenderedReports