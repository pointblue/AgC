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

#write a function to render one report with markdown
render_one_html <- function(project) {
  rmarkdown::render(
    input = 'LandStewardReports.Rmd', #identify the markdown file that will be used to render the report
    output_file = paste0(project, '_Report_', Sys.Date(), '.html'), #ID the file path and naming pattern
    output_dir = "Z:/Soils Team/AgC Data/RenderedReports",
    params = list(project_name = project), #"project_name" references the name of the parameter in the YAML header; "project" represents the current throughput of the loop
    envir = parent.frame()
  )
}

#layering that so you can pass a vector of project names to render many reports at once
render_html_report <- function(projects){ #when projects is a project name or a vector of project names
  for(project in projects){
    render_one_html(project)
  }
}


#Reports will write to
    #Z:\Soils Team\AgC Data\RenderedReports