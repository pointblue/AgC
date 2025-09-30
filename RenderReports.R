# ---- Rendering ACTION Reports ----
# Author: Avalon Cook
# Date created: 20241205
# Date updated: 20241205

###Reports will write to
#G:/Shared drives/ACTION/MonitoringSupport/ACTION_MonitoringData/LandStewardReports
#And the relevant project folder in G:/Shared drives/ACTION/MonitoringSupport/TAP Resources/Data management/Deliverable Uploads

## ---- 1. Set up ----

#Rendered reports are stored in ACTION > MonitoringSupport > ACTION_MonitroingData > LandStewardReports and are named by [PROJECTCODE]_Report_[DATE].html

#Ensure environment is clear
rm(list = ls())
renv::restore()

#establish the path where ACTION data is stored
data_path<-("G:/Shared drives/Ag-C (ACTION)/MonitoringSupport/ACTION_MonitoringData") 

#write a function to render a report with markdown; it will save to LandStewardReports and to the TAP's project folder
render_one_html <- function(project) {
  rmarkdown::render(
    input = 'LandStewardReports_BASIC.Rmd', #identify the markdown file that will be used to render the report; update this as we develop ACTION reports V2
    output_file = paste0(project, '_Report_', Sys.Date(), '.html'), #ID the file path and naming pattern
    output_dir = "G:/Shared drives/Ag-C (ACTION)/MonitoringSupport/ACTION_MonitoringData/LandStewardReports",
    params = list(project = project), #"project" references the name of the parameter in the YAML header; "project" represents the current throughput of the loop
    envir = parent.frame()
  )
  
  dir2<- "G:/Shared drives/Ag-C (ACTION)/MonitoringSupport/TAP Resources/Data management/Deliverable Uploads"
  readwrite_path<-list.dirs(dir2, recursive=TRUE)[grepl(project, basename(list.dirs(dir2, recursive=TRUE)))]
  file.copy(from = paste0("G:/Shared drives/Ag-C (ACTION)/MonitoringSupport/ACTION_MonitoringData/LandStewardReports/", project, '_Report_', Sys.Date(), '.html'), to = readwrite_path, overwrite = TRUE)
}

render_one_PDF <- function(project) {
  rmarkdown::render(
    input = 'LandStewardReports_BASIC_PDF.Rmd', #identify the markdown file that will be used to render the report; update this as we develop ACTION reports V2
    output_file = paste0(project, '_Report_', Sys.Date(), '.pdf'), #ID the file path and naming pattern
    output_dir = "G:/Shared drives/Ag-C (ACTION)/MonitoringSupport/ACTION_MonitoringData/LandStewardReports",
    params = list(project = project), #"project" references the name of the parameter in the YAML header; "project" represents the current throughput of the loop
    envir = parent.frame()
  )
  
  dir2<- "G:/Shared drives/Ag-C (ACTION)/MonitoringSupport/TAP Resources/Data management/Deliverable Uploads"
  readwrite_path<-list.dirs(dir2, recursive=TRUE)[grepl(project, basename(list.dirs(dir2, recursive=TRUE)))]
  file.copy(from = paste0("G:/Shared drives/Ag-C (ACTION)/MonitoringSupport/ACTION_MonitoringData/LandStewardReports/", project, '_Report_', Sys.Date(), '.pdf'), to = readwrite_path, overwrite = TRUE)
}


## ---- 2. Render One Report ----

#If you'd just like to render one report, change the function input to the project code, and run

render_one_PDF("PTRA.19.CA") #for a static PDF report

render_one_html("PTRA.19.CA") #for an HTML report with interactive maps

## ---- 3. Establish Report Names ----
#If you want to batch render reports, run this section; it will pull all project codes for you

#List all master datasheets
MasterSheetsList<-list.files( #lists all the CSVs in the directory
  path = paste0(data_path, "/MasterDatasheets"),
  pattern = "^ACTION_Master_Datasheet.*\\.csv$",
  full.names = TRUE)

#identify the file paths of the most recent and second most recent master datasheets
MasterDatasheet_MostRecent_file <- MasterSheetsList[which.max(as.Date(substr(MasterSheetsList, 113, 122), format = "%Y-%m-%d"))] #this indexing patterns makes sure we're using the most recent master datasheet
MasterDatasheet_SecondMostRecent_file <- MasterSheetsList[which.max(as.Date(substr(MasterSheetsList, 113, 122), format = "%Y-%m-%d"))-1] #this indexing patterns gets the second most recent data sheet

#read in CSVs for the most recent and second most recent master datasheets
MasterDatasheet.MostRecent <- read.csv(MasterDatasheet_MostRecent_file) #read it in
MasterDatasheet.SecondMostRecent <- tryCatch(read.csv(MasterDatasheet_SecondMostRecent_file), error=function(e){data.frame()}) #tryCatch returns an empty dataframe for our current special case since there's only one master datasheet so far

#identify project codes included in the most recent and second most recent master datasheets
all_projects<-unique(substr(MasterDatasheet.MostRecent$Sample.ID, 1, 10))
secondmostrecent_projects<-unique(substr(MasterDatasheet.SecondMostRecent$Sample.ID, 1, 10)) 

#identify the projects that are new to the most recent datasheet; these should be projects that haven't had reports rendered yet
new_projects<-setdiff(all_projects, secondmostrecent_projects)



## ---- 4. Loop Render Reports ----

#Use all_projects to render reports for every complete ACTION project
#Use new_projects to render reports for projects that are new to the most recent master datasheet

#For static PDF reports
for (project in new_projects){
  render_one_PDF(project)
}

#For HTML reports with dynamic maps
for (project in new_projects){
  render_one_html(project)
}

###Reports will write to
    #G:/Shared drives/ACTION/MonitoringSupport/ACTION_MonitoringData/LandStewardReports
    #And the relevant project folder in G:/Shared drives/ACTION/MonitoringSupport/TAP Resources/Data management/Deliverable Uploads
