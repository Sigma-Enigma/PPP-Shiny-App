# Interactive PPP Mapping Widget UI

require(rgdal)
require(dplyr)
require(RColorBrewer)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

#require(rmapshaper)
# require(profvis) # for optimization testing

load(file = "PPP_Widget_Data_Cleaned.RData")
# find way to autorun this script upon navigating to the web-page

######### DASHBOARD WIDGET BEGIN ############

# conside including a submit button to run changes in inputs (or longer delay in waiting for input changes to stop)

# define UI for dashboard
ui <- dashboardPage(
  skin = "red",
  dashboardHeader( title = "PPP Loan Dashboard"),
  dashboardSidebar(
    dateRangeInput(
      "DateApproved", 
      label = "Date Range",
      start = min(pppDataCalifornia$DateApproved),
      end = max(pppDataCalifornia$DateApproved),
      min = min(pppDataCalifornia$DateApproved),
      max = max(pppDataCalifornia$DateApproved),
      format = "yyyy-mm-dd",
      separator = " to "
    ),
    selectInput(inputId = "selectDataLayer", label = "Select Data Layer", choices = names(zipAndDateAggregatedData[3:length(zipAndDateAggregatedData)]) ),
    actionButton("actionButton", "Submit"),
    p("Click the Submit button to display the data for the features selected in the side panel.", "Note: Widget is currently hosted on a free server so expect 5-10 second load times.")
    
  ),
  dashboardBody(
    fluidRow( box( width = 12, leafletOutput( outputId = "leafletMap", height = "750px" ) ) ), #adjust height when publishing
    fluidRow( box( width = 12, dataTableOutput( outputId = "summaryTable" ) ) )
  )
  
)
