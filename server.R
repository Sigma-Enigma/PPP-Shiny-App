# Interactive PPP Mapping Widget Server

library(rgdal)
library(dplyr)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)


# Define server input/output logic to pass map data and build map widget
server <- function(input, output){
  
  # select dataset
  zipAndDateAggregatedData <- reactive({
    filename <- paste0(input$stateName, ".Rdata")
    get(load(filename)) # this is loading and overwriting all 3 objects then storing the last one as pppData
  }) # look into observe() function, and reconsider your data architecture
  # may need to break up this workspace load into 3 separate object loads, as you are updating 3 separate objects
  # will need to change values like ZipAndDateAggregatedData to be dynamic
  # https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent
  # https://shiny.rstudio.com/reference/shiny/latest/observe.html
  
  dataInput <- eventReactive(input$actionButton, {
    # use dplyr to aggregate data and create tables to pass to polygons 
    zipAndDateAggregatedData() %>%
      filter( DateApproved >= input$DateApproved[1] ) %>% # lower slider bound
      filter( DateApproved <= input$DateApproved[2] ) %>% # upper slider bound
      group_by( Zip ) %>% # create data table given slider inputs
      summarize(Total_Amount_Loaned = sum(as.numeric(Total_Amount_Loaned), na.rm = TRUE),
                Total_Loans_Approved = n(), 
                Total_Jobs_Retained = sum(as.numeric(Total_Jobs_Retained), na.rm = TRUE)
                # add proportion of each business type
                # add proportion of each industry type NAICS
                # add CONTROL VARIABLES!!!
      )
    
  })
  
  # re-order data for label values so they line up nicely with zipboundary polygons table order
  dataInputOrdered <- eventReactive(input$actionButton, { 
    dataInput()[order(match(dataInput()$Zip, finalZipBoundaryShapefile()$ZCTA5CE10)), ] 
  })
  
  
  # make eventReactive value = data layer variable name
  inputDataLayerName <- eventReactive(input$actionButton, {
    input$selectDataLayer[1]
  })
  
  # make eventReactive value = values of above data layer
  inputDataLayerValues <- eventReactive(input$actionButton, {
    pull( dataInputOrdered(), inputDataLayerName() )
  })
  
  colorPalette <- eventReactive(input$actionButton, { colorBin("RdYlBu", reverse = TRUE, domain = c(min( inputDataLayerValues() ), max( inputDataLayerValues() ) ), na.color = "#808080") 
    
  })
  
  # set label variables to pass data 
  # to do: include if statement to bold the text of the displayed data layer
  labels <- eventReactive(input$actionButton, {
    paste("<p>", "Zip Code Region: ", dataInputOrdered()$Zip, "<p>",
          "<p>", "Total Amount Loaned: ", format(dataInputOrdered()$Total_Amount_Loaned, nsmall = 0, big.mark = ","), "<p>",
          "<p>", "Total Loans Approved: ", format(dataInputOrdered()$Total_Loans_Approved, nsmall = 0, big.mark = ","), "<p>",
          "<p>", "Total Jobs Retained: ", format(dataInputOrdered()$Total_Jobs_Retained, nsmall = 0, big.mark = ","), "<p>",
          sep = "")
    
  })
  
  # build and render leaflet map
  output$leafletMap <- renderLeaflet( 
    leaflet(finalZipBoundaryShapefile()) %>% 
      setView(lng = state.center$x[which(state.name == input$stateName[1] )], lat = state.center$y[which(state.name == input$stateName[1] )], zoom = 7 ) %>% # sets initial map starting view
      addProviderTiles(providers$CartoDB.Positron) %>%  # adding background base map
      addPolygons( weight = 1, # adding shapefiles
                   smoothFactor = 0.5,
                   color = "white",
                   fillOpacity = 0.25,
                   fillColor = colorPalette()( inputDataLayerValues() ), # note: double parenthesis because its a eventReactive function!
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "#ffffff",
                     dashArray = NULL,
                     fillOpacity = 0.5,
                     bringToFront = TRUE
                   ),
                   label = lapply(labels(), HTML)) %>%
      addLegend( pal = colorPalette(), # will need to be adjusted later with dropdown layers
                 values = ~inputDataLayerValues() , # dynamically changes with layers, (should it keep constant with date changes?)
                 opacity = 0.25,
                 position = "topright",
                 title = paste( inputDataLayerName()  ) ) # dynamically changes this value when switching layers
    
  ) 
  
  # build and render data table
  output$summaryTable <- renderDataTable( format.data.frame(dataInput(), big.mark = ",") )
  
}

# execute shinyApp 
#shinyApp(ui = ui, server = server)

######## DASHBOARD WIDGET END ##########

# dynamically adjust legend color scale (and polygonFill) when changing date ranges
# figure out how to use leafletProxy to speed up execution
# add additional data layers



# Extra stuff

# other data: 
# https://www.sba.gov/about-sba/sba-performance/open-government/digital-sba/open-data/open-data-sources
# https://www.sba.gov/article/2020/jul/13/sba-treasury-announce-release-paycheck-protection-program-loan-data


# NOTE: Consider reproduing this widget but instead at state level, rather than zip level (note will need to merge all state tables), this would render much more quickly and might be more interesting to a general audience.