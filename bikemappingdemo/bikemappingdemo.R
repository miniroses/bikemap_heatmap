library('shiny')
options(shiny.maxRequestSize = 20000*1024^2)
myIcon = makeAwesomeIcon(icon = "fa-bicycle", library = "fa",
                         markerColor = "red", iconColor = "yellow", spin = FALSE,
                         extraClasses = NULL)
server<-function(input, output,session) {
  
  output$mymap <- renderLeaflet({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    leaflet(df) %>%addProviderTiles("OpenStreetMap.Mapnik")%>% addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius=3,
      weight=2,
      label= ~as.character(Bike_id),
      fill = TRUE, color = 'blue',
      fillOpacity = 1
      
    )%>%
      addAwesomeMarkers(icon=myIcon,clusterOptions = markerClusterOptions(freezeAtZoom = FALSE))
    
  })
  
  observeEvent(input$mymap_marker_click, {
    leafletProxy("mymap", session) %>%
      removeMarker(input$map1_marker_click$id)
  })
  
  
  
  
  
  
}


ui<-fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                  
                )
      ),
      width =12
      
      
    ),
    
    
    
    
    
    mainPanel(
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      
      leafletOutput("mymap",width = 1500,height =800)
      #tableOutput("contents")
      
    )
  )
)



shinyApp(ui,server)