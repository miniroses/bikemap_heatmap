library(leaflet)
library(htmltools)
library(htmlwidgets)
library(dplyr)
library(shiny)
library(leaflet)

options(shiny.maxRequestSize = 20000*1024^2)
path='C:\\Users\\Administrator\\Desktop\\×ÀÃæ\\pre\\my_shiny\\shiny_heatmap'

server<-function(input, output,session) {
  heatPlugin <- htmlDependency("Leaflet.heat", "99.99.99",
                               src = normalizePath(path),
                               script = "leaflet-heat.js"
  )
  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Integer"),
      Value = as.character(c(input$integer)),
      
      stringsAsFactors = FALSE)
    
  })
  
  # output$values <- renderTable({
  #   sliderValues()
  # })
  
  
  
  output$mymap <- renderLeaflet({
    req(input$file1)
    
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    values1 <- sliderValues()[1,2]
    
    
    leaflet()%>%
      addTiles()%>%
      fitBounds(min(df$longitude), min(df$latitude), max(df$longitude),max(df$latitude)) %>%
      registerPlugin(heatPlugin) %>%
      addCircleMarkers(data=df,color="DarkRed",radius = 1,weight=2) %>%
      onRender(paste0("function(el, x, data) {
                      data = HTMLWidgets.dataframeToD3(data);
                      data = data.map(function(val) { return [val.latitude, val.longitude]; });
                      var heat = L.heatLayer(data, {radius: ",values1,"}).addTo(this);
  }"), data = df %>% select(latitude, longitude))
    
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
      sliderInput("integer", "Integer:",
                  min = 0, max = 30,
                  value = 15),
      width = 12
      
      
      
      
    ),
    
    
    
    mainPanel(
      
      
      leafletOutput("mymap",width = 800,height =500)
      
      #tableOutput("values")
      
    )
  )
)



shinyApp(ui,server)