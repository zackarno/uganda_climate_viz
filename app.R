#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(ggreach)
library(dplyr)
library(sf)
library(highcharter)
# library(readr)
# reach_cols()
rawdata<-readr::read_rds("rawdata.rds")

rawdata$district<- rawdata$district %>%
    filter(id!=129)


rawdata$district<- rawdata$district %>%
  rename(Precipitation= "cum_mm")


bb<-st_bbox(rawdata$district)


avg_precip_uga<-rawdata$precip_district %>%
    group_by(date) %>%
    summarise(
        precip_avg= mean(precip,na.rm=T)
    )


leafmap<-leaflet(options= leafletOptions(zoomSnap = 0.1,
                                         zoomDelta=0.1)) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas,
                   options = providerTileOptions(noWrap = T)) %>%
  leaflet::setView(lng = 32.39093,
                   lat =  1.278573,zoom = 7.7)




# Define UI for application that draws a histogram
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(
      # custom CSS
      includeCSS("styles.css")
    ),
    leafletOutput("mymap", width = "100%", height = "100%"),
    absolutePanel(id = "time_series_box", class = "panel panel-default", fixed = TRUE,
                  draggable = F, top = 25, left = "auto", right = 20,
                  width = 325,
                  p(highchartOutput("precip_plot"))),
    absolutePanel(id = "parameters", class = "panel panel-default", fixed = TRUE,
                  draggable = F, top = 25, right = "auto", left = 45,
                  width = 295,
                  h3(HTML(sprintf("<span style='color: %s;'><strong>Analysis Options</span></strong>",
                                  reach_cols("medred")))),
                  shinyWidgets::sliderTextInput(inputId = "temporal_opts",
                                                "Temporal Selection:",
                              width = 200,
                              grid=T,
                              choices = c("Historical", "Current Status", "Forecasted"),
                              selected = "Current Status"),
                  selectInput(inputId = "env_opts", "Environmental Options:",
                              choices = c("Precipitation", "NDVI"),
                              selected = "Precipitation",
                              width= 100)
    )



    )


# Define server logic required to draw a histogram
server <- function(input, output) {

get_pal<- reactive({
  if(input$env_opts=="Precipitation"){
    bins<- seq(0, 350, by = 50)
    pal <- colorBin(palette = "YlOrRd", domain = rawdata$district$Precipitation, bins = bins)
  }
  if(input$env_opts=="NDVI"){
    bins<- pretty(rawdata$district$NDVI)
    pal <- colorBin(palette = "Greens",
                    domain = rawdata$district$NDVI,
                    bins = bins)

  }
  return(pal)
})





output$mymap<- renderLeaflet({
    leafmap
})

observe({
  pal <- get_pal()
  leafletProxy("mymap") %>%
    leaflet::addPolygons(data=rawdata$district,
                         fillColor = ~pal(rawdata$district[[input$env_opts]]),
                         # fillColor = ~pal(!!sym(input$env_opts)),
                         label = rawdata$district[[input$env_opts]],
                         weight = 2,
                         opacity = 1,
                         color = "white",
                         dashArray = "3",
                         fillOpacity = 0.5,
                         layerId = ~DName2019,
                         highlight = highlightOptions(
                           weight = 5,
                           color = "#666",
                           dashArray = "",
                           fillOpacity = 0.2,
                           bringToFront = TRUE))

})


output$precip_plot<-
    renderHighchart({avg_precip_uga %>%
    highcharter::hchart(type = "line",
                        hcaes(x = date, y = precip_avg)) %>%
            hc_title(text=glue::glue("Average {input$env_opts} Across Uganda"))})


observeEvent(input$mymap_shape_click,{
  print("observed map_shap_click")
    print(input$mymap_shape_click$id)
  output$precip_plot <- renderHighchart({
    return(
        rawdata$precip_district %>%
            filter(DName2019==input$mymap_shape_click$id) %>%
            highcharter::hchart(type = "line",
                                hcaes(x = date, y = precip)) %>%
          hc_title(text=glue::glue("Average {input$env_opts} Across {stringr::str_to_title(input$mymap_shape_click$id)} District"))

    )
  })

}
)

}
# Run the application
shinyApp(ui = ui, server = server)
