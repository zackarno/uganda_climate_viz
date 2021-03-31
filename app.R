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


bins<- seq(0, 350, by = 50)
pal <- colorBin("YlOrRd", domain = rawdata$precip_district_cumm, bins = bins)

# ndvi_bins<-pretty(rawdata$district$NDVI)
# ndvi_pal <- colorBin("YlBluGr", domain = rawdata$ndvi_district, bins = ndvi_bins)

bb<-st_bbox(rawdata$district)
# lng = mean(bb[[1]],bb[[3]]),lat = mean(bb[[2]],bb[[4]]),

avg_precip_uga<-rawdata$precip_district %>%
    group_by(date) %>%
    summarise(
        precip_avg= mean(precip,na.rm=T)
    )

leafmap<-leaflet(options= leafletOptions(zoomSnap = 0.1,
                                         zoomDelta=0.05)) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas,
                   options = providerTileOptions(noWrap = T))



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
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
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
# htitle<- glue::glue("Average {input$env_opts} Across Uganda")

output$mymap<- renderLeaflet({
    leafmap

})
output$precip_plot<-
    renderHighchart({avg_precip_uga %>%
    highcharter::hchart(type = "line",
                        hcaes(x = date, y = precip_avg)) %>%
            hc_title(text=glue::glue("Average {input$env_opts} Across Uganda"))})

# add leaflet proxy so map changes with NDVI vs precip



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
