
jackknifeServer <- function(id, nav_page){
  moduleServer(id, function(input, output, session) {
    
    output$map = renderLeaflet({
      basemap |> 
        addPolygons(data = enr_bb, 
                    fillOpacity = 0,
                    weight = 2, 
                    opacity = 1, 
                    color = "blue", 
                    label = ~ENR) |> 
        addLegend("bottomright", pal = cp_jack, values = breaks_jack,
                  title = "Bias (%)", opacity = 1)
    })
    
    observe({
      req(nav_page() == "Segmentation")
      leafletProxy("map") |>
        clearGroup("points") |> 
        addCircleMarkers(data = filter(jackknife_station, 
                                       period == input$period & masterCode == input$param), 
                         popup = ~paste0("ENR: ", ENR, 
                                         "<br>WBID: ", WBID, 
                                         "<br>Station: ", dropped_sta,
                                         "<br>Years: ", round(n, 0),
                                         "<br>Bias: ", round(pbias, 2), "%", 
                                         "<br>SE Bias: ", round(se_pbias, 2), "%"), 
                         label = ~dropped_sta, 
                         radius = 6,
                         color = "black",
                         weight = 1,
                         opacity = 1,
                         fillColor = ~cp_jack(pbias),
                         fillOpacity = 0.8,
                         group = "points")
    })
    
  })
}
