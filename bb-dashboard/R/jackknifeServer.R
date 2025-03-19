
jackknifeServer <- function(id, nav_page){
  moduleServer(id, function(input, output, session) {
    
    output$map = renderLeaflet({
      basemap |> 
        addPolygons(data = bb_nnc, 
                    fillColor = "white",  
                    fillOpacity = 0.5,
                    weight = 1, 
                    opacity = 1, 
                    color = "blue", 
                    label = ~ENR) |> 
        addLegend("bottomright", pal = cp_jack, values = breaks_jack,
                  title = "Bias (%)", opacity = 1)
    })
    
    observe({
      req(nav_page() == "Jackknife")
      leafletProxy("map") |>
        addCircleMarkers(data = filter(jackknife_station, 
                                       period == input$period & masterCode == input$param), 
                         popup = ~paste0("Station: ", dropped_sta,
                                         "<br>Years: ", round(n,0),
                                         "<br>Bias: ", round(pbias, 2), "%", 
                                         "<br>SE Bias: ", round(se_pbias, 2), "%"), 
                         label = ~dropped_sta, 
                         radius = 3, 
                         color = ~cp_jack(pbias),
                         opacity = 0.8,
                         fillOpacity = 0.8)
    })
    
  })
}
