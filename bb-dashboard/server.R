
function(input, output, session) {
  
  rv <- reactiveValues(wbid = NULL)
  
  output$map = renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      setView(lng = -80.231, lat = 25.61, zoom = 10) |>
      addProviderTiles(providers$Esri.WorldTopoMap) |> 
      addPolygons(data = bb_nnc,
                  weight = 1,
                  opacity = 1,
                  color = "blue",
                  fillOpacity = 0.1,
                  label = ~WBID,
                  layerId = ~WBID_base,
                  group = "base")
  })
  
  proxy <- leafletProxy("map")
  
  observeEvent(input$map_shape_click, {
    if (input$map_shape_click$group == "base"){
      rv$wbid = sub("Base ", "", input$map_shape_click$id)
    } else {
      rv$wbid = input$map_shape_click$id
    }
  })
  
  observe({
    proxy |>
      clearGroup("selected") |> 
      addPolygons(data = nncSub(),
                  weight = 1,
                  opacity = 1,
                  color = "blue",
                  fillColor = "yellow",
                  fillOpacity = 0.8,
                  label = ~WBID,
                  layerId = ~WBID,
                  group = "selected")
  })
  
  nncSub <- reactive({
    req(rv$wbid)
    filter(bb_nnc, WBID == rv$wbid)
  })
  
  reflineSub <- reactive({
    req(rv$wbid)
    filter(refline, wbid == rv$wbid)
  })
  
  logmeansSub <- reactive({
    req(rv$wbid)
    filter(logmeans, wbid == rv$wbid & year >= input$years[1] & year <= input$years[2])
  })
  
  output$plot <- renderPlotly({
    validate(need(!is.null(rv$wbid), "Click on a WBID polygon on map to view plots."))
    
    p = ggplot(logmeansSub()) +
      geom_col(aes(x = year, y = geo_mean), fill="blue") +
      geom_hline(data = reflineSub(), aes(yintercept = refline), 
                 linetype = "dashed", color = "red") +
      labs(x = "Year", y = "Geometric Average") +
      facet_wrap(~ param, ncol = 1, scales = "free_y") +
      theme_minimal() +
      theme(strip.text = element_text(size = 12))
    
    ggplotly(p)
  })
  
}
