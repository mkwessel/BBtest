
timeseriesServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    output$map = renderLeaflet({
      basemap |> 
        addPolygons(data = bb_nnc,
                    weight = 1,
                    opacity = 1,
                    color = "blue",
                    fillOpacity = 0.1,
                    label = ~WBID,
                    layerId = ~WBID_base,
                    group = "base")
    })
    
    observeEvent(input$map_shape_click, {
      if (input$map_shape_click$group == "base"){
        tmp = sub("Base ", "", input$map_shape_click$id)
      } else {
        tmp = input$map_shape_click$id
      }
      updateSelectInput(session, "wbid", selected = tmp)
    })
    
    observe({
      leafletProxy("map") |>
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
      req(input$wbid != "")
      filter(bb_nnc, WBID == input$wbid)
    })
    
    reflineSub <- reactive({
      req(input$wbid != "")
      refline |> 
        filter(wbid == input$wbid) |> 
        crossing(data.frame(year = seq(input$years[1] - 1, 
                                       input$years[2] + 1, 
                                       by = 1))) |> 
        mutate(tooltip_text = paste("Reference:", refline))
    })
    
    logmeansSub <- reactive({
      req(input$wbid != "")
      logmeans |> 
        filter(wbid == input$wbid & year >= input$years[1] & year <= input$years[2]) |> 
        mutate(tooltip_text = paste0("Year: ", year, "<br>",
                                     "Geo. Avg.: ", round(geo_mean, 3)))
    })
    
    output$plot <- renderPlotly({
      validate(need(input$wbid != "", "Click on a WBID polygon on the map to view plots."))
      p = ggplot(logmeansSub()) +
        geom_col(aes(x = year, y = geo_mean, text = tooltip_text), fill="blue") +
        geom_line(data = reflineSub(), aes(x = year, y = refline, text = tooltip_text), 
                  linetype = "dashed", color = "red") +
        labs(x = "Year", y = "Geometric Average (µg/L)") +
        scale_x_continuous(expand = expansion(add = c(0.3, 0.3))) +
        facet_wrap(~ param, ncol = 1, scales = "free_y") +
        theme_minimal() +
        theme(strip.text = element_text(size = 12))
      
      ggplotly(p, tooltip = "text") |> 
        layout(margin = list(l = 50), hovermode = "x") 
    })
    
  })
}
