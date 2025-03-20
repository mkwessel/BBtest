
timeseriesServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    output$map = renderLeaflet({
      basemap |> 
        addPolygons(data = bb_nnc,
                    weight = 1,
                    opacity = 1,
                    color = "blue",
                    fillOpacity = 0.1,
                    label = ~ENR,
                    layerId = ~ENR_WBID_base,
                    group = "base")
    })
    
    observeEvent(input$map_shape_click, {
      req(input$map_shape_click$id)
      sel = strsplit(input$map_shape_click$id, " ")[[1]][1]
      updateSelectInput(session, "enr", selected = sel)
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
                    label = ~ENR,
                    layerId = ~ENR_WBID,
                    group = "selected")
    })
    
    nncSub <- reactive({
      req(input$enr != "")
      filter(bb_nnc, ENR == input$enr)
    })
    
    reflineSub <- reactive({
      req(input$enr != "")
      refline |> 
        filter(ENR == input$enr) |> 
        crossing(data.frame(year = seq(input$years[1] - 1, 
                                       input$years[2] + 1, 
                                       by = 1))) |> 
        mutate(tooltip_text = paste("Reference:", refline))
    })
    
    logmeansSub <- reactive({
      req(input$enr != "")
      logmeans |> 
        filter(enr == input$enr & year >= input$years[1] & year <= input$years[2]) |> 
        mutate(tooltip_text = paste0("Year: ", year, "<br>",
                                     "Geo. Avg.: ", round(geo_mean, 3)))
    })
    
    output$plot <- renderPlotly({
      validate(need(input$enr != "", "Click on a ENR polygon on the map to view plots."))
      p = ggplot(logmeansSub()) +
        geom_col(aes(x = year, y = geo_mean, text = tooltip_text), fill="blue") +
        geom_line(data = reflineSub(), aes(x = year, y = refline, text = tooltip_text), 
                  linetype = "dashed", color = "red") +
        labs(x = "Year", y = "Geometric Average") +
        scale_x_continuous(expand = expansion(add = c(0.3, 0.3))) +
        facet_wrap(~ param, ncol = 1, scales = "free_y") +
        theme_minimal() +
        theme(strip.text = element_text(size = 12))
      
      ggplotly(p, tooltip = "text") |> 
        layout(margin = list(l = 50), hovermode = "x") 
    })
    
  })
}
