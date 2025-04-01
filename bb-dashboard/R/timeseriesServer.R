
timeseriesServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    enrSub <- reactive({
      filter(enr_bb, ENR == input$enr)
    })
    
    stations <- reactive({
      out = stations_ts |> 
        filter(ENR == input$enr & Year >= input$years[1] & Year <= input$years[2]) 
      
      if (nrow(out) > 0) {
        out = out |> 
          group_by(ENR, Station) |> 
          summarise(across(c("TP", "TN", "CHLAC"), ~ sum(.x))) |> 
          filter(TP > 0 | TN > 0 | CHLAC > 0) |> 
          mutate(Popup = paste0("ENR: ", ENR, 
                                "<br>Station: ", Station,
                                "<br><strong>Number of Years</strong>",
                                "<br>Total Phosphorous: ", TP,
                                "<br>Total Nitrogen: ", TN, 
                                "<br>Chlorophyll a: ", CHLAC))
      }
      
      out
    })
    
    output$map = renderLeaflet({
      basemap |> 
        addPolygons(data = enr_bb,
                    weight = 2,
                    opacity = 1,
                    color = "blue",
                    fillOpacity = 0,
                    label = ~ENR,
                    layerId = ~ENR_base,
                    group = "base")
    })
    
    observeEvent(input$map_shape_click, {
      req(input$map_shape_click$id)
      sel = strsplit(input$map_shape_click$id, " ")[[1]][1]
      updateSelectInput(session, "enr", selected = sel)
    })
    
    proxy <- leafletProxy("map")
    
    observe({
      proxy |>
        clearGroup("selected") |> 
        clearGroup("stations") |> 
        addPolygons(data = enrSub(),
                    weight = 3,
                    opacity = 1,
                    color = "yellow",
                    fillOpacity = 0,
                    label = ~ENR,
                    layerId = ~ENR,
                    group = "selected")
      
      if (input$show_stations){
        req(nrow(stations()) > 0)
        proxy |> 
          addCircleMarkers(data = left_join(stations_bb, stations(),
                                            by = join_by(Station, ENR)) |> 
                             filter(!is.na(Popup)), 
                           popup = ~Popup,
                           label = ~Station, 
                           radius = 4,
                           color = "black",
                           weight = 1,
                           opacity = 1,
                           fillColor = "white",
                           fillOpacity = 0.8,
                           group = "stations")
      }
    })
    
    reflineSub <- reactive({
      refline |> 
        filter(ENR == input$enr) |> 
        crossing(data.frame(year = seq(input$years[1] - 1, 
                                       input$years[2] + 1, 
                                       by = 1))) |> 
        mutate(tooltip_text = paste("Reference:", refline))
    })
    
    logmeansSub <- reactive({
      logmeans |> 
        filter(ENR == input$enr & year >= input$years[1] & year <= input$years[2]) |> 
        mutate(tooltip_text = paste0("Year: ", year, "<br>",
                                     "Geo. Avg.: ", round(geo_mean, 3)))
    })
    
    output$plot <- renderPlotly({
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
    
    table <- reactive({
      logmeansSub() |> 
        mutate(geo_mean = round(geo_mean, 5)) |> 
        select(ENR, Year = year, masterCode, geo_mean) |> 
        pivot_wider(names_from = masterCode, values_from = geo_mean)
    })
    
    output$table <- DT::renderDataTable({
      table()
    })
    
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste0("GeometricAverage_", input$enr, "_", input$years[1], "-", input$years[2], ".csv")
      },
      content = function(file) {
        write.csv(table(), file, row.names = FALSE)
      }
    )
    
  })
}
