
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
      filter(bb_nnc, ENR == input$enr)
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
        filter(enr == input$enr & year >= input$years[1] & year <= input$years[2]) |> 
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
        select(ENR = enr, Year = year, masterCode, geo_mean) |> 
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
