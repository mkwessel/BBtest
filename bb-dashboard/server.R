
function(input, output, session) {
  
  nav_page <- reactive({ input$nav_page })
  
  timeseriesServer("ts")
  
  jackknifeServer("jack", nav_page)
  
}
