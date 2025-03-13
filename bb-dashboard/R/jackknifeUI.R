
jackknifeUI <- function(id){
  ns = NS(id)
  layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("period"), "Period", periods, "New"),
      selectInput(ns("param"), "Parameter", params_jack, "TN")
    ),
    leafletOutput(ns("map"))
  )
}


