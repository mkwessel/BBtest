
jackknifeUI <- function(id){
  ns = NS(id)
  layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("period"), "Period", periods, "2017-2023"),
      selectInput(ns("param"), "Parameter", params_jack, "TN")
    ),
    leafletOutput(ns("map"))
  )
}


