
timeseriesUI <- function(id){
  ns = NS(id)
  layout_sidebar(
    sidebar = sidebar(
      selectInput(inputId = ns("enr"), label = "ENR", choices = enrs),
      sliderInput(inputId = ns("years"), label = "Years", min = year_min,
                  max = year_max, value = c(1995, year_max), sep = ""),
      conditionalPanel(
        condition = 'input.nav_card == "Table"',
        downloadButton(ns("downloadTable"), "Download Table", icon = icon("download"))
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      navset_card_underline(
        nav_panel(
          title = "Map",
          leafletOutput(ns("map"))
        )
      ),
      navset_card_underline(
        id = "nav_card",
        nav_panel(
          title = "Plots", 
          plotlyOutput(ns("plot"))
        ),
        nav_panel(
          title = "Table",
          DT::dataTableOutput(ns("table"))
        )
      )
    )
  )
}
