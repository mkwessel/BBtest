
page_fillable(
  title = "Biscayne Bay",
  layout_columns(
    col_widths = c(4, 8),
    card(
      full_screen = TRUE,
      card_header("Map"),
      leafletOutput("map")
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Plots",
        popover(
          bsicons::bs_icon("gear", title = "Settings"),
          title = "Settings",
          sliderInput(inputId = "years", label = "Years", min = year_min, 
                      max = year_max, value = c(year_min, year_max), sep = "")
        )
      ),
      plotlyOutput("plot")
    )
  )
)

