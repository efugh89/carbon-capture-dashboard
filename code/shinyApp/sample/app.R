library(shiny)
library(bslib)

ui <- page_fillable(
  layout_columns( 
    card( 
      card_header("Card 1 header"),
      p("Card 1 body"),
      sliderInput("slider", "Slider", 0, 10, 5),
    ), 
    card( 
      card_header("Card 2 header"),
      p("Card 2 body"),
      textInput("text", "Add text", ""),
    ), 
  ) 
)

server <- function(input, output) {
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)