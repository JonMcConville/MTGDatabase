library(shiny)
library(formattable)
library(scales)

ui <- fluidPage(
  titlePanel("Three-Color Gradient Heatmap with formattable"),
  formattableOutput("heatmap_table")
)

server <- function(input, output, session) {
  # Sample data: Using the first 10 rows of mtcars dataset
  data <- mtcars[1:10, 1:5]
  
  # Function to create a formatter for a three-color gradient
  gradient_formatter <- function(x) {
    rng <- range(x, na.rm = TRUE)
    mid <- median(x, na.rm = TRUE)
    
    # Apply color gradients
    formatter("span",
              style = x ~ style(
                display = "block",
                padding = "5px",
                `border-radius` = "4px",
                `background-color` = csscolor(
                  gradient_n_pal(c("green", "yellow", "red"))(
                    scales::rescale(x, to = c(0, 1), from = rng)
                  )
                )
              )
    )
  }
  
  output$heatmap_table <- renderFormattable({
    formattable(data, list(
      mpg = gradient_formatter(data$mpg),
      cyl = gradient_formatter(data$cyl),
      disp = gradient_formatter(data$disp),
      hp = gradient_formatter(data$hp),
      drat = gradient_formatter(data$drat)
    ))
  })
}

shinyApp(ui, server)
