library(shiny)

ui<- fluidPage(
  
  selectInput(inputId = "select", label = h3("Select Deck"), 
              choices = MasterFrame %>% filter(commander == "Y") %>%
                select(card_name), 
              selected = 1),
  
  hr(),

  dataTableOutput("dfHead")
  
  )


server <- function(input, output) {
  
  DeckList <- Master_Data %>%
    filter(commander_deck == "select") %>%
    select(card_name, manaCost, types, manaValue) %>%
    arrange(types, card_name)
  
  output$dfHead <- renderDataTable({DeckList})
  
}

shinyApp(ui = ui, server =server)