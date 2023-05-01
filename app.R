library(shiny)
library(dbplyr)

ui<- fluidPage(
  
  selectInput(inputId="select", label = h3("Select Deck"), 
              choices = MasterFrame %>% filter(commander == "Y") %>%
                select(card_name), 
              selected = 1),
  
  hr(),

  titlePanel(title = "Creatures"),
  dataTableOutput("DeckCreatures"),
  
  titlePanel(title = "Sorceries"),
  dataTableOutput("Sorcery"),
  
  titlePanel(title = "Instants"),
  dataTableOutput("Instant"),
  
  titlePanel(title = "Artifacts"),
  dataTableOutput("Artifact"),
  
  titlePanel(title = "Enchantments"),
  dataTableOutput("Enchantment"),
  
  titlePanel(title = "Planeswalkers"),
  dataTableOutput("Planeswalker"),
  
  titlePanel(title = "Lands"),
  dataTableOutput("Land")
  
  
  )


server <- function(input, output) {
  

  output$DeckCreatures <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Creature" | types =="Artifact,Creature" | types =="Enchantment,Creature" | types =="Land,Creature") %>%
      select(card_name, manaCost, manaValue) %>%
      group_by(card_name, manaCost, manaValue) %>%
      count(card_name)%>%
      arrange(manaValue)%>%
      select(card_name, manaCost, n)}) 
  
  output$Sorcery <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Sorcery" | types =="Tribal,Sorcery") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)})
  
  output$Instant <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Instant") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)})
  
  output$Artifact <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Artifact") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)})
  
  output$Enchantment <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Enchantment" | types == "Tribal,Enchantment") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)})
  
  output$Planeswalker <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Planeswalker") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)})
  
  output$Land <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Land") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)})
  
  
  
}

shinyApp(ui = ui, server =server)