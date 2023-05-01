library(shiny)
library(dplyr)

ui<- fluidPage(
  
  titlePanel("Jon's Deck Display"),
  
  
  selectInput(inputId="select", label = h3("Select Deck"), 
              choices = MasterFrame %>% filter(commander == "Y") %>%
                select(commander_deck), 
              selected = 1),
  
  hr(),


  fluidRow(
    column(width = 7,
  
  titlePanel(title = "Commander"),
  dataTableOutput("CommanderCreature"),
           
           
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
          ),
  
  column(width = 5,
         
  plotOutput("Curve") ,
  
  plotOutput("CardTypeDevision"),
  
  plotOutput("DeckAverage"),
  
  plotOutput("SaltScore")
  
  
         
         
         
         
         ))
  )


server <- function(input, output) {
  

  output$CommanderCreature <- renderDataTable ({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(commander =="Y") %>%
      group_by(card_name,manaCost)%>%
      count(card_name) %>%
      select(card_name, manaCost, n)
  })
  
  output$DeckCreatures <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(card_name != input$select) %>%
      filter(types == "Creature" | types =="Artifact,Creature" | types =="Enchantment,Creature" | types =="Land,Creature") %>%
      select(card_name,manaCost,manaValue) %>%
      group_by(card_name, manaCost) %>%
      count(card_name)%>%
      select(card_name, manaCost, n)# %>%
#      arrange(n)
      }) 
  
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
  
  
  output$Curve <- renderPlot({
    ggplot( Master_Data %>% 
              filter(types!= "Land") %>%
              filter(commander_deck == input$select)%>%
              count(commander_deck, types, manaValue), 
            aes(manaValue, n, fill = types)) +
      geom_bar(stat="identity") +
      theme(legend.position = c(0.9, 0.8))
  
  })
    
    
  output$DeckAverage <- renderPlot({
    ggplot(Master_Data_Landless,aes(x = commander_deck, y = manaValue, fill = commander_deck)) +
      geom_boxplot() +
      coord_flip() +
      stat_summary(fun = mean, geom = "point", col = "red") +
      stat_summary(fun = mean, geom = "text", col = "red", vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) +
      ylim(0, 12) +
      theme(legend.position = "none")
    
  })

  
  output$CardTypeDevision <- renderPlot({
    ggplot(Master_Data %>%
             filter(commander_deck == input$select),
           aes(x = types, fill=types)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
  })
  
  output$SaltScore <- renderPlot({
    ggplot(Master_Data %>%
#             filter(types != "Land")%>%
             select(commander_deck, edhrecSaltiness)%>%
             group_by(commander_deck) %>%
             summarise(SaltScore = mean(edhrecSaltiness, na.rm = TRUE)),
           aes(x=commander_deck, y=SaltScore)) +
      geom_bar(stat="identity")
           
  })
    
  
  
}

shinyApp(ui = ui, server =server)