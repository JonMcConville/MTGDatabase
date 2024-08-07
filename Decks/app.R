library(shiny)
library(dplyr)
library(shinydashboard)


ui<- dashboardPage(
  
  dashboardHeader(title = "Jon's Deck Display"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Deck Information", tabName = "DeckBoard"),
      menuItem(text = "Card Lookup", tabName = "CardBoard"),
      menuItem(text = "Options", tabName = "OptionBoard")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "DeckBoard",
              
              
              
              tabsetPanel(
                tabPanel(title = "Decks",
                         
                         
                         
                         selectInput(inputId="select", label = h3("Select Deck"), 
                                     choices = MasterFrame %>% filter(commander == "Y") %>%
                                       select(commander_deck) %>%
                                       arrange(commander_deck), 
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
                                  
                                  plotOutput("ManaDistrib"),
                                  
                                  plotOutput("Curve") ,
                                  
                                  plotOutput("CardTypeDevision")
                                  
                                  
                                  
                           ))),
                tabPanel(title = "Overview", 
                         
                         plotOutput("DeckAverage"),
                         
                         plotOutput("SaltScore")
                         
                         
                )
                
                
                
                
                
                
              )),
      
      tabItem(tabName = "CardBoard"),
      
      tabItem(tabName = "OptionBoard",
              
              radioButtons("selectProvider", label = h3("Price Provider"),
                           choices = list("Card Hoarder" = "cardhoarder", "TCG Player" = "tcgplayer", "Card Market (default)" = "cardmarket", "Card Kingdom" = "cardkingdom", "Card Sphere" = "cardsphere"), 
                           selected = "cardmarket")
              
      )
      
      
    )
  ))





server <- function(input, output) {
  
#Filtering Data for tables ----  
  
  
  output$Deck <-  renderDataTable ({ merge(
    (Master_Data %>%
       filter(commander_deck == input$select) %>%
       group_by(card_name, commander, types, manaCost, uuid)%>%
       count(card_name) %>%
       select(n, card_name, commander, types, manaCost, uuid)),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
      by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, card_name, manaCost, price)
  
  })
    

#Tables for Deck Page ----  
    
  
  output$CommanderCreature <- renderDataTable ({ merge(
    (output$Deck %>%
       filter(commander =="Y") %>%
       group_by(card_name,manaCost, uuid)%>%
       count(card_name) %>%
       select(n, card_name, manaCost, uuid)), 
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail")
    , by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, card_name, manaCost, price)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$DeckCreatures <- renderDataTable({ merge(
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(card_name != input$select) %>%
      filter(types == "Creature" | types =="Artifact,Creature" | types =="Enchantment,Creature" | types =="Land,Creature") %>%
      select(card_name,manaCost,manaValue, uuid) %>%
      group_by(card_name, manaCost, uuid) %>%
      count(card_name)%>%
      select(n, card_name, manaCost, uuid),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail")
    , by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, card_name, manaCost, price)
    
    
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  )) 
  
  output$Sorcery <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Sorcery" | types =="Tribal,Sorcery") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)%>%
      select(n, card_name, manaCost)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Instant <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Instant") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)%>%
      select(n, card_name, manaCost)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Artifact <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Artifact") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)%>%
      select(n, card_name, manaCost)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Enchantment <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Enchantment" | types == "Tribal,Enchantment") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)%>%
      select(n, card_name, manaCost)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Planeswalker <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Planeswalker") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)%>%
      select(n, card_name, manaCost)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Land <- renderDataTable({
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Land") %>%
      select(card_name, manaCost) %>%
      arrange(card_name)%>%
      group_by(card_name, manaCost) %>%
      count(card_name)%>%
      select(n, card_name, manaCost)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  
  output$ManaDistrib <- renderPlot({
    ggplot( data.frame( ManaColour = c("Green", "Black", "Blue", "White", "Red","Colourless"),
                        Amount = c(sum(Master_Data$GreenMana[Master_Data$commander_deck==input$select]/sum(Master_Data$manaValue[Master_Data$commander_deck==input$select]), na.rm = TRUE),
                                   sum(Master_Data$BlackMana[Master_Data$commander_deck==input$select]/sum(Master_Data$manaValue[Master_Data$commander_deck==input$select]), na.rm = TRUE),
                                   sum(Master_Data$BlueMana[Master_Data$commander_deck==input$select]/sum(Master_Data$manaValue[Master_Data$commander_deck==input$select]), na.rm = TRUE),
                                   sum(Master_Data$WhiteMana[Master_Data$commander_deck==input$select]/sum(Master_Data$manaValue[Master_Data$commander_deck==input$select]), na.rm = TRUE),
                                   sum(Master_Data$RedMana[Master_Data$commander_deck==input$select]/sum(Master_Data$manaValue[Master_Data$commander_deck==input$select]), na.rm = TRUE),
                                   sum(Master_Data$ColourlessMana[Master_Data$commander_deck==input$select]/sum(Master_Data$manaValue[Master_Data$commander_deck==input$select]), na.rm = TRUE)
                        )),
            
            aes(x="", y= Amount, fill = ManaColour)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) + 
      geom_text(aes(label = paste0(round(Amount*100), "%")), position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values=c("#333333", "#1468AB", "#999999", "#00733E", "#D33242", "#F8E7B9")) +
      labs(x = NULL, y = NULL, fill = NULL, title = "Mana Division") +
      theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#666666"))
    
  })
  
  
  
  
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
           aes(x=commander_deck, y=SaltScore, fill = commander_deck)) +
      geom_bar(stat="identity") +
      coord_flip() +
      guides(fill="none")
    
  })
  
  
  
}

shinyApp(ui = ui, server =server)