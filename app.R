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
                         choices = c("Choose Deck ...",MasterFrame %>% filter(commander == "Y") %>%
                           select(commander_deck) %>%
                           arrange(commander_deck))#, 
                         #selected = 1
                         ),
             
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
             
             fluidRow(
               column(width = 6,
                      
              titlePanel(title = "Deck Mana Averages"),
              plotOutput("DeckAverage")),
             
             column( width = 6,
             
              titlePanel(title = "Deck Salt"),
              plotOutput("SaltScore"))
             
             ),
             
             fluidRow(
               column(width = 6,
            
                      
              titlePanel(title = "Deck Costs - Distribution"),
              plotOutput("deckCosts")),
            
            column(width = 6,
                   
              titlePanel(title = "Deck Costs - Absolute"),
              dataTableOutput("deckPrice")                   
                   )


             )
             
    )
    
    
  )),
  
  tabItem(tabName = "CardBoard",
          
          fluidRow(
            column(width = 6,
          
          
          textInput("cardLookup", label = h3("Card Name"), value = "")),
          
          column(width = 6,
          
          checkboxGroupInput("manaLookup", label = h3("Mana Colours"), 
                             choices = list("White" = 1, "Blue" = 2, "Black" = 3, "Red" = 4, "Green" =5),
                             selected = 1)))
          
          
          ),
  
  tabItem(tabName = "OptionBoard",
          
          radioButtons("selectProvider", label = h3("Price Provider"),
                       choices = list("Card Hoarder" = "cardhoarder", "TCG Player" = "tcgplayer", "Card Market (default)" = "cardmarket", "Card Kingdom" = "cardkingdom", "Card Sphere" = "cardsphere"), 
                       selected = "cardmarket"),
          
          actionButton("priceUpdate", label = "Update Prices"),
          
          actionButton("dbUpdate", label = "Update Card Database"),
          
          )
  
  
    )
  ))
  

server <- function(input, output) {
  
  
  output$CommanderCreature <- renderDataTable ({ merge(
    (Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(commander =="Y") %>%
      group_by(name,manaCost, uuid, edhrecSaltiness)%>%
      count(name) %>%
      select(n, name, manaCost, uuid, edhrecSaltiness)), 
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail")
      , by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, name, manaCost, price, edhrecSaltiness)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$DeckCreatures <- renderDataTable({ merge(
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(name != input$select) %>%
      filter(types == "Creature" | types =="Artifact, Creature" | types =="Enchantment, Creature" | types =="Land, Creature") %>%
      select(name,manaCost,manaValue, uuid, edhrecSaltiness) %>%
      group_by(name, manaCost, uuid, edhrecSaltiness) %>%
      count(name)%>%
      select(n, name, manaCost, uuid, edhrecSaltiness),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
      by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, name, manaCost, price, edhrecSaltiness)
    
    
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  )) 
  
  output$Sorcery <- renderDataTable({ merge(
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Sorcery" | types =="Tribal,Sorcery") %>%
      select(name, manaCost, manaValue, uuid, edhrecSaltiness) %>%
      group_by(name, manaCost, uuid, edhrecSaltiness) %>%
      count(name)%>%
      select(n, name, manaCost, uuid, edhrecSaltiness),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
    by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
    select(n, name, manaCost, price, edhrecSaltiness)
    
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Instant <- renderDataTable({ merge(
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Instant") %>%
      select(name, manaCost, manaValue, uuid, edhrecSaltiness) %>%
      group_by(name, manaCost, uuid, edhrecSaltiness) %>%
      count(name)%>%
      select(n, name, manaCost, uuid, edhrecSaltiness),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
    by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, name, manaCost, price, edhrecSaltiness)
    
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Artifact <- renderDataTable({ merge(
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Artifact") %>%
      select(name, manaCost, manaValue, uuid, edhrecSaltiness) %>%
      group_by(name, manaCost, uuid, edhrecSaltiness) %>%
      count(name)%>%
      select(n, name, manaCost, uuid, edhrecSaltiness),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
    by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, name, manaCost, price, edhrecSaltiness)
    
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Enchantment <- renderDataTable({ merge(
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Enchantment" | types == "Tribal,Enchantment") %>%
      select(name, manaCost, manaValue, uuid, edhrecSaltiness) %>%
      group_by(name, manaCost, uuid, edhrecSaltiness) %>%
      count(name)%>%
      select(n, name, manaCost, uuid, edhrecSaltiness),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
    by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, name, manaCost, price, edhrecSaltiness)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Planeswalker <- renderDataTable({ merge(
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Planeswalker") %>%
      select(name, manaCost, manaValue, uuid, edhrecSaltiness) %>%
      group_by(name, manaCost, uuid, edhrecSaltiness) %>%
      count(name)%>%
      select(n, name, manaCost, uuid, edhrecSaltiness),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
    by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, name, manaCost, price, edhrecSaltiness)
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '20px', targets = c(0)))
  ))
  
  output$Land <- renderDataTable({ merge(
    Master_Data %>%
      filter(commander_deck == input$select) %>%
      filter(types == "Land") %>%
      select(name, manaCost, manaValue, uuid, edhrecSaltiness) %>%
      group_by(name, manaCost, uuid, edhrecSaltiness) %>%
      count(name)%>%
      select(n, name, manaCost, uuid, edhrecSaltiness),
    
    cardPrices %>%
      filter(priceProvider == input$selectProvider) %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
    by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(n, name, manaCost, price, edhrecSaltiness)
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
  
  output$deckCosts <- renderPlot({
    
    ggplot(merge(
      Master_Data %>%
        #    filter(commander_deck == input$select) %>%
        filter(name != "Forest" | name != "Swamp" | name != "Mountain" | name != "Plains" | name != "Island") %>%
        select(commander_deck, name, manaCost, manaValue, uuid) %>%
        group_by(commander_deck, name, manaCost, uuid) %>%
        count(name)%>%
        select(commander_deck, n, name, manaCost, uuid),
      
      cardPrices %>%
        filter(priceProvider == input$selectProvider) %>%
        filter(cardFinish == "normal") %>%
        filter(providerListing == "retail"),
      by.x = 'uuid', by.y = 'uuid', all.x =TRUE),
      aes(x = commander_deck, y = price, fill = commander_deck)) +
      geom_boxplot() +
      coord_flip() +
      stat_summary(fun = mean, geom = "point", col = "red") +
      stat_summary(fun = mean, geom = "text", col = "red", vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) +
      theme(legend.position = "none") +
      xlab("Deck Name") + 
      ylab("Card Costs")
    
  })
  
  
  output$deckPrice <- renderDataTable({
    merge(
      Master_Data %>%
        select(commander_deck, name, manaCost, manaValue, uuid) %>%
        group_by(commander_deck, name, manaCost, uuid) %>%
        count(name)%>%
        select(commander_deck, n, name, manaCost, uuid),
      
      cardPrices %>%
        filter(priceProvider == input$selectProvider) %>%
        filter(cardFinish == "normal") %>%
        filter(providerListing == "retail"),
      by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
      select(commander_deck, price)%>%
      group_by(commander_deck)%>%
      summarise("Deck Cost" = sum(price, na.rm = TRUE))
    
    
  })
  
  {observeEvent(input$priceUpdate,{
    
    cardPricesdl <- read.csv("https://mtgjson.com/api/v5/csv/cardPrices.csv")
    write.csv(cardPricesdl, "DataFiles/cardPrices.csv", row.names=FALSE)
    cardPrices <- read.csv("DataFiles/cardPrices.csv")
    
  })}
  
  {observeEvent(input$dbUpdate,{
    
    cardsdl <- read.csv("https://mtgjson.com/api/v5/csv/cards.csv")
    setsdl <- read.csv("https://mtgjson.com/api/v5/csv/sets.csv")
    tokensdl <- read.csv("https://mtgjson.com/api/v5/csv/tokens.csv")
    cardRulingsdl <- read.csv("https://mtgjson.com/api/v5/csv/cardRulings.csv")
    cardLegalitiesdl <- read.csv("https://mtgjson.com/api/v5/csv/cardLegalities.csv")
    metadl <- read.csv("https://mtgjson.com/api/v5/csv/meta.csv")
    
    write.csv(cardsdl, "DataFiles/cards.csv", row.names=FALSE)
    write.csv(setsdl, "DataFiles/sets.csv", row.names=FALSE)
    write.csv(tokensdl, "DataFiles/tokens.csv", row.names=FALSE)
    write.csv(cardRulingsdl, "DataFiles/cardRulings.csv", row.names=FALSE)
    write.csv(cardLegalitiesdl, "DataFiles/cardsLegalities.csv", row.names=FALSE)
    write.csv(metadl, "Datafiles/meta.csv", row.names=FALSE)
    
    ##Reading in Data ----
    
    cards <- read.csv("DataFiles/cards.csv")
    
  })}  

  
  
}

shinyApp(ui = ui, server =server)