library(dplyr)
library(tidyverse)
library(stringdist)
library(stringr)
library(reshape2)
library(tidyr)
library(wesanderson)
library(pivottabler)
library(shiny)
library(shinydashboard)

cards <- read.csv("DataFiles/cards.csv")
cardPrices <- read.csv("DataFiles/cardPrices.csv")
legalities <- read.csv("DataFiles/cardsLegalities.csv")

Lathril <-read.csv("Decks/Lathril_0623.csv")
Magda <- read.csv("Decks/Magda_0624.csv")
Titania <- read.csv("Decks/Titania_230625.csv")
Dina <- read.csv("Decks/Dina_290623.csv")
Doran <- read.csv("Decks/Doran_290623.csv")
Sakashima <- read.csv("Decks/Sakashima_020723.csv")
Liesa <- read.csv("Decks/Liesa_020723.csv")
Ghired <- read.csv("Decks/Ghired_020723.csv")
Ashcoat <- read.csv("Decks/Ashcoat_020723.csv")
Lorescale <- read.csv("Decks/Lorescale_020723.csv")
Mizzix <- read.csv("Decks/Mizzix_040723.csv")
Anikthea <- read.csv("Decks/Anikthea_230723.csv")



Lathril$commander_deck <-c("Lathril, Blade of the Elves")
Magda$commander_deck <- c("Magda, Brazen Outlaw")
Titania$commander_deck <- c("Titania, Protector of Argoth")
Dina$commander_deck <- c("Dina, Soul Steeper")
Doran$commander_deck <- c("Doran, the Siege Tower")
Sakashima$commander_deck <- c("Sakashima the Impostor")
Liesa$commander_deck <- c("Liesa, Shroud of Dusk")
Ghired$commander_deck <- c("Ghired, Conclave Exile")
Ashcoat$commander_deck <- c("Ashcoat of the Shadow Swarm")
Lorescale$commander_deck <- c("Lorescale Coatl")
Mizzix$commander_deck <- c("Mizzix of the Izmagnus")
Anikthea$commander_deck <- c("Anikthea, Hand of Erebos")

MasterFrame <- rbind(Lathril,Magda,Titania,Dina,Doran,Sakashima,Liesa,Ghired,Ashcoat,Lorescale,Mizzix,Anikthea)


## TestCheck <-read.delim("C:/SQLd/mtg/mtg/Downloaded Decks/Lathril-Elven Army.txt", header = FALSE) #TODO Check what this is

##Processing Files ----

##Splitting Mana Cost column into Constituent Mana Costs

cards <- cbind(cards, GreenMana = str_count(cards$manaCost,"\\{G\\}"))
cards <- cbind(cards, BlackMana = str_count(cards$manaCost,"\\{B\\}"))
cards <- cbind(cards, BlueMana = str_count(cards$manaCost,"\\{U\\}"))
cards <- cbind(cards, WhiteMana = str_count(cards$manaCost,"\\{W\\}"))
cards <- cbind(cards, RedMana = str_count(cards$manaCost,"\\{R\\}"))
cards <- cbind(cards, GreenBlackMana = str_count(cards$manaCost,"\\{G/B\\}") + str_count(cards$manaCost,"\\{B/G\\}"))
cards <- cbind(cards, GreenBlueMana = str_count(cards$manaCost,"\\{G/U\\}") + str_count(cards$manaCost,"\\{U/G\\}"))
cards <- cbind(cards, GreenWhiteMana = str_count(cards$manaCost,"\\{G/W\\}") + str_count(cards$manaCost,"\\{W/G\\}"))
cards <- cbind(cards, GreenRedMana = str_count(cards$manaCost,"\\{G/R\\}") + str_count(cards$manaCost,"\\{R/G\\}"))
cards <- cbind(cards, BlackBlueMana = str_count(cards$manaCost,"\\{B/U\\}") + str_count(cards$manaCost,"\\{U/B\\}"))
cards <- cbind(cards, BlackWhiteMana = str_count(cards$manaCost,"\\{B/W\\}") + str_count(cards$manaCost,"\\{W/B\\}"))
cards <- cbind(cards, BlackRedMana = str_count(cards$manaCost,"\\{B/R\\}") + str_count(cards$manaCost,"\\{R/B\\}"))
cards <- cbind(cards, BlueWhiteMana = str_count(cards$manaCost,"\\{R/W\\}") + str_count(cards$manaCost,"\\{W/U\\}"))
cards <- cbind(cards, BlueRedMana = str_count(cards$manaCost,"\\{U/R\\}") + str_count(cards$manaCost,"\\{R/U\\}"))
cards <- cbind(cards, WhiteRedMana = str_count(cards$manaCost,"\\{W/R\\}") + str_count(cards$manaCost,"\\{R/W\\}"))
cards <- cbind(cards, GreenPhyrexian = str_count(cards$manaCost,"\\{G/P\\}") + str_count(cards$manaCost,"\\{P/G\\}"))
cards <- cbind(cards, BlackPhyrexian = str_count(cards$manaCost,"\\{B/P\\}") + str_count(cards$manaCost,"\\{P/B\\}"))
cards <- cbind(cards, BluePhyrexian = str_count(cards$manaCost,"\\{U/P\\}") + str_count(cards$manaCost,"\\{P/U\\}"))
cards <- cbind(cards, WhitePhyrexian = str_count(cards$manaCost,"\\{W/P\\}") + str_count(cards$manaCost,"\\{P/W\\}"))
cards <- cbind(cards, RedPhyrexian = str_count(cards$manaCost,"\\{R/P\\}") + str_count(cards$manaCost,"\\{P/R\\}"))
cards <- cbind(cards, GreenWhitePhyrexian = str_count(cards$manaCost,"\\{G/W/P\\}") + str_count(cards$manaCost,"\\{W/G/P\\}"))
cards <- cbind(cards, GreenRedPhyrexian = str_count(cards$manaCost,"\\{G/R/P\\}") + str_count(cards$manaCost,"\\{R/G/P\\}"))
cards <- cbind(cards, WhiteRedPhyrexian = str_count(cards$manaCost,"\\{W/R/P\\}") + str_count(cards$manaCost,"\\{R/W/P\\}"))
cards <- cbind(cards, GreenTwo = str_count(cards$manaCost,"\\{G/2\\}") + str_count(cards$manaCost,"\\{2/G\\}"))
cards <- cbind(cards, BlackTwo = str_count(cards$manaCost,"\\{B/2\\}") + str_count(cards$manaCost,"\\{2/B\\}"))
cards <- cbind(cards, BlueTwo = str_count(cards$manaCost,"\\{U/2\\}") + str_count(cards$manaCost,"\\{2/U\\}"))
cards <- cbind(cards, WhiteTwo = str_count(cards$manaCost,"\\{W/2\\}") + str_count(cards$manaCost,"\\{2/W\\}"))
cards <- cbind(cards, RedTwo = str_count(cards$manaCost,"\\{R/2\\}") + str_count(cards$manaCost,"\\{2/R\\}"))


cards <- cbind(cards, ColourlessMana = str_count(cards$manaCost,"\\{1\\}") + str_count(cards$manaCost,"\\{2\\}")*2 + str_count(cards$manaCost,"\\{3\\}")*3 + str_count(cards$manaCost,"\\{4\\}")*4 + 
                 str_count(cards$manaCost,"\\{5\\}")*5 + str_count(cards$manaCost,"\\{6\\}")*6 + str_count(cards$manaCost,"\\{7\\}")*7 + str_count(cards$manaCost,"\\{8\\}")*8 + 
                 str_count(cards$manaCost,"\\{9\\}")*9 + str_count(cards$manaCost,"\\{10\\}")*10 + str_count(cards$manaCost,"\\{11\\}")*11 + str_count(cards$manaCost,"\\{12\\}")*12 + 
                 str_count(cards$manaCost,"\\{13\\}")*13 + str_count(cards$manaCost,"\\{14\\}")*14 + str_count(cards$manaCost,"\\{15\\}")*15 + str_count(cards$manaCost,"\\{16\\}")*16)

cards <- cbind(cards, ManaDiscrep = cards$faceManaValue - cards$GreenMana - cards$BlueMana - cards$BlackMana - cards$WhiteMana - cards$RedMana - cards$ColourlessMana - cards$GreenBlackMana - cards$GreenBlueMana - cards$GreenWhiteMana - cards$GreenRedMana - cards$BlackBlueMana -
                 cards$BlackWhiteMana - cards$BlackRedMana - cards$BlueWhiteMana - cards$BlueRedMana - cards$WhiteRedMana)


Master_Data <- merge(MasterFrame, (cards%>%filter(side!="b")), by.x = c('cardNumber','setCode'), by.y = c('number','setCode'), all.x = TRUE)
names(legalities)[names(legalities) == 'commander'] <- 'commanderEDH'

Master_Data <- merge(Master_Data, legalities, by.x = 'uuid', by.y = 'uuid')

Master_Data_Landless <- Master_Data %>%
  filter(types != "Land") %>%
  arrange(manaValue)



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
             
             fluidRow(
               
              column(width = 6,

             
             selectInput(inputId="select", label = h3("Select Deck"), 
                         choices = c("Choose Deck ...",MasterFrame %>% filter(commander == "Y") %>%
                           select(commander_deck) %>%
                           arrange(commander_deck))
                         )),
             
             column(width = 6, 
                    
                radioButtons(inputId = "deckLegal", label = h3("Deck Check..."),
                      choices = list("Pauper Commander" = 'paupercommander', "CommanderEDH" = 'commanderEDH'), 
                      selected = 'commanderEDH')
                    
            )),
             
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
              
              column(width = 6)),
             
             
            fluidRow(
            
            column(width = 12,
                   
              titlePanel(title = "Deck Costs - Absolute"),
              dataTableOutput("deckPrice"))                   
                   )


             )
             
    )
    
    
  ),
  
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
#      select(name, manaCost, uuid, edhrecSaltiness, input$deckLegal, name, input$deckLegal)%>%
      group_by(name, manaCost, uuid, edhrecSaltiness))%>%
      count(name) %>%
      select(n, name, manaCost, uuid, edhrecSaltiness), 
    
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
    
    options(timeout = 180)
    
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