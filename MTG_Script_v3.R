#Loading Libraries ----

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


# Define the folder path you want to check and create if it doesn't exist
CaPData_path <- "C:/MTGDataFiles/CardandPricesData"
Deck_path <- "C:/MTGDataFiles/Decks"

# Check if the folder exists
if (!file.exists(CaPData_path)) {
  # If the folder doesn't exist, create it
  dir.create(CaPData_path, recursive = TRUE)
  cat("Folder created:", CaPData_path, "\n")
} else {
  cat("Folder already exists:", CaPData_path, "\n")
}

# Check if the folder exists
if (!file.exists(Deck_path)) {
  # If the folder doesn't exist, create it
  dir.create(Deck_path, recursive = TRUE)
  cat("Folder created:", Deck_path, "\n")
} else {
  cat("Folder already exists:", Deck_path, "\n")
}



#Updating csv files from online database ----

options(timeout = 180)

cardsdl <- read.csv("https://mtgjson.com/api/v5/csv/cards.csv")
setsdl <- read.csv("https://mtgjson.com/api/v5/csv/sets.csv")
tokensdl <- read.csv("https://mtgjson.com/api/v5/csv/tokens.csv")
cardRulingsdl <- read.csv("https://mtgjson.com/api/v5/csv/cardRulings.csv")
cardPricesdl <- read.csv("https://mtgjson.com/api/v5/csv/cardPrices.csv")
cardLegalitiesdl <- read.csv("https://mtgjson.com/api/v5/csv/cardLegalities.csv")
metadl <- read.csv("https://mtgjson.com/api/v5/csv/meta.csv")

cardPricesdb <- read.csv("C:/MTGDataFiles/CardandPricesData/cardPrices.csv")

cardPricesdb <- cardPricesdb %>%
  anti_join(cardPricesdl, by = c("cardFinish", "currency", "gameAvailability", "priceProvider", "providerListing")) %>%
  bind_rows(cardPricesdl)

write.csv(cardsdl, "C:/MTGDataFiles/CardandPricesData/cards.csv", row.names=FALSE)
write.csv(setsdl, "C:/MTGDataFiles/CardandPricesData/sets.csv", row.names=FALSE)
write.csv(tokensdl, "C:/MTGDataFiles/CardandPricesData/tokens.csv", row.names=FALSE)
write.csv(cardRulingsdl, "C:/MTGDataFiles/CardandPricesData/cardRulings.csv", row.names=FALSE)
write.csv(cardPricesdb, "C:/MTGDataFiles/CardandPricesData/cardPrices.csv", row.names=FALSE)
write.csv(cardLegalitiesdl, "C:/MTGDataFiles/CardandPricesData/cardsLegalities.csv", row.names=FALSE)
write.csv(metadl, "C:/MTGDataFiles/CardandPricesData/meta.csv", row.names=FALSE)




##Reading in Data ----

cards <- read.csv("C:/MTGDataFiles/CardandPricesData/cards.csv")
cardPrices <- read.csv("C:/MTGDataFiles/CardandPricesData/cardPrices.csv")
legalities <- read.csv("C:/MTGDataFiles/CardandPricesData/cardsLegalities.csv")

Lathril <-read.csv("Decks/Lathril_0623.csv")
Magda <- read.csv("Decks/Magda_0624.csv")
Titania <- read.csv("Decks/Titania_230625.csv")
Dina <- read.csv("Decks/Dina_290623.csv")
Doran <- read.csv("Decks/Doran_250224.csv")
Sakashima <- read.csv("Decks/Sakashima_020723.csv")
Liesa <- read.csv("Decks/Liesa_020723.csv")
Ghired <- read.csv("Decks/Ghired_020723.csv")
Ashcoat <- read.csv("Decks/Ashcoat_020723.csv")
Lorescale <- read.csv("Decks/Lorescale_020723.csv")
Mizzix <- read.csv("Decks/Mizzix_040723.csv")
Anikthea <- read.csv("Decks/Anikthea_230723.csv")
CMM <- read.csv("Decks/CMM_040823.csv")
Ghyrson <- read.csv("Decks/Ghyrson_240923.csv")
Marchesa <- read.csv("Decks/Marchesa_091023.csv")
Galadriel <- read.csv("Decks/Galadriel_081123.csv")
Clavileno <- read.csv("Decks/Clavileno_231123.csv")
Rares <- read.csv("Decks/Rares.csv")
Draft <- read.csv("Decks/Draft_120324.csv")
Mycotyrant <- read.csv("Decks/Mycotyrant_130324.csv")



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
CMM$commander_deck <- c("CMM")
Ghyrson$commander_deck <- c("Ghyrson Starn, Kelermorph")
Marchesa$commander_deck <- c("Queen Marchesa")
Galadriel$commander_deck <- c("Galadriel, Light of Valinor")
Clavileno$commander_deck <- c("Clavileño, First of the Blessed")
Rares$commander_deck <- c("Rares")
Draft$commander_deck <- c("Draft Cards")
Mycotyrant$commander_deck <- c("The Mycotyrant")

MasterFrame <- rbind(Lathril,Magda,Titania,Dina,Doran,Sakashima,Ghired,Ashcoat,Lorescale,Anikthea,Ghyrson,Marchesa, Galadriel, Clavileno, Rares, Draft, Mycotyrant)


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


# End of file cleansing process

# Play Area ----

Draft_Card_Text <- Master_Data%>%
  filter(commander_deck == "Draft Cards")%>%
  select(colorIdentity, keywords, manaCost, name, type, text, types, )



Tokens <- Master_Data%>%
  select(name, text)

Tokens <- cbind(Tokens, createtoken = str_count(Tokens$text,"\\{T\\}"))



merge(
  Master_Data %>%
#    filter(commander_deck == input$select) %>%
#    filter(types == "Sorcery" | types =="Tribal,Sorcery") %>%
    select(commander_deck, name, manaCost, manaValue, uuid) %>%
    group_by(commander_deck, name, manaCost, uuid) %>%
    count(name)%>%
    select(commander_deck, n, name, manaCost, uuid),
  
  cardPrices %>%
    filter(priceProvider == "cardkingdom") %>%
    filter(cardFinish == "normal") %>%
    filter(providerListing == "retail"),
  by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
  select(commander_deck, price)%>%
  group_by(commander_deck)%>%
  summarise("Deck Cost" = sum(price, na.rm = TRUE))



Master_Data %>%
  filter(commander_deck == "Doran, the Siege Tower")%>%
  select(cardNumber, setCode, name)



## Filter for Deck ----


ggplot(merge(
  Master_Data %>%
#    filter(commander_deck == input$select) %>%
    filter(name != "Forest") %>%
    filter(name != "Swamp") %>%
    filter(name != "Mountain") %>%
    select(commander_deck, name, manaCost, manaValue, uuid) %>%
    group_by(commander_deck, name, manaCost, uuid) %>%
    count(name)%>%
    select(commander_deck, n, name, manaCost, uuid),
  
  cardPrices %>%
    filter(priceProvider == "cardmarket") %>%
    filter(cardFinish == "normal") %>%
    filter(providerListing == "retail"),
  by.x = 'uuid', by.y = 'uuid', all.x =TRUE),
  aes(x = commander_deck, y = price, fill = commander_deck)) +
  geom_boxplot() +
  coord_flip() +
  stat_summary(fun = mean, geom = "point", col = "red") +
  stat_summary(fun = mean, geom = "text", col = "red", vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) +
  theme(legend.position = "none")


MasterFrame %>% filter(commander == "Y") %>%
  select(commander_deck) %>%
  arrange(commander_deck)

Price_Check <- merge(
  Master_Data %>%
    #    filter(commander_deck == input$select) %>%
#    filter(name != "Forest") %>%
#    filter(name != "Swamp") %>%
#    filter(name != "Mountain") %>%
    select(commander_deck, name, manaCost, manaValue, uuid) %>%
    group_by(commander_deck, name, manaCost, uuid) %>%
    count(name)%>%
    select(commander_deck, n, name, manaCost, uuid),
  
  cardPrices %>%
    filter(priceProvider == "cardmarket") %>%
    filter(cardFinish == "normal") %>%
    filter(providerListing == "retail"),
  by.x = 'uuid', by.y = 'uuid', all.x =TRUE)


Master_Data %>%
  colnames()


merge(
  (Master_Data %>%
     filter(commander_deck == "Ghired, Conclave Exile") %>%
#     filter(commander =="Y") %>%
     group_by(name, manaCost, uuid, edhrecSaltiness, paupercommander)%>%
     count(name) %>%
     select(n, name, manaCost, uuid, edhrecSaltiness, paupercommander)), 
  
  cardPrices %>%
    filter(priceProvider == "cardmarket") %>%
    filter(cardFinish == "normal") %>%
    filter(providerListing == "retail")
  , by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
  select(n, name, manaCost, price, edhrecSaltiness, paupercommander)


colnames(Master_Data)
write.csv(Master_Data,file = "Master_data.csv")


cards %>%
  filter(setCode == "CMM")%>%
  select(rarity, name)%>%
  arrange(name)%>%
  distinct(name, .keep_all = TRUE)%>%
  group_by(rarity)%>%
  summarise(count = n())



merge(Master_Data %>%
  filter(commander_deck == "CMM") %>%
  select(name, manaCost, manaValue, uuid, edhrecSaltiness, rarity) %>%
  group_by(name, manaCost, uuid, edhrecSaltiness, rarity) %>%
  count(name)%>%
  select(n, name, manaCost, uuid, edhrecSaltiness, rarity),

cardPrices %>%
  filter(priceProvider == "cardmarket") %>%
  filter(cardFinish == "normal") %>%
  filter(providerListing == "retail"),
by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
  select(n, name, manaCost, price, edhrecSaltiness, rarity)%>%
  arrange(desc(price))%>%
  filter(rarity == "common" | rarity =="uncommon")%>%
  filter(price>=1.5)


left_join(
  Master_Data%>%
    select(commander_deck)%>%
    count(commander_deck),
  merge(
    Master_Data %>%
      select(commander_deck, name, manaCost, manaValue, uuid) %>%
      group_by(commander_deck, name, manaCost, uuid) %>%
      count(name)%>%
      select(commander_deck, n, name, manaCost, uuid),
    
    cardPrices %>%
      filter(priceProvider == "cardmarket") %>%
      filter(cardFinish == "normal") %>%
      filter(providerListing == "retail"),
    by.x = 'uuid', by.y = 'uuid', all.x =TRUE) %>%
    select(commander_deck, price)%>%
    group_by(commander_deck)%>%
    summarise("Deck Cost" = sum(price, na.rm = TRUE)),
  
  by = 'commander_deck')

check <- Master_Data %>%
  select(commander_deck, types) %>%
  group_by(commander_deck, types)%>%
  count(commander_deck)%>%
  pivot_wider(names_from = types, values_from = n)


cards %>%
  select(types)%>%
  distinct(types)


trueCheck <- TRUE

cards%>%
  if(trueCheck){filter(colorIdentity == "W")}else{filter(colorIdentity != "W")}

if(trueCheck == TRUE){filter(colorIdentity == "W")}else{filter(colorIdentity != "W")}



df <- Master_Data %>%
  filter(commander_deck == "Draft Cards")%>%
  distinct(name, .keep_all = TRUE)%>%
  select(name, manaCost, type, types, power, toughness, text, keywords, colorIdentity) %>%
#  filter(!str_detect(colorIdentity, "U"))%>%
#  filter(!str_detect(colorIdentity, "W"))%>%
#  filter(!str_detect(colorIdentity, "R"))%>%
  group_by(name) %>%
  count(name)%>%
  select(name)

write.table(df, file = "data.txt", sep = "\t", row.names = FALSE)


