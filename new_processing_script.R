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




# Load data files ----

cards <- read.csv("C:/MTGDataFiles/CardandPricesData/cards.csv")
cardPrices <- read.csv("C:/MTGDataFiles/CardandPricesData/cardPrices.csv")
legalities <- read.csv("C:/MTGDataFiles/CardandPricesData/cardsLegalities.csv")
sets <- read.csv("C:/MTGDataFiles/CardandPricesData/sets.csv")

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


#Data Cleanse and add markers ----

#sets$releaseDate <- ymd(sets$releaseDate)
#colnames(sets)[colnames(sets) == 'name'] <- 'setName'


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

#Merge Datasets ----


ConsolidatedData <- merge(cards, sets, by.x = 'setCode', by.y = 'code', all.x = TRUE)

cards_slim <- ConsolidatedData %>%
  filter(side != "b") %>%
  arrange(desc(releaseDate)) %>%
  distinct(name, .keep_all = TRUE)


Master_Data <- merge(MasterFrame, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Master_Data_Landless <- Master_Data %>%
  filter(types != "Land") %>%
  arrange(manaValue)

Master_Data_Prices <- merge(Master_Data, cardPrices [filter(priceProvider == "tcgplayer")], by.x = 'uuid', by.y = 'uuid', all.x = TRUE)

Price_Check <- Master_Data_Prices %>%
  select(commander_deck, name, price) %>%
  filter(commander_deck == "DinaV2")
  group_by(commander_deck) %>%
  summarise(sum(price, na.rm = TRUE))

  
cardPrices %>% pivot_wider(names_from = c(priceProvider), values_from = price)  


cardPrices %>%
  select(priceProvider)%>%
  distinct(priceProvider) %>%
  select(uuid)
  
Amsterdam_Artists <- c("Magali Villeneuve", "Leon Tukker", "Kieran Yanner", "Jason Rainville", "Aurore Folny", "Milivoj Ceran", "Mathias Kollros", "Anna Steinbauer", "Alayna Danner")

Artist_Check <- merge(cards %>%
  select(artist, name, setCode, printings, rarity, edhrecSaltiness) %>%
    filter(rarity == "rare" | rarity == "mythic" | rarity == "special" | rarity == "bonus")%>%
  filter(artist %in% Amsterdam_Artists),
  setsdl%>%
    select(setCode = code, setName = name, releaseDate), 
  by = 'setCode', all.x = TRUE)%>%
  group_by(artist)%>%
  arrange(desc(edhrecSaltiness))%>%
  distinct(name, .keep_all = TRUE)


merge(cards%>%
  filter(rarity == "special")%>%
  select(name, setCode),
  setsdl%>%
    select(setCode = code, setName = name, releaseDate), 
  by = 'setCode', all.x = TRUE)
  




