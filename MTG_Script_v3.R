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

#Updating csv files from online database ----

cardsdl <- read.csv("https://mtgjson.com/api/v5/csv/cards.csv")
setsdl <- read.csv("https://mtgjson.com/api/v5/csv/sets.csv")
tokensdl <- read.csv("https://mtgjson.com/api/v5/csv/tokens.csv")
cardRulingsdl <- read.csv("https://mtgjson.com/api/v5/csv/cardRulings.csv")
cardPricesdl <- read.csv("https://mtgjson.com/api/v5/csv/cardPrices.csv")
cardLegalitiesdl <- read.csv("https://mtgjson.com/api/v5/csv/cardLegalities.csv")
metadl <- read.csv("https://mtgjson.com/api/v5/csv/meta.csv")

write.csv(cardsdl, "DataFiles/cards.csv", row.names=FALSE)
write.csv(setsdl, "DataFiles/sets.csv", row.names=FALSE)
write.csv(tokensdl, "DataFiles/tokens.csv", row.names=FALSE)
write.csv(cardRulingsdl, "DataFiles/cardRulings.csv", row.names=FALSE)
write.csv(cardPricesdl, "DataFiles/cardPrices.csv", row.names=FALSE)
write.csv(cardLegalitiesdl, "DataFiles/cardsLegalities.csv", row.names=FALSE)
write.csv(metadl, "Datafiles/meta.csv", row.names=FALSE)




##Reading in Data ----

cards <- read.csv("DataFiles/cards.csv")
cardPrices <- read.csv("DataFiles/cardPrices.csv")

Lathril <-read.csv("Decks/Lathril_0623.csv")
Magda <- read.csv("Decks/Magda_0624.csv")
Titania <- read.csv("Decks/Titania_230625.csv")



Lathril$commander_deck <-c("Lathril, Blade of the Elves")
Magda$commander_deck <- c("Magda, Brazen Outlaw")
Titania$commander_deck <- c("Titania, Protector of Argoth")


MasterFrame <- rbind(Lathril,Magda,Titania)




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

Master_Data_Landless <- Master_Data %>%
  filter(types != "Land") %>%
  arrange(manaValue)


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


