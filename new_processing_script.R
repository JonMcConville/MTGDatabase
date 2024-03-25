# Load data files ----

cards <- read.csv("DataFiles/cards.csv")
sets <- read.csv("DataFiles/sets.csv")
cardPrices <- read.csv("DataFiles/cardPrices.csv")



Ghired <-read.csv("Decks/Ghired.csv")
Dina <-read.csv("Decks/Dina.csv")
Lathril <-read.csv("Decks/lathril.csv")
Titania <-read.csv("Decks/Titania.csv")
Magda <-read.csv("Decks/Magda.csv")
Mizzix <-read.csv("Decks/Mizzix.csv")
Liesa <-read.csv("Decks/Liesa.csv")
Lorescale <-read.csv("Decks/Lorescale.csv")
Dina_upgrade <-read.csv("Decks/Dina_Upgrade.csv")
DinaV2 <-read.csv("Decks/Dina_v2.csv")
Rats <-read.csv("Decks/Rats.csv")

#Data Cleanse and add markers ----

sets$releaseDate <- ymd(sets$releaseDate)
colnames(sets)[colnames(sets) == 'name'] <- 'setName'

Ghired$commander_deck <-c("Ghired, Conclave Exile")
Dina$commander_deck <-c("Dina, Soul Steeper")
Lathril$commander_deck <-c("Lathril, Blade of the Elves")
Titania$commander_deck <-c("Titania, Protector of Argoth")
Magda$commander_deck <-c("Magda, Brazen Outlaw")
Mizzix$commander_deck <-c("Mizzix of the Izmagnus")
Liesa$commander_deck <-c("Liesa, Shroud of Dusk")
Lorescale$commander_deck <-c("Lorescale Coatl")
Dina_upgrade$commander_deck <-c("Dina_upgrade")
DinaV2$commander_deck <-c("DinaV2")
Rats$commander_deck <-c("Rats")

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

MasterFrame <- rbind(Ghired, Dina, Lathril, Titania, Magda, Mizzix, Liesa, Lorescale,Dina_upgrade, DinaV2, Rats)

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
  

