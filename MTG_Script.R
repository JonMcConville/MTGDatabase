#Loading Libraries ----

library(dplyr)
library(tidyverse)
library(stringdist)
library(stringr)
library(reshape2)
library(tidyr)
library(wesanderson)


##Reading in Data ----

setwd("C:/SQLd/mtg/mtg")
cards <- read.csv("cards.csv")
ghired <-read.csv("C:/SQLd/mtg/mtg/Decks/Ghired.csv")
dina <-read.csv("C:/SQLd/mtg/mtg/Decks/Dina.csv")
Lathril <-read.csv("C:/SQLd/mtg/mtg/Decks/lathril.csv")
Titania <-read.csv("C:/SQLd/mtg/mtg/Decks/Titania.csv")
Magda <-read.csv("C:/SQLd/mtg/mtg/Decks/Magda.csv")
Mizzix <-read.csv("C:/SQLd/mtg/mtg/Decks/Mizzix.csv")
Liesa <-read.csv("C:/SQLd/mtg/mtg/Decks/Liesa.csv")


ghired$commander_deck <-c("Ghired")
dina$commander_deck <-c("Dina")
Lathril$commander_deck <-c("Lathril")
Titania$commander_deck <-c("Titania")
Magda$commander_deck <-c("Magda")
Mizzix$commander_deck <-c("Mizzix")
Liesa$commander_deck <-c("Liesa")

MasterFrame <- rbind(ghired, dina, Lathril, Titania, Magda, Mizzix, Liesa)



TestCheck <-read.delim("C:/SQLd/mtg/mtg/Downloaded Decks/Lathril-Elven Army.txt", header = FALSE) #TODO Check what this is

##Processing Files ----

cards_slim <- cards %>%
  
  select(index, id, colorIdentity, colorIndicator, colors,
         convertedManaCost, edhrecRank, edhrecSaltiness, faceConvertedManaCost, faceFlavorName, faceManaValue, faceName,
         hand, hasAlternativeDeckLimit, hasContentWarning,
         isAlternative,
         isReserved, isStarter, isTimeshifted, keywords,
         leadershipSkills, life, loyalty, manaCost, manaValue, mcmId,
         mcmMetaId,
         name, originalPrintings, originalReleaseDate, originalText, originalType, otherFaceIds, power,
         printings, rarity,
         subtypes, supertypes,
         text, toughness, type, types,
         watermark) %>%
  arrange(desc(id)) %>%
  distinct(name, .keep_all = TRUE)


##Splitting Mana Cost column into Constituent Mana Costs

GreenMana = str_count(cards_slim$manaCost,"\\{G\\}")
BlackMana = str_count(cards_slim$manaCost,"\\{B\\}")
BlueMana = str_count(cards_slim$manaCost,"\\{U\\}")
WhiteMana = str_count(cards_slim$manaCost,"\\{W\\}")
RedMana = str_count(cards_slim$manaCost,"\\{R\\}")
GreenBlackMana = str_count(cards_slim$manaCost,"\\{G/B\\}") + str_count(cards_slim$manaCost,"\\{B/G\\}")
GreenBlueMana = str_count(cards_slim$manaCost,"\\{G/U\\}") + str_count(cards_slim$manaCost,"\\{U/G\\}")
GreenWhiteMana = str_count(cards_slim$manaCost,"\\{G/W\\}") + str_count(cards_slim$manaCost,"\\{W/G\\}")
GreenRedMana = str_count(cards_slim$manaCost,"\\{G/R\\}") + str_count(cards_slim$manaCost,"\\{R/G\\}")
BlackBlueMana = str_count(cards_slim$manaCost,"\\{B/U\\}") + str_count(cards_slim$manaCost,"\\{U/B\\}")
BlackWhiteMana = str_count(cards_slim$manaCost,"\\{B/W\\}") + str_count(cards_slim$manaCost,"\\{W/B\\}")
BlackRedMana = str_count(cards_slim$manaCost,"\\{B/R\\}") + str_count(cards_slim$manaCost,"\\{R/B\\}")
BlueWhiteMana = str_count(cards_slim$manaCost,"\\{R/W\\}") + str_count(cards_slim$manaCost,"\\{W/U\\}")
BlueRedMana = str_count(cards_slim$manaCost,"\\{U/R\\}") + str_count(cards_slim$manaCost,"\\{R/U\\}")
WhiteRedMana = str_count(cards_slim$manaCost,"\\{W/R\\}") + str_count(cards_slim$manaCost,"\\{R/W\\}")

ColourlessMana = str_count(cards_slim$manaCost,"\\{1\\}") + str_count(cards_slim$manaCost,"\\{2\\}")*2 + str_count(cards_slim$manaCost,"\\{3\\}")*3 + str_count(cards_slim$manaCost,"\\{4\\}")*4 + 
  str_count(cards_slim$manaCost,"\\{5\\}")*5 + str_count(cards_slim$manaCost,"\\{6\\}")*6 + str_count(cards_slim$manaCost,"\\{7\\}")*7 + str_count(cards_slim$manaCost,"\\{8\\}")*8 + 
  str_count(cards_slim$manaCost,"\\{9\\}")*9 + str_count(cards_slim$manaCost,"\\{10\\}")*10 + str_count(cards_slim$manaCost,"\\{11\\}")*11 + str_count(cards_slim$manaCost,"\\{12\\}")*12 + 
  str_count(cards_slim$manaCost,"\\{13\\}")*13 + str_count(cards_slim$manaCost,"\\{14\\}")*14 + str_count(cards_slim$manaCost,"\\{15\\}")*15 + str_count(cards_slim$manaCost,"\\{16\\}")*16

ManaDiscrep = cards_slim$faceManaValue - GreenMana - BlueMana - BlackMana - WhiteMana - RedMana - ColourlessMana - GreenBlackMana - GreenBlueMana - GreenWhiteMana - GreenRedMana - BlackBlueMana -
  BlackWhiteMana - BlackRedMana - BlueWhiteMana - BlueRedMana - WhiteRedMana

cards_slim <- cbind(cards_slim, GreenMana)
cards_slim <- cbind(cards_slim, BlackMana)
cards_slim <- cbind(cards_slim, BlueMana)
cards_slim <- cbind(cards_slim, WhiteMana)
cards_slim <- cbind(cards_slim, RedMana)
cards_slim <- cbind(cards_slim, ColourlessMana)
cards_slim <- cbind(cards_slim, ManaDiscrep)
cards_slim <- cbind(cards_slim, GreenBlackMana)
cards_slim <- cbind(cards_slim, GreenBlueMana)
cards_slim <- cbind(cards_slim, GreenWhiteMana)
cards_slim <- cbind(cards_slim, GreenRedMana)
cards_slim <- cbind(cards_slim, BlackBlueMana)
cards_slim <- cbind(cards_slim, BlackWhiteMana)
cards_slim <- cbind(cards_slim, BlackRedMana)
cards_slim <- cbind(cards_slim, BlueWhiteMana)
cards_slim <- cbind(cards_slim, BlueRedMana)
cards_slim <- cbind(cards_slim, WhiteRedMana)


##Processing Ghired Deck ----

Ghired_Data <- merge(ghired, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Ghired_Data %>%
  select(card_name, manaCost, GreenMana, WhiteMana, RedMana, GreenWhiteMana, GreenRedMana, WhiteRedMana, ColourlessMana, types) %>%
  arrange (types)


##Processing Dina Deck ----

Dina_Data <- merge(dina, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Dina_Data %>%
  select(card_name, manaCost, types) %>%
  arrange(types, card_name) %>%
  count (types)

##Processing Lathril Deck ----

Lathril_Data <- merge(Lathril, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Lathril_Data %>%
  select(card_name, manaCost, types) %>%
  arrange(types, card_name) %>%
  count (types)


##Processing Titania Deck ----

Titania_Data <- merge(Titania, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Titania_Data %>%
  select(card_name, manaCost, types) %>%
  arrange(types, card_name) %>%
  count (types)


##Processing Magda Deck ----

Magda_Data <- merge(Magda, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Magda_Data %>%
  select(card_name, manaCost, types) %>%
  arrange(types, card_name) %>%
  count (types)


##Processing Mizzix Deck ----

Mizzix_Data <- merge(Mizzix, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Mizzix_Data %>%
  select(card_name, manaCost, types) %>%
  arrange(types, card_name) %>%
  count (types)


##Processing Liesa Deck ----

Liesa_Data <- merge(Liesa, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Liesa_Data %>%
  select(card_name, manaCost, types, manaValue) %>%
  arrange(types, card_name) %>%
  arrange(manaValue) %>%
  count (types)


##Processing Master Frame ----

Master_Data <- merge(MasterFrame, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Master_Data %>%
  select(card_name, manaCost, types, manaValue) %>%
  arrange(types, card_name) %>%
  arrange(manaValue) %>%
  count (types)


Master_Data_Landless <- Master_Data %>%
  filter(types != "Land") %>%
  arrange(manaValue)



#Names of Decks ----

Salt_Score <- data.frame( Deck_Name = c("Ghired", "Dina", "Lathril", "Titania", "Mizzix", "Liesa", "Magda"),
                          SaltScore = c(mean(Ghired_Data$edhrecSaltiness[Ghired_Data$types != "Land"], na.rm = TRUE),
                                        mean(Dina_Data$edhrecSaltiness[Dina_Data$types != "Land"], na.rm = TRUE),
                                        mean(Lathril_Data$edhrecSaltiness[Lathril_Data$types != "Land"], na.rm = TRUE),
                                        mean(Titania_Data$edhrecSaltiness[Titania_Data$types != "Land"], na.rm = TRUE),
                                        mean(Mizzix_Data$edhrecSaltiness[Mizzix_Data$types != "Land"], na.rm = TRUE),
                                        mean(Liesa_Data$edhrecSaltiness[Liesa_Data$types != "Land"], na.rm = TRUE),
                                        mean(Magda_Data$edhrecSaltiness[Magda_Data$types != "Land"], na.rm = TRUE)))


Salt_Score <- data.frame( Deck_Name = c("Ghired", "Dina", "Lathril", "Titania", "Mizzix", "Liesa", "Magda"),
                          SaltScore = c(sum(Ghired_Data$edhrecSaltiness[Ghired_Data$types != "Land"], na.rm = TRUE),
                                        sum(Dina_Data$edhrecSaltiness[Dina_Data$types != "Land"], na.rm = TRUE),
                                        sum(Lathril_Data$edhrecSaltiness[Lathril_Data$types != "Land"], na.rm = TRUE),
                                        sum(Titania_Data$edhrecSaltiness[Titania_Data$types != "Land"], na.rm = TRUE),
                                        sum(Mizzix_Data$edhrecSaltiness[Mizzix_Data$types != "Land"], na.rm = TRUE),
                                        sum(Liesa_Data$edhrecSaltiness[Liesa_Data$types != "Land"], na.rm = TRUE),
                                        sum(Magda_Data$edhrecSaltiness[Magda_Data$types != "Land"], na.rm = TRUE)))



barplot(Salt_Score$SaltScore, names.arg = Salt_Score$Deck_Name, xlab = "Deck", ylab = "Saltiness", col = "green")


Mana_Score <- data.frame( Deck_Name = c("Ghired", "Dina", "Lathril", "Titania", "Mizzix", "Liesa", "Magda"),
                          ManaScore = c(mean(Ghired_Data$manaValue[Ghired_Data$types != "Land"], na.rm = TRUE),
                                        mean(Dina_Data$manaValue[Dina_Data$types != "Land"], na.rm = TRUE),
                                        mean(Lathril_Data$manaValue[Lathril_Data$types != "Land"], na.rm = TRUE),
                                        mean(Titania_Data$manaValue[Titania_Data$types != "Land"], na.rm = TRUE),
                                        mean(Mizzix_Data$manaValue[Mizzix_Data$types != "Land"], na.rm = TRUE),
                                        mean(Liesa_Data$manaValue[Liesa_Data$types != "Land"], na.rm = TRUE),
                                        mean(Magda_Data$manaValue[Magda_Data$types != "Land"], na.rm = TRUE)))

Mana_Score2 <- Master_Data %>%
                  filter(types != "Land") %>%
                  group_by(commander_deck) %>%
                  summarise_at(vars(manaValue), list(name = mean))


ggplot(Mana_Score,aes(x = Deck_Name, y = ManaScore)) +
  geom_boxplot() +
  coord_flip()


##Better Plots produced using master_Data frames ----

ggplot(Master_Data_Landless,aes(x = commander_deck, y = manaValue, fill = commander_deck)) +
  geom_boxplot() +
  coord_flip() +
  stat_summary(fun = mean, geom = "point", col = "red") +
  stat_summary(fun = mean, geom = "text", col = "red", vjust = 1.5, aes(label = paste("Mean:", round(after_stat(y), digits = 1)))) +
  ylim(0, 12)

ggplot(Master_Data, aes(x = types, fill=types)) +
  geom_bar() +
  facet_wrap(~ commander_deck) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




## Check cards that approximately match wordstring ----

agrep("Marble Talisman", cards_slim$name, max.distance = .25, value = TRUE)


y <- colsplit(TestCheck," ",c("Number of Card","CardName"))
tail(y)

mat <- do.call("rbind", strsplit(sub(" ", ";", TestCheck), ";"))
colnames(mat) <- c("MANUF", "MAKE")


df <- data.frame(Location = c("Number of Card", "CardName"))
df_new <- separate(TestCheck, V1, into = c("Number of Card", "Card Name"), sep = "^\\S*\\K\\s+")



##Trying to work out Mana costs

Card_Check <- cards %>%
  filter(name == "Harmonize")



length(grep("G", Lathril_Data$manaCost))


Lathril_Data$manaCost <- str_replace_all(Lathril_Data$manaCost, "G", "")

str_count(Lathril_Data$manaCost, "{G}")



#Old Stuff



foreign_data %>%
  select(language) %>%
  distinct(language)

rm(cards2)

cards <- read.csv("cards.csv")

ghired <-read.csv("~/Decks/Ghired.csv")


cards %>%
  nrow()

colnames(cards)


refcards <- cards %>%
  select(id, colorIdentity, colors, keywords, loyalty, manaCost, manaValue, name, originalText, originalType, power, rarity, setCode, subset, subtypes, supertypes, text, toughness, types)


Greencards <- refcards %>%
  filter(colors == "G")