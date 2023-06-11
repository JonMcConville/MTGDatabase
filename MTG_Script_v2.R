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

##Reading in Data ----

cards <- read.csv("cards.csv")
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
Rats <- read.csv("Decks/Rats.csv")


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

MasterFrame <- rbind(Ghired, Dina, Lathril, Titania, Magda, Mizzix, Liesa, Lorescale,Dina_upgrade, DinaV2, Rats)


#colourGuilds <- c()

## TestCheck <-read.delim("C:/SQLd/mtg/mtg/Downloaded Decks/Lathril-Elven Army.txt", header = FALSE) #TODO Check what this is

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
         printings, rarity, side,
         subtypes, supertypes,
         text, toughness, type, types,
         watermark) %>%
  filter(side != "b") %>%
  arrange(desc(id)) %>%
  distinct(name, .keep_all = TRUE)


##Splitting Mana Cost column into Constituent Mana Costs

cards_slim <- cbind(cards_slim, GreenMana = str_count(cards_slim$manaCost,"\\{G\\}"))
cards_slim <- cbind(cards_slim, BlackMana = str_count(cards_slim$manaCost,"\\{B\\}"))
cards_slim <- cbind(cards_slim, BlueMana = str_count(cards_slim$manaCost,"\\{U\\}"))
cards_slim <- cbind(cards_slim, WhiteMana = str_count(cards_slim$manaCost,"\\{W\\}"))
cards_slim <- cbind(cards_slim, RedMana = str_count(cards_slim$manaCost,"\\{R\\}"))
cards_slim <- cbind(cards_slim, GreenBlackMana = str_count(cards_slim$manaCost,"\\{G/B\\}") + str_count(cards_slim$manaCost,"\\{B/G\\}"))
cards_slim <- cbind(cards_slim, GreenBlueMana = str_count(cards_slim$manaCost,"\\{G/U\\}") + str_count(cards_slim$manaCost,"\\{U/G\\}"))
cards_slim <- cbind(cards_slim, GreenWhiteMana = str_count(cards_slim$manaCost,"\\{G/W\\}") + str_count(cards_slim$manaCost,"\\{W/G\\}"))
cards_slim <- cbind(cards_slim, GreenRedMana = str_count(cards_slim$manaCost,"\\{G/R\\}") + str_count(cards_slim$manaCost,"\\{R/G\\}"))
cards_slim <- cbind(cards_slim, BlackBlueMana = str_count(cards_slim$manaCost,"\\{B/U\\}") + str_count(cards_slim$manaCost,"\\{U/B\\}"))
cards_slim <- cbind(cards_slim, BlackWhiteMana = str_count(cards_slim$manaCost,"\\{B/W\\}") + str_count(cards_slim$manaCost,"\\{W/B\\}"))
cards_slim <- cbind(cards_slim, BlackRedMana = str_count(cards_slim$manaCost,"\\{B/R\\}") + str_count(cards_slim$manaCost,"\\{R/B\\}"))
cards_slim <- cbind(cards_slim, BlueWhiteMana = str_count(cards_slim$manaCost,"\\{R/W\\}") + str_count(cards_slim$manaCost,"\\{W/U\\}"))
cards_slim <- cbind(cards_slim, BlueRedMana = str_count(cards_slim$manaCost,"\\{U/R\\}") + str_count(cards_slim$manaCost,"\\{R/U\\}"))
cards_slim <- cbind(cards_slim, WhiteRedMana = str_count(cards_slim$manaCost,"\\{W/R\\}") + str_count(cards_slim$manaCost,"\\{R/W\\}"))
cards_slim <- cbind(cards_slim, GreenPhyrexian = str_count(cards_slim$manaCost,"\\{G/P\\}") + str_count(cards_slim$manaCost,"\\{P/G\\}"))
cards_slim <- cbind(cards_slim, BlackPhyrexian = str_count(cards_slim$manaCost,"\\{B/P\\}") + str_count(cards_slim$manaCost,"\\{P/B\\}"))
cards_slim <- cbind(cards_slim, BluePhyrexian = str_count(cards_slim$manaCost,"\\{U/P\\}") + str_count(cards_slim$manaCost,"\\{P/U\\}"))
cards_slim <- cbind(cards_slim, WhitePhyrexian = str_count(cards_slim$manaCost,"\\{W/P\\}") + str_count(cards_slim$manaCost,"\\{P/W\\}"))
cards_slim <- cbind(cards_slim, RedPhyrexian = str_count(cards_slim$manaCost,"\\{R/P\\}") + str_count(cards_slim$manaCost,"\\{P/R\\}"))
cards_slim <- cbind(cards_slim, GreenWhitePhyrexian = str_count(cards_slim$manaCost,"\\{G/W/P\\}") + str_count(cards_slim$manaCost,"\\{W/G/P\\}"))
cards_slim <- cbind(cards_slim, GreenRedPhyrexian = str_count(cards_slim$manaCost,"\\{G/R/P\\}") + str_count(cards_slim$manaCost,"\\{R/G/P\\}"))
cards_slim <- cbind(cards_slim, WhiteRedPhyrexian = str_count(cards_slim$manaCost,"\\{W/R/P\\}") + str_count(cards_slim$manaCost,"\\{R/W/P\\}"))
cards_slim <- cbind(cards_slim, GreenTwo = str_count(cards_slim$manaCost,"\\{G/2\\}") + str_count(cards_slim$manaCost,"\\{2/G\\}"))
cards_slim <- cbind(cards_slim, BlackTwo = str_count(cards_slim$manaCost,"\\{B/2\\}") + str_count(cards_slim$manaCost,"\\{2/B\\}"))
cards_slim <- cbind(cards_slim, BlueTwo = str_count(cards_slim$manaCost,"\\{U/2\\}") + str_count(cards_slim$manaCost,"\\{2/U\\}"))
cards_slim <- cbind(cards_slim, WhiteTwo = str_count(cards_slim$manaCost,"\\{W/2\\}") + str_count(cards_slim$manaCost,"\\{2/W\\}"))
cards_slim <- cbind(cards_slim, RedTwo = str_count(cards_slim$manaCost,"\\{R/2\\}") + str_count(cards_slim$manaCost,"\\{2/R\\}"))


cards_slim <- cbind(cards_slim, ColourlessMana = str_count(cards_slim$manaCost,"\\{1\\}") + str_count(cards_slim$manaCost,"\\{2\\}")*2 + str_count(cards_slim$manaCost,"\\{3\\}")*3 + str_count(cards_slim$manaCost,"\\{4\\}")*4 + 
  str_count(cards_slim$manaCost,"\\{5\\}")*5 + str_count(cards_slim$manaCost,"\\{6\\}")*6 + str_count(cards_slim$manaCost,"\\{7\\}")*7 + str_count(cards_slim$manaCost,"\\{8\\}")*8 + 
  str_count(cards_slim$manaCost,"\\{9\\}")*9 + str_count(cards_slim$manaCost,"\\{10\\}")*10 + str_count(cards_slim$manaCost,"\\{11\\}")*11 + str_count(cards_slim$manaCost,"\\{12\\}")*12 + 
  str_count(cards_slim$manaCost,"\\{13\\}")*13 + str_count(cards_slim$manaCost,"\\{14\\}")*14 + str_count(cards_slim$manaCost,"\\{15\\}")*15 + str_count(cards_slim$manaCost,"\\{16\\}")*16)

cards_slim <- cbind(cards_slim, ManaDiscrep = cards_slim$faceManaValue - cards_slim$GreenMana - cards_slim$BlueMana - cards_slim$BlackMana - cards_slim$WhiteMana - cards_slim$RedMana - cards_slim$ColourlessMana - cards_slim$GreenBlackMana - cards_slim$GreenBlueMana - cards_slim$GreenWhiteMana - cards_slim$GreenRedMana - cards_slim$BlackBlueMana -
                      cards_slim$BlackWhiteMana - cards_slim$BlackRedMana - cards_slim$BlueWhiteMana - cards_slim$BlueRedMana - cards_slim$WhiteRedMana)


Master_Data <- merge(MasterFrame, cards_slim, by.x = 'card_name', by.y = 'name', all.x = TRUE)

Master_Data_Landless <- Master_Data %>%
  filter(types != "Land") %>%
  arrange(manaValue)







## Filter for Deck ----


Master_Data %>%
  filter(commander_deck == "Rats") %>%
#  filter(types!= "Land") %>%
  select(card_name, manaCost, types, manaValue) %>%
  arrange(types, card_name) %>%
  arrange(manaValue) %>%
  count (types)


Master_Data %>%
  filter(commander_deck == "Dina_upgrade") %>%
  select(card_name, edhrecSaltiness)%>%
  arrange(desc(edhrecSaltiness))



##Processing Master Frame ----



Master_Data %>%
  select(card_name, manaCost, types, manaValue) %>%
  arrange(types, card_name) %>%
  arrange(manaValue) %>%
  count (types)

Master_Data %>%
  count (commander_deck)

Master_Data %>%
  select(types) %>%
  group_by(types) %>%
  filter(types == "Creature" | types == "Artifact,Creature") %>%
  count(types)
  


#Names of Decks

Salt_Score <- data.frame( Deck_Name = c("Ghired", "Dina", "Lathril", "Titania", "Mizzix", "Liesa", "Magda"),
                          SaltScore = c(mean(Ghired_Data$edhrecSaltiness[Ghired_Data$types != "Land"], na.rm = TRUE),
                                        mean(Dina_Data$edhrecSaltiness[Dina_Data$types != "Land"], na.rm = TRUE),
                                        mean(Lathril_Data$edhrecSaltiness[Lathril_Data$types != "Land"], na.rm = TRUE),
                                        mean(Titania_Data$edhrecSaltiness[Titania_Data$types != "Land"], na.rm = TRUE),
                                        mean(Mizzix_Data$edhrecSaltiness[Mizzix_Data$types != "Land"], na.rm = TRUE),
                                        mean(Liesa_Data$edhrecSaltiness[Liesa_Data$types != "Land"], na.rm = TRUE),
                                        mean(Magda_Data$edhrecSaltiness[Magda_Data$types != "Land"], na.rm = TRUE),
                                        mean(Lorescale_Data$edhrecSaltiness[Lorescale_Data$types != "Land"], na.rm = TRUE)))


barplot(Salt_Score$SaltScore, names.arg = Salt_Score$Deck_Name, xlab = "Deck", ylab = "Saltiness", col = "green")


Mana_Score <- data.frame( Deck_Name = c("Ghired", "Dina", "Lathril", "Titania", "Mizzix", "Liesa", "Magda"),
                          ManaScore = c(mean(Ghired_Data$manaValue[Ghired_Data$types != "Land"], na.rm = TRUE),
                                        mean(Dina_Data$manaValue[Dina_Data$types != "Land"], na.rm = TRUE),
                                        mean(Lathril_Data$manaValue[Lathril_Data$types != "Land"], na.rm = TRUE),
                                        mean(Titania_Data$manaValue[Titania_Data$types != "Land"], na.rm = TRUE),
                                        mean(Mizzix_Data$manaValue[Mizzix_Data$types != "Land"], na.rm = TRUE),
                                        mean(Liesa_Data$manaValue[Liesa_Data$types != "Land"], na.rm = TRUE),
                                        mean(Magda_Data$manaValue[Magda_Data$types != "Land"], na.rm = TRUE),
                                        mean(Lorecale_Data$manaValue[Lorescale_Data$types != "Land"], na.rm = TRUE)))

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
  stat_summary(fun = mean, geom = "text", col = "red", vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) +
  ylim(0, 12)

ggplot(Master_Data, aes(x = types, fill=types)) +
  geom_bar() +
  facet_wrap(~ commander_deck) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




ggplot( Master_Data %>% 
          filter(types!= "Land") %>%
  count(commander_deck, types, manaValue), 
  aes(manaValue, n, fill = types)) +
  geom_bar(stat="identity") +
  facet_wrap(~ commander_deck)


ggplot( data.frame( ManaColour = c("Green", "Black", "Blue", "White", "Red","Colourless"),
                    Amount = c(sum(Master_Data$GreenMana[Master_Data$commander_deck=="Dina, Soul Steeper"]/sum(Master_Data$manaValue[Master_Data$commander_deck=="Dina, Soul Steeper"]), na.rm = TRUE),
                               sum(Master_Data$BlackMana[Master_Data$commander_deck=="Dina, Soul Steeper"]/sum(Master_Data$manaValue[Master_Data$commander_deck=="Dina, Soul Steeper"]), na.rm = TRUE),
                               sum(Master_Data$BlueMana[Master_Data$commander_deck=="Dina, Soul Steeper"]/sum(Master_Data$manaValue[Master_Data$commander_deck=="Dina, Soul Steeper"]), na.rm = TRUE),
                               sum(Master_Data$WhiteMana[Master_Data$commander_deck=="Dina, Soul Steeper"]/sum(Master_Data$manaValue[Master_Data$commander_deck=="Dina, Soul Steeper"]), na.rm = TRUE),
                               sum(Master_Data$RedMana[Master_Data$commander_deck=="Dina, Soul Steeper"]/sum(Master_Data$manaValue[Master_Data$commander_deck=="Dina, Soul Steeper"]), na.rm = TRUE),
                               sum(Master_Data$ColourlessMana[Master_Data$commander_deck=="Dina, Soul Steeper"]/sum(Master_Data$manaValue[Master_Data$commander_deck=="Dina, Soul Steeper"]), na.rm = TRUE)
                               )),
        
        aes(x="", y= Amount, fill = ManaColour)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Amount*100), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#333333", "#2F48AA", "#999999", "#55AA55", "#F21919", "#DDDDDD")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Mana Division") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))







## Check cards that approximately match wordstring ----

agrep("Okiba Reckoner Raid", cards_slim$name, max.distance = .25, value = TRUE)

everything_pivot <- qhpvt(cards_slim,"colorIdentity","types","n()")

colnames(everything_pivot)[colnames(everything_pivot) %in% c("")] <- c("color")

ggplot(everything_pivot,aes(x = Deck_Name, y = ManaScore)) +
  geom_boxplot() +
  coord_flip()


qhpvt(Master_Data_Landless, "commander_deck", "manaValue", "n()")


write.csv(
merge(Dina_upgrade %>% 
        filter(!Dina_upgrade$card_name %in% Dina$card_name), 
      cards_slim, 
      by.x = 'card_name', 
      by.y = 'name', 
      all.x = TRUE) %>%
  arrange(types, card_name),
  "Cards_to_add.csv")

write.csv(
merge(Dina %>% 
        filter(!Dina$card_name %in% Dina_upgrade$card_name), 
      cards_slim, 
      by.x = 'card_name', 
      by.y = 'name', 
      all.x = TRUE) %>%
  arrange(types, card_name),
  "Cards_to_cut.csv")


Dina_upgrade %>% 
  filter(!Dina_upgrade$card_name %in% Dina$card_name)


Master_Data %>%
         filter(types != "Land")%>%
         select(commander_deck, edhrecSaltiness)%>%
         group_by(commander_deck) %>%
         summarise(SaltScore = mean(edhrecSaltiness, na.rm = TRUE))