cards <- read.csv("DataFiles/cards.csv")
cardPrices <- read.csv("DataFiles/cardPrices.csv")
legalities <- read.csv("DataFiles/cardsLegalities.csv")

CMM <- cards %>%
  filter(setCode == "CMM")%>%
  filter(number <= 699)%>%
  filter(nchar(number) != 4)%>%
#  filter(is.na(isOversized))%>%
#  filter(is.na(isPromo))%>%
  select(rarity, name)%>%
  arrange(name)%>%
  distinct(name, .keep_all = TRUE)


class(cards$number)

CMMPrice <- merge(cardPrices %>%
                    filter(priceProvider == "cardmarket")%>%
                    filter(cardFinish == "normal"), 
                  cards %>%
                    select(uuid, name),
                  by.x = 'uuid', by.y = 'uuid', all.x =TRUE)

CMMComplete <- merge(CMM, CMMPrice,  by.x = 'name', by.y = 'name', all.x =TRUE)%>%
  select(name, rarity, price) %>%
  arrange(price)%>%
  distinct(name, .keep_all = TRUE)


CMMComplete %>%
  select(rarity, price)%>%
  group_by(rarity)%>%
  summarise(median(price, na.rm = TRUE))

CMMComplete %>%
  filter(rarity == "mythic") %>%
  arrange(desc(price))

Checklist5 <- replicate(10000, {

rbind(sample_n(CMMComplete %>%
  filter(rarity == "rare"),57),
sample_n(CMMComplete %>%
  filter(rarity == "mythic"), 8))%>%
  summarise(Price = sum(price,na.rm=TRUE))
})




Checklist4 <- tibble(Checklist5)
Checklist6 <- as.numeric(unlist(Checklist4$Checklist5))

hist(Checklist6*0.86,breaks=40)

boxplot(Checklist6*0.86) +
  geom_boxplot() +
  coord_flip()

min(Checklist6)*0.86
max(Checklist6)*0.86
median(Checklist6)*0.86
mean(Checklist6)*0.86
quantile(Checklist6)*0.86


hist(Checklist5)

