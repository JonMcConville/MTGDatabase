library(tidyverse)

cards <- read.csv("C:/MTGDataFiles/CardandPricesData/cards.csv")

#Capenna Pack simulator (draft)----

settouse <- "SNC"
pack_number <- 6

cap_contents <- cards %>%
  filter(setCode == "SNC")%>%
  filter(isOnlineOnly != "True")%>%
  filter(boosterTypes == "default")

  
capreturn <- bind_rows(lapply(seq_len(pack_number), function(pack_id) {
  common_cards <- sample_n(cap_contents %>%
                             filter(rarity == "common", supertypes != "Basic"), 10)
  uncommon_cards <- sample_n(cap_contents %>%
                               filter(rarity == "uncommon"), 3)
  rare_card <- sample_n(cap_contents %>%
                             filter(rarity == ifelse(runif(1) < 0.125, "mythic", "rare")), 1)
  
  # Combine all selected cards and add a pack identifier
  bind_rows(common_cards, uncommon_cards, rare_card) %>%
    mutate(pack_id = pack_id) %>%
    select(name, rarity, type, colorIdentity, manaCost, power, toughness, pack_id)
}))

#Foundations Pack Simulator ----

settouse <- "FDN"
pack_number <- 6

fdn_contents <- cards %>%
  filter((setCode == "FDN") | (setCode == "SPG" & originalReleaseDate == '2024-11-15'))%>%
  filter(isOnlineOnly != "True")%>%
  filter(supertypes != "Basic")%>%
  select(name, rarity, type, colorIdentity, manaCost, borderColor, boosterTypes, power, toughness, supertypes, setCode)

fdnrun_list <- vector("list", 10000)

for  (i in seq_len(10000)) {
fdnrun_list[[i]] <- bind_rows(lapply(seq_len(pack_number), function(pack_id) {
  common_cards <- sample_n(fdn_contents %>%
                             filter(rarity == "common", supertypes != "Basic")%>%
                             filter(boosterTypes == "default"), 6)
  common_wild_card <- if (runif(1) < 0.015) {
    sample_n(fdn_contents %>%
               filter(setCode == "SPG"), 1)
  } else {
    sample_n(fdn_contents %>%
               filter(rarity == "common")%>%
               filter(boosterTypes == "default"), 1)
  }
  uncommon_cards <- sample_n(fdn_contents %>%
                               filter(rarity == "uncommon")%>%
                               filter(boosterTypes == "default"), 3)
  prob <- runif(1)
  rare_card <- if (prob < 0.78) {
    sample_n(fdn_contents %>%
               filter(rarity == "rare")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.78 + 0.128) {
    sample_n(fdn_contents %>%
               filter(rarity == "mythic")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.78 + 0.128 + 0.077) {
    sample_n(fdn_contents %>%
               filter(rarity == "rare")%>%
               filter(borderColor == "borderless"), 1)
  } else {
    sample_n(fdn_contents %>%
               filter(rarity == "mythic")%>%
               filter(borderColor == "borderless"), 1)
  }
  prob <- runif(1)
  nf_wild_card <- if (prob < 0.167) {
    sample_n(fdn_contents %>%
               filter(rarity == "common")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.167 + 0.583) {
    sample_n(fdn_contents %>%
               filter(rarity == "uncommon")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163) {
    sample_n(fdn_contents %>%
               filter(rarity == "rare")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163 + 0.026) {
    sample_n(fdn_contents %>%
               filter(rarity == "rare")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163 + 0.026 + 0.018) {
    sample_n(fdn_contents %>%
               filter(rarity == "common")%>%
               filter(borderColor == "borderless"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163 + 0.026 + 0.018 + 0.024) {
    sample_n(fdn_contents %>%
               filter(rarity == "uncommon")%>%
               filter(borderColor == "borderless"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163 + 0.026 + 0.018 + 0.024 + 0.016) {
    sample_n(fdn_contents %>%
               filter(rarity == "rare")%>%
               filter(borderColor == "borderless"), 1)
  } else {
    sample_n(fdn_contents %>%
               filter(rarity == "mythic")%>%
               filter(borderColor == "borderless"), 1)
  }
  
  prob <- runif(1)
  f_wild_card <- if (prob < 0.167) {
    sample_n(fdn_contents %>%
               filter(rarity == "common")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.167 + 0.583) {
    sample_n(fdn_contents %>%
               filter(rarity == "uncommon")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163) {
    sample_n(fdn_contents %>%
               filter(rarity == "rare")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163 + 0.026) {
    sample_n(fdn_contents %>%
               filter(rarity == "rare")%>%
               filter(boosterTypes == "default"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163 + 0.026 + 0.018) {
    sample_n(fdn_contents %>%
               filter(rarity == "common")%>%
               filter(borderColor == "borderless"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163 + 0.026 + 0.018 + 0.024) {
    sample_n(fdn_contents %>%
               filter(rarity == "uncommon")%>%
               filter(borderColor == "borderless"), 1)
  } else if (prob < 0.167 + 0.583 + 0.163 + 0.026 + 0.018 + 0.024 + 0.016) {
    sample_n(fdn_contents %>%
               filter(rarity == "rare")%>%
               filter(borderColor == "borderless"), 1)
  } else {
    sample_n(fdn_contents %>%
               filter(rarity == "mythic")%>%
               filter(borderColor == "borderless"), 1)
  }
  
  
  
  # Combine all selected cards and add a pack identifier
  bind_rows(common_cards, common_wild_card, uncommon_cards, rare_card, nf_wild_card, f_wild_card) %>%
    mutate(pack_id = pack_id, iteration =i) %>%
    select(name, rarity, type, colorIdentity, manaCost, borderColor, boosterTypes, power, toughness, pack_id, iteration)
}))

# Print update every 1000 iterations
if (i %% 1000 == 0) {
  print(paste("Iteration:", i, "of 10000 completed..."))
}

}

fdnreturn_simulation <- bind_rows(fdnrun_list)

write.csv(fdnreturn_simulation,"FDN_Monte.csv", row.names = FALSE)

table(fdnreturn_simulation$rarity) / nrow(fdnreturn_simulation)

table(fdnreturn_simulation$borderColor)

fdnreturn_simulation %>%
  group_by(iteration, rarity) %>%
  summarise(count = n(), .groups = "drop")

fdnreturn_simulation %>%
  count(name)

fdnreturn_simulation %>%
  filter(name == "Angelic Destiny")%>%
  distinct(iteration, .keep_all = TRUE)%>%
  select(iteration, rarity)%>%
  group_by(rarity)%>%
  count(rarity)%>%
  select(rarity, "Average" = n)

fdnreturn_simulation %>%
#  filter(colour_length >= 3) %>%
  filter(grepl("Legendary Creature", type, ignore.case = TRUE))%>%
  distinct(iteration)%>%
  tally()/10000

fdnreturn_simulation %>%
  filter(colour_length >= 3) %>%
  distinct(type)

# fdnreturn_simulation <- fdnreturn_simulation %>%
#  mutate(colour_length = nchar(colorIdentity))

write.csv(cards %>%
  filter((setCode == "FDN") | (setCode == "SPG" & originalReleaseDate == '2024-11-15'))%>%
  filter(isOnlineOnly != "True")%>%
  filter(supertypes != "Basic")%>%
  filter(grepl("Legendary Creature", type, ignore.case = TRUE))%>%
  select(name, colorIdentity, rarity, text)%>%
  distinct(name, .keep_all = TRUE),"Commanders.csv",row.names = FALSE)

write.csv(cards %>%
  filter((setCode == "FDN") | (setCode == "SPG" & originalReleaseDate == '2024-11-15'))%>%
  filter(isOnlineOnly != "True")%>%
  filter(supertypes != "Basic")%>%
  filter(grepl("Exile all|Destroy all|Exile target|Destroy target|Counter target", text, ignore.case = TRUE))%>%
  select(name, colorIdentity, manaCost, rarity, type, text)%>%
  distinct(name, .keep_all = TRUE),"Removal.csv", row.names = FALSE)

test <- cards %>%
  filter((setCode == "FDN") | (setCode == "SPG" & originalReleaseDate == '2024-11-15'))%>%
  filter(isOnlineOnly != "True")%>%
  filter(supertypes != "Basic")%>%
  filter(grepl("Elf", type, ignore.case = TRUE))%>%
  select(name, colorIdentity, rarity, setCode, text)%>%
  distinct(name, .keep_all = TRUE)
