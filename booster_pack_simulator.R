library(tidyverse)

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
  filter(supertypes != "Basic")


fdnreturn <- bind_rows(lapply(seq_len(pack_number), function(pack_id) {
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
    mutate(pack_id = pack_id) %>%
    select(name, rarity, type, colorIdentity, manaCost, borderColor, boosterTypes, power, toughness, pack_id)
}))

ifelse(runif(1) < 0.015, "SPG", "common")


fdn_contents%>%
  filter(rarity == "common")
