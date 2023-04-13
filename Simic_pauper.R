## Cut down databse to simic commons ----

Simic_pauper_cards <- cards %>%
  filter(rarity == "common") %>%
  filter(colorIdentity == "G"| colorIdentity == "U" |colorIdentity == "G,U" | colorIdentity == "U,G" | colorIdentity == "") %>%
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

Draw_cards <- Simic_pauper_cards[grep("draw", Simic_pauper_cards$originalText),]

Plus_cards <- Simic_pauper_cards[grep("\\+", Simic_pauper_cards$originalText),]

Proliferate_cards <- Simic_pauper_cards[grep("Proliferate", Simic_pauper_cards$keywords),]