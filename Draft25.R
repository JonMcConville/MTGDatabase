Draft25 <- Master_Data %>%
  filter(commander_deck == "Draft 25") %>%
#  filter(grepl("Vehicle | Saddle", text, ignore.case = TRUE))%>%
#  filter(grepl("Destroy | Exile", text, ignore.case = TRUE))%>%
  filter(grepl("Creature", types, ignore.case = TRUE))%>%
#  filter(!grepl("W", colorIdentity, ignore.case = TRUE))%>%
#  filter(!grepl("U", colorIdentity, ignore.case = TRUE))%>%
#  filter(!grepl("B", colorIdentity, ignore.case = TRUE))%>%
#  filter(!grepl("R", colorIdentity, ignore.case = TRUE))%>%
#  filter(!grepl("G", colorIdentity, ignore.case = TRUE))%>%
  select(name, colorIdentity, manaCost, text, type, types, subtypes, edhrecSaltiness)%>%
  distinct(name, .keep_all = TRUE)

summary_df <- data.frame(
  White = sum(!grepl("U|B|R|G", Draft25$colorIdentity)),
  Blue = sum(!grepl("W|B|R|G", Draft25$colorIdentity)),
  Black = sum(!grepl("W|U|R|G", Draft25$colorIdentity)),
  Red = sum(!grepl("W|U|B|G", Draft25$colorIdentity)),
  Green = sum(!grepl("W|U|B|R", Draft25$colorIdentity)),
  Azorius = sum(!grepl("B|R|G", Draft25$colorIdentity)),
  Orzhov = sum(!grepl("U|R|G", Draft25$colorIdentity)),
  Boros = sum(!grepl("U|B|G", Draft25$colorIdentity)),
  Selesnya = sum(!grepl("U|B|R", Draft25$colorIdentity)),
  Dimir = sum(!grepl("W|R|G", Draft25$colorIdentity)),
  Izzet = sum(!grepl("W|B|G", Draft25$colorIdentity)),
  Simic = sum(!grepl("W|B|R", Draft25$colorIdentity)),
  Rakdos = sum(!grepl("W|U|G", Draft25$colorIdentity)),
  Golgari = sum(!grepl("W|U|R", Draft25$colorIdentity)),
  Gruul = sum(!grepl("W|U|B", Draft25$colorIdentity)),
  Esper = sum(!grepl("R|G", Draft25$colorIdentity)),
  Jeskai = sum(!grepl("B|G", Draft25$colorIdentity)),
  Mardu = sum(!grepl("U|G", Draft25$colorIdentity)),
  Bant = sum(!grepl("B|R", Draft25$colorIdentity)),
  Abzan = sum(!grepl("U|R", Draft25$colorIdentity)),
  Naya = sum(!grepl("U|B", Draft25$colorIdentity)),
  Grixis = sum(!grepl("W|G", Draft25$colorIdentity)),
  Sultai = sum(!grepl("W|R", Draft25$colorIdentity)),
  Temur = sum(!grepl("W|B", Draft25$colorIdentity)),
  Jund = sum(!grepl("W|U", Draft25$colorIdentity)),
  Yore = sum(!grepl("G", Draft25$colorIdentity)),
  Witch = sum(!grepl("R", Draft25$colorIdentity)),
  Ink = sum(!grepl("B", Draft25$colorIdentity)),
  Dune = sum(!grepl("U", Draft25$colorIdentity)),
  Glint = sum(!grepl("W", Draft25$colorIdentity)),
  Rainbow = n_distinct(Draft25$name))%>%
  pivot_longer(cols = everything(),
    names_to = "color",
               values_to = "number")

summary_df$color <- factor(summary_df$color, levels = unique(summary_df$color))

ggplot(summary_df, aes(x = color, y = number)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Category Counts", x = "Colour", y = "Viable Cards")



write.csv(Draft25%>%select(name),"Draft25.csv",row.names = FALSE)


deckstats <- merge(Master_Data,
                   setsdl,
                   by.x = 'setCode',
                   by.y = 'code')%>%
  filter(commander_deck == "The Mycotyrant")%>%
  filter(name.x != "Forest" & name.x != "Swamp" & name.x != "Forest // Forest")%>%
  select(name = "name.y")%>%
  count(name)%>%
  arrange(n)

cardstats <- merge(Master_Data%>%
                     filter(commander_deck == "The Mycotyrant")%>%
                   select(name),
                 cards%>%
                   arrange(edhrecSaltiness)%>%
                   select(name, edhrecSaltiness)%>%
                   distinct(name, .keep_all = TRUE),
                 by.x = "name",
                 by.y = "name")


pie_chart <- ggplot(deckstats, aes(x = "", y = n, fill = name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +  # Remove background and axes for a classic pie chart look
  geom_text(aes(label = paste0(n)), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Pie Chart of Categories", fill = "Category") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


pie_chart <- ggplot(deckstats, aes(x = "", y = n, fill = name)) +
  geom_bar(stat = "identity", width = 1) +
#  coord_polar(theta = "y", start = -pi/4) +  # Rotate to start at the top
  theme_void() +  # Remove background and axes for a classic pie chart look
  geom_text(aes(label = paste0(n)), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Pie Chart of Categories", fill = "Category") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


cards%>%
  arrange(edhrecSaltiness)%>%
  select(name, edhrecSaltiness)%>%
  distinct(name, .keep_all = TRUE)





# Sort the data by size of `n` in descending order
deckstats <- deckstats[order(-deckstats$n), ]

# Calculate cumulative sum of proportions for alignment
deckstats$cumsum <- cumsum(deckstats$n / sum(deckstats$n))

# Adjust the start position so the largest segment starts at 12 o'clock
start_angle <- -pi * (deckstats$cumsum[1] - deckstats$n[1] / (2 * sum(deckstats$n)))

# Create the pie chart
pie_chart <- ggplot(deckstats, aes(x = "", y = n, fill = name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y", start = start_angle) +  # Rotate to start at 12 o'clock
  theme_void() +  # Remove background and axes for a classic pie chart look
  geom_text(aes(label = paste0(n)), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Pie Chart of Categories", fill = "Category") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title



