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

cards <- read.csv("C:/MTGDataFiles/CardandPricesData/cards.csv")
cardPrices <- read.csv("C:/MTGDataFiles/CardandPricesData/cardPrices.csv")
legalities <- read.csv("C:/MTGDataFiles/CardandPricesData/cardsLegalities.csv")

cards %>%
  filter(grepl('Demon', name))%>%
  filter(colorIdentity == '' | !grepl('X', colorIdentity))%>%
  filter(colorIdentity == '' | !grepl('X', colorIdentity))%>%
  filter(colorIdentity == '' | !grepl('X', colorIdentity))%>%
  filter(colorIdentity == '' | !grepl('X', colorIdentity))%>%
  filter(colorIdentity == '' | !grepl('X', colorIdentity))%>%
  select(name, manaCost, type, power, toughness, text, printings)%>%
  distinct(name, manaCost, type, power, toughness, text, printings)
