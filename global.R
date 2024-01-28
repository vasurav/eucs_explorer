library(tidyverse)
library(data.table)
library(dtplyr)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(thematic)
library(DT)
library(bslib)
library(bsicons)
library(markdown)
library(networkD3)
source("server/read_data.R")

theme_set(theme_minimal())
thematic_shiny(font = "auto")

enableBookmarking("url")

# Read Data
data_path <- "demo_data/output_demo_2023_eucs_only/"
game_data <- read_all_game_data(data_path)
summary_data <- read_all_summary_data(data_path)
wildcard_data <- read_all_wildcard_data(data_path) %>% 
  mutate(Season = as.character(Season))


# Color Schemes
color_eucf_guaranteed_dark <- "#88CC99"
color_eucf_guaranteed_light <- "#DDEEDD"
color_eucf_likely_dark <- "#7799FF"
color_eucf_likely_light <- "#CCDDFF"
color_primary_light <- '#FFCC00'
color_primary_dark <- "#DDAA00"