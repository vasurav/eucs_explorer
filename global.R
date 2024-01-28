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
color_eucf_guaranteed <- "#88CC99"
color_eucf_probable <- "#7799FF"
color_primary_light <- '#FFCC00'
color_primary_dark <- "#DDAA00"