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
game_data <- read_all_game_data("demo_data/output_demo_2023_eucs_only/")
summary_data <- read_all_summary_data("demo_data/output_demo_2023_eucs_only/")
