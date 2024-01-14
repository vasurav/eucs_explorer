library(tidyverse)
library(plotly)
library(shiny)
library(shinyWidgets)
library(thematic)
library(DT)
library(bslib)
library(bsicons)
library(markdown)
theme_set(theme_minimal())
source("read_data.R")

thematic_shiny(font = "auto")

enableBookmarking("url")

# Read Data
game_data <- read_all_game_data("demo_data/output_demo_2023/")
summary_data <- read_all_summary_data("demo_data/output_demo_2023/")
