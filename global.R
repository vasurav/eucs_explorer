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
library(countrycode)
library(dbplyr)
library(RMariaDB)
library(config)
library(pool)
source("read/read_data.R")

theme_set(theme_minimal())
thematic_shiny(font = "auto")

enableBookmarking("url")

# Read Data
# data_path <- "data/"
# game_data <- read_all_game_data(data_path)
# summary_data <- read_all_summary_data(data_path)


# teams_data <- read_all_teams_data(data_path)
# wildcard_data <- read_all_wildcard_data(data_path) %>% 
#   # mutate(Season = as.character(Season)) %>% 
#   mutate(Wildcard_Date = mdy(Wildcard_Date))
# end_of_season_data <- read_all_end_of_season_data(data_path)

#connect to database
dbConf <- config::get("gravato")

pool <- 
  dbPool(RMariaDB::MariaDB(),
                 host = dbConf$host,
                 user = dbConf$user,
                 password = dbConf$password,
                 dbname = dbConf$dbname
                 )

end_of_season_data <- tbl(pool, "euf_ranking_eos") %>% 
  as.data.table()
teams_data <- tbl(pool, "rostering_master_teams") %>% 
  as.data.table %>% 
  mutate(iso2c = countrycode(country_ioc, "ioc", "iso2c"))
game_data <- tbl(pool, "euf_ranking_games")
wildcard_data <- tbl(pool, "euf_ranking_wildcards") %>% 
  as.data.table()
summary_data <- tbl(pool, "euf_ranking_summary")
master_roster <- tbl(pool, "rostering_master_rosters")
event_teams <- tbl(pool, "rostering_event_teams")
event_roster <- tbl(pool, "rostering_event_rosters")
events <- tbl(pool, "rostering_events")


# Color Schemes
color_eucf_guaranteed_dark <- "#88CC99"
color_eucf_guaranteed_light <- "#DDEEDD"
color_eucf_likely_dark <- "#7799FF"
color_eucf_likely_light <- "#CCDDFF"
color_primary_light <- '#FFCC00'
color_primary_dark <- "#DDAA00"