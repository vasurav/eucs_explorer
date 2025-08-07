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

# Old Code used to read data locally - could be used to debug things
# data_path <- "data/"
# game_data <- read_all_game_data(data_path)
# summary_data <- read_all_summary_data(data_path)

# teams_data <- read_all_teams_data(data_path)
# wildcard_data <- read_all_wildcard_data(data_path) %>% 
#   # mutate(season = as.character(Season)) %>% 
#   mutate(Wildcard_Date = mdy(Wildcard_Date))
# end_of_season_data <- read_all_end_of_season_data(data_path)


# New code - Connect to database and read everything using pool
dbConf <- config::get("gravato")

pool <- 
  dbPool(RMariaDB::MariaDB(),
                 host = dbConf$host,
                 user = dbConf$user,
                 password = dbConf$password,
                 dbname = dbConf$dbname
                 )

# get 2025 data to add later
teams_data_2025 <- tbl(pool, "rostering_master_teams_2025") %>% 
  mutate(season = 2025)
master_roster_2025 <- tbl(pool, "rostering_master_rosters_2025") %>% 
  mutate(season = 2025)
event_roster_2025 <- tbl(pool, "rostering_event_rosters_2025") %>% 
  mutate(season = 2025)
events_2025 <- tbl(pool, "rostering_events_2025") %>% 
  mutate(season = 2025)
event_teams_2025 <- tbl(pool, "rostering_event_teams_2025") %>% 
  mutate(season = 2025)


# get 2024 data and append 2025 data to it - this should be replaced later when backend is fixed
teams_data <- tbl(pool, "rostering_master_teams") %>% 
  mutate(season = 2024) %>% 
  union_all(teams_data_2025) %>% 
  as.data.table %>% 
  mutate(iso2c = countrycode(country_ioc, "ioc", "iso2c"))
master_roster <- tbl(pool, "rostering_master_rosters")  %>% 
  mutate(season = 2024) %>% 
  union_all(master_roster_2025)
event_teams <- tbl(pool, "rostering_event_teams")%>% 
  mutate(season = 2024) %>% 
  union_all(event_teams_2025)
event_roster <- tbl(pool, "rostering_event_rosters") %>% 
  mutate(season = 2024) %>% 
  union_all(event_roster_2025)
events <- tbl(pool, "rostering_events") %>% 
  mutate(season = 2024) %>% 
  union_all(events_2025)

# get ranking data - this data already has a season column so no appending necessary
end_of_season_data <- tbl(pool, "euf_ranking_eos") %>% 
  as.data.table()
game_data <- tbl(pool, "euf_ranking_games")
wildcard_data <- tbl(pool, "euf_ranking_wildcards") %>% 
  as.data.table()

wildcard_data_collapsed <- 
  wildcard_data %>% 
  group_by(Team, Division, Season) %>%
  summarize(Wildcard_Event = paste(Wildcard_Event, collapse = ", "), 
            Wildcard_Date = min(Wildcard_Date),
            .groups = 'drop')
summary_data <- tbl(pool, "euf_ranking_summary")

# game_data <- tbl(pool, "test_games")
# summary_data <- tbl(pool, "test_summary")

champ_events <- tbl(pool,"euf_events")
bids <- tbl(pool,"euf_bids")


# Color Schemes
color_eucf_guaranteed_dark <- "#88CC99"
color_eucf_guaranteed_light <- "#DDEEDD"
color_eucf_likely_dark <- "#BBBBBB"
color_eucf_likely_light <- "#DDDDDD"
color_eucf2_dark <- "#7799FF"
color_eucf2_light <- "#BBDDFF"
color_primary_light <- '#FFCC00'
color_primary_dark <- "#DDAA00"