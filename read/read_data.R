# All the functions involved in reading data that the EUCS Algorithm outputs

# Read all the game data
read_all_game_data <- function(path) {
  read_all_output_data_filetype(path, "games")
}

get_seasons_from_path <- function(data_path){
  list.dirs(data_path)
}

get_rankings_from_season_path <- function(season_path){
  list.dirs(season_path)
}

get_season_data <- function(rankings){
  
}


# Read all the summary data
read_all_summary_data <- function(path) {
  read_all_output_data_filetype(path, "summary", read_summary_data) %>% 
    add_delta_to_summary_data
}

add_delta_to_summary_data <- function(summary_data)
{
  next_ranking_table <- summary_data %>% 
    select(Season, Ranking_Calculation_Date) %>% 
    unique %>%
    group_by(Season) %>% 
    arrange(Ranking_Calculation_Date) %>% 
    mutate(Next_Ranking_Date = lead(Ranking_Calculation_Date))
  
  prior_ranking <- 
    summary_data %>% 
    left_join(next_ranking_table, by=c("Season", "Ranking_Calculation_Date")) %>% 
    select(Team, Ranking, Rating_USAU, Division, Season, Ranking_Calculation_Date, Next_Ranking_Date) %>% 
    rename(Prior_Ranking_Date = Ranking_Calculation_Date,
           Ranking_Calculation_Date = Next_Ranking_Date,
           Prior_Ranking = Ranking,
           Prior_Rating_USAU = Rating_USAU)
  
  summary_data %>% 
    left_join(prior_ranking, by = c("Team", "Season", "Division", "Ranking_Calculation_Date")) %>% 
    mutate(Delta_Ranking = Prior_Ranking - Ranking,
           Delta_Rating_USAU = round(Rating_USAU - Prior_Rating_USAU,2))
}

#Read all wildcard data
read_all_wildcard_data <- function(path) {
  read_all_output_data_filetype(path, "wildcard", fread) 
}

read_all_teams_data <- function(path) {
  read_all_output_data_filetype(path, "teams", fread) %>% 
    mutate(iso2c = countrycode(country_ioc, "ioc", "iso2c"))
}

read_all_end_of_season_data <- function(path) {
  read_all_output_data_filetype(path, "eos", fread)
}


# Read all the output data from a path for a filetype
read_all_output_data_filetype <- function(path, filetype, FUN = read_output_data)
{
  list_all_files_filetype(path, filetype) %>% 
    lapply(FUN = FUN) %>% 
    rbindlist
}

# Read output data from the EUCS Algorithm
read_output_data <- function(path){
  fread(path) %>% 
    label_output_data(path)
}

# Read Summary Data - This adds the ranking 
read_summary_data <- function(path) {
  read_output_data(path) %>% 
    mutate(Ranking = rank(desc(Rating_USAU)))
}

# Label the output data with some extra columns like the division and the ranking calculation date
label_output_data <- function(df, path){
  df %>% 
    mutate(
      Division = 
        path %>% 
        division_from_path %>% 
        str_to_title,
      Ranking_Calculation_Date = 
        path %>% 
        date_from_path %>% 
        ymd,
      Season =
        path %>% 
        season_from_path
    )
}

# For a given filetype ("games", "summary", etc) - list all the files in a given directory
list_all_files_filetype <- function(path, filetype, recursive = T)
{
  list.files(path, recursive = recursive) %>% 
    as_tibble_col(column_name = "filename") %>% 
    filter(grepl(".csv", filename)) %>% 
    filter(filetype_from_path(filename) == filetype) %>% 
    mutate(full_path = paste0(path, filename)) %>% 
    pull(full_path)
}


# Extract words from the filename of an output file 'number' here is the number of the word in the string
word_from_path <- function(path, number){
  matches <- path %>% 
    basename() %>% 
    str_match(paste0("^(?:[^-]+-){", number - 1,"}(\\w*)"))
  
  matches[,2]
}

# Functions to extract specific words
division_from_path <- function(path) word_from_path(path, 2)
date_from_path <- function(path) word_from_path(path, 5)
filetype_from_path <- function(path) word_from_path(path, 4)
season_from_path <- function(path) word_from_path(path, 1)
