str_to_input_link <- function(string, href="#", select_input = "team", tab="Team")
{
  paste0('<a href="', 
         href ,"\",",
         " onclick=\"Shiny.setInputValue('",
         select_input, "', '",
         string, "',{priority:'event'})\">",
         string, "</a>")
}

flag_img <- function(country_code = "eu")
{
  tags$img(
    src = paste0(
      "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",
      country_code %>% str_to_lower,
      ".svg"
    ),
    height="10.5px"
  ) %>% 
    as.character()
}

get_iso2c <- function(team, division)
{
  teams_data %>% 
    filter(team_name == team, division_name == division) %>% 
    first() %>% 
    pull(iso2c)
}

flag_team <- function(team, division)
{
  get_iso2c(team, division) %>% flag_img
}

flag_and_link <- function(team, division, left_side = T)
{
  flag <- flag_team(team, division)
  
  if_else(left_side,
          paste0(flag, " ", str_to_input_link(team)),
          paste0(str_to_input_link(team), " ", flag)
          )
}

str_to_url_link <- function(team, tab="Team", input)
{
  paste0("<a href=\"?_inputs_",
         "&main_tab=&quot;",tab,"&quot;",
         "&team=&quot;",team, "&quot;",
         "&division=&quot;",input$division, "&quot;",
         "&season=&quot;",input$season, "&quot;",
         "&eligible_only=&quot;",input$eligible_only, "&quot;",
         "&ranking_date=&quot;",input$ranking_date, "&quot;",
         "\">",team,"</a>")
}
  