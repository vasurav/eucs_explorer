str_to_input_link <- function(string, href="#", select_input = "team", tab="Team")
{
  paste0('<a href="', 
         href ,"\",",
         " onclick=\"Shiny.setInputValue('",
         select_input, "', '",
         string, "')\">",
         string, "</a>")
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
  