function(input, output, session) {
  game_data_filtered <- reactive({
    game_data %>% filter_output_data
  })
  
  summary_data_filtered <- reactive({
    summary_data %>% filter_output_data
  })
  
  filter_output_data <- function(df) {
    df %>% filter(Division == input$division,
                  Season == input$season)
  }
  
  output$season_ranking_table <- renderDT({
    summary_data_filtered() %>%
      arrange(Ranking) %>% 
      select(Ranking, Team, Rating_USAU, Tournaments, Games) %>% 
      dplyr::rename(Rating = Rating_USAU) %>% 
      format_DT
  })
  
  output$season_games_table <- renderDT({
    game_data_filtered() %>% 
      select(Tournament, Date, 
             Team_1, Team_2, Score_1, Score_2, 
             Game_Rank_Diff_USAU,
             Team_Rank_Diff_USAU) %>% 
      format_DT
    
  })
  
  format_DT <- function(table)
  {
    table %>% 
      datatable(filter = "top", rownames = F,
                options = 
                  list(paging = F, 
                       info = F,
                       scrollY = "50VH",
                       columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  )
      )
  }
}
