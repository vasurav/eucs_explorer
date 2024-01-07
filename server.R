function(input, output, session) {
  # Game Data functions
  game_data_filtered <- reactive({
    game_data %>% filter_output_data
  })
  
  summary_data_filtered <- reactive({
    summary_data %>% filter_output_data
  })
  
  summary_data_filtered_eligible <- reactive({
    summary_data_filtered() %>% 
      filter(Games >= 10) %>% 
      mutate(Ranking = rank(-Rating_USAU))
  })
  
  filter_output_data <- function(df) {
    df %>% filter(Division == input$division,
                  Season == input$season)
  }
  
  
  # Functions for Season Tab
  output$season_ranking_table <- renderDT({
    
    if(input$eligible_only == "10+ Games Only")
      ranking_data = summary_data_filtered_eligible()
    else
      ranking_data = summary_data_filtered()
    
    ranking_data %>% 
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
      mutate(Game_Rank_Diff_USAU = Game_Rank_Diff_USAU %>% round(2),
             Team_Rank_Diff_USAU = Team_Rank_Diff_USAU %>% round(2)) %>% 
      format_DT
    
  })
  
  #Functions for Team Tab
  
  team_summary_data <- reactive({
    req(input$team)
    game_data_filtered() %>% 
      filter(Team_1 == input$team | Team_2 == input$team) %>% 
      mutate(Team              = input$team,
             Score_Team        = if_else(Team_1 == input$team, Score_1, Score_2),
             Opponent          = if_else(Team_1 == input$team, Team_2, Team_1),
             Score_Opponent    = if_else(Team_1 == input$team, Score_2, Score_1),
             Score             = paste0(Score_Team, " - ", Score_Opponent),
             Result            = if_else(Score_Team >= Score_Opponent, "Win", "Loss")) %>% 
      left_join(summary_data_filtered() %>% 
                  select(Team, Rating_USAU),
                by = c("Opponent"="Team")) %>% 
      rename(Opponent_Rating = Rating_USAU) %>% 
      mutate(Game_Rating = ifelse(Result == "Win", 
                                  Opponent_Rating + Game_Rank_Diff_USAU, 
                                  Opponent_Rating - Game_Rank_Diff_USAU) %>% 
               round(2)) %>% 
      mutate(Counted = ifelse(Is_Ignored_Windmill, "No", "Yes"))
  })
  
  output$team_selector <- renderUI({
    selectInput("team", label = NULL, width="100%",
                choices = summary_data_filtered() %>% 
                  arrange(Team) %>% 
                  pull(Team))
  })
  
  ranking_row <- reactive({
    summary_data_filtered() %>% filter(Team == input$team)
  })
  
  eucf_cutoff_rating <- reactive({
    if(input$eligible_only == "10+ Games Only")
      ranking_data = summary_data_filtered_eligible()
    else
      ranking_data = summary_data_filtered()
    
    rating_list <- 
      ranking_data %>% 
        arrange(Ranking) %>% 
        pull(Rating_USAU)
    
    rating_list[16]
  })
  
  team_distance_from_eucf_cutoff_rating <- reactive({
    eucf_cutoff_rating() - ranking_row()$Rating_USAU
  })
  
  output$team_rank <- renderText(ranking_row()$Ranking)
  output$team_rating <- renderText(ranking_row()$Rating_USAU)
  output$team_record <- renderText({
    paste0(ranking_row()$Wins, " - ", ranking_row()$Losses)
  })
  output$team_strength_of_schedule <- renderText({
    team_summary_data() %>% 
      filter(Counted == "Yes") %>% 
      pull(Opponent_Rating) %>% 
      mean() %>% round(2)
  })
  output$distance_from_eucf_cutoff_rating <- 
    renderText(team_distance_from_eucf_cutoff_rating())
  
  output$team_games_table <- renderDT({
    req(input$team)
    team_summary_data() %>% 
      select(Tournament, Date, Opponent, Result, Score, Game_Rating, Opponent_Rating, Counted) %>% 
      arrange(desc(Date)) %>% 
      format_DT(scrollY = "45VH")
  })
  
  
  
  
  # General Formatting Functions
  format_DT <- function(table, scrollY = "60VH", rownames = F)
  {
    table %>% 
      datatable(rownames = rownames,
                options = 
                  list(paging = F, 
                       info = F,
                       scrollY = scrollY,
                       columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  )
      )
  }
}
