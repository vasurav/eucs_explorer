source("algorithm_explainer.R")

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
      filter(if(input$eligible_only == ">10 Games Only") Games >= 10 
             else T) %>% 
      mutate(Ranking = rank(-Rating_USAU))
  })
  
  filter_output_data <- function(df) {
    df %>% filter(Division == input$division,
                  Season == input$season)
  }
  
  
  # Functions for Season Tab
  output$season_ranking_table <- renderDT({
    
    # if(input$eligible_only == "10+ Games Only")
    #   ranking_data = summary_data_filtered_eligible()
    # else
    #   ranking_data = summary_data_filtered()
    
    summary_data_filtered_eligible() %>% 
      arrange(Ranking) %>% 
      select(Ranking, Team, Rating_USAU, Tournaments, Games) %>% 
      mutate(Team = str_to_url_link(Team, input = input)) %>% 
      dplyr::rename(Rating = Rating_USAU) %>% 
      format_DT
  })
  
  output$season_games_table <- renderDT({
    game_data_filtered() %>% 
      mutate(Game = paste0(str_to_url_link(Team_1, input = input), " vs. ", str_to_url_link(Team_2, input = input), " : ", 
                           Score_1, "-", Score_2)) %>% 
      mutate(Game_Rating_Diff = Game_Rank_Diff_USAU %>% round(2),
             Team_Rating_Diff = Team_Rank_Diff_USAU %>% round(2)) %>% 
      select(Tournament, Date, 
             Game, 
             Game_Rating_Diff,
             Team_Rating_Diff) %>% 
      format_DT
    
  })
  
  output$season_network <- renderSimpleNetwork({
    game_data_filtered() %>% 
      simpleNetwork("Team_1", "Team_2", 
                    linkColour = "#DA0", nodeColour = "#000", 
                    zoom=T, 
                    fontFamily = font_google("Bebas Neue"),
                    linkDistance = 250)
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
             Result            = case_when(Score_Team > Score_Opponent ~ "Win", 
                                           Score_Team < Score_Opponent ~ "Loss",
                                           Score_Team == Score_Opponent ~ "Draw")) %>% 
      left_join(summary_data_filtered() %>% 
                  select(Team, Rating_USAU),
                by = c("Opponent"="Team")) %>% 
      rename(Opponent_Rating = Rating_USAU) %>% 
      mutate(Game_Rating = ifelse(Result == "Win", 
                                  Opponent_Rating + Game_Rank_Diff_USAU, 
                                  Opponent_Rating - Game_Rank_Diff_USAU) %>% 
               round(2)) %>% 
      mutate(Counted = ifelse(Is_Ignored_USAU, "No", "Yes"))
  })
  
  output$team_selector <- renderUI({
    selectInput("team", label = NULL, width="100%",
                choices = summary_data_filtered() %>% 
                  arrange(Team) %>% 
                  pull(Team))
  })
  
  observeEvent(input$team, {
    updateSelectInput(session, "team", selected=input$team)
  })
  
  ranking_row <- reactive({
    summary_data_filtered() %>% filter(Team == input$team)
  })
  
  ranking_row_eligible <- reactive({
    summary_data_filtered_eligible() %>% filter(Team == input$team)
  })
  
  eucf_cutoff_rating <- reactive({
    # if(input$eligible_only == "10+ Games Only")
    #   ranking_data = summary_data_filtered_eligible()
    # else
    #   ranking_data = summary_data_filtered()
    
    rating_list <- 
      summary_data_filtered_eligible() %>% 
        arrange(Ranking) %>% 
        pull(Rating_USAU)
    
    rating_list[16]
  })
  
  team_distance_from_eucf_cutoff_rating <- reactive({
    eucf_cutoff_rating() - ranking_row()$Rating_USAU
  })
  
  output$team_rank <- renderText({
    req(input$team)
    ifelse(count(ranking_row_eligible()) == 0, 
            "Not Enough Games",
            ranking_row_eligible()$Ranking
            )
  })
  output$team_rating <- renderText({
    req(input$team)
    ranking_row()$Rating_USAU
  })

  output$team_record <- renderText({
    req(input$team)
    paste0(ranking_row()$Wins, " - ", ranking_row()$Losses)
  })
  output$team_strength_of_schedule <- renderText({
    req(input$team)
    team_summary_data() %>% 
      filter(Counted == "Yes") %>% 
      pull(Opponent_Rating) %>% 
      mean() %>% round(2)
  })
  output$distance_from_eucf_cutoff_rating <- renderText({
    req(input$team)
    team_distance_from_eucf_cutoff_rating()
  })
  
  output$team_games_table <- renderDT({
    req(input$team)
    team_summary_data() %>% 
      select(Tournament, Date, Opponent, Result, Score, Game_Rating, Opponent_Rating, Counted) %>% 
      arrange(desc(Date)) %>% 
      mutate(Opponent = str_to_input_link(Opponent)) %>% 
      format_DT(scrollY = "40VH")
  })
  
  # Functions for Matchup Tab
  output$matchup_team_1 <- renderUI({
    select_matchup_team(1)
  })
  
  output$matchup_team_2 <- renderUI({
    select_matchup_team(2)
  })
  
  select_matchup_team <- function(team_number)
  {
    teams <- summary_data_filtered() %>% 
      pull(Team) %>% sort()
    
    selectInput(paste0("matchup_team_",team_number), 
                label = NULL,
                choices = teams, 
                selected = teams[team_number],
                width="100%")
  }
  
  output$matchup_rating_diff <- renderText({
    req(input$matchup_team_1, input$matchup_team_2)
    
    matchup_rating_diff()
  })
  
  output$matchup_expected_score <- renderText({
    req(input$matchup_team_1, input$matchup_team_2)
    
    matchup_expected_score_string(matchup_rating_diff())
  })
  
  matchup_expected_score_string <- function(rating_diff)
  {
    score_diff = calc_score_diff_rating_diff(rating_diff) %>% 
      round(2)
    
    case_when(
      abs(rating_diff) > 600 ~ "Blowout",
      abs(rating_diff) <= 45 ~ "Draw",
      rating_diff < 0 ~ paste0((15 - score_diff), " - 15"),
      .default = paste0("15 - ", (15 - score_diff))
    )
  }
  
  
  output$matchup_mutual_games <- renderDT({
    req(input$matchup_team_1, input$matchup_team_2)
    game_data_filtered() %>% 
      filter(
        (Team_1 == input$matchup_team_1 & Team_2 == input$matchup_team_2) | 
          (Team_1 == input$matchup_team_2 & Team_2 == input$matchup_team_1)
      ) %>%  
      mutate(Score = ifelse(Team_1 == input$matchup_team_1,
                            paste0(Score_1, " - ", Score_2),
                            paste0(Score_2, " - ", Score_1))) %>% 
      mutate(Rating_Difference = Game_Rank_Diff_USAU %>% round(2)) %>% 
      mutate(Counted = ifelse(Is_Ignored_USAU, "No", "Yes")) %>% 
      select(Tournament, Date, Score, Rating_Difference, Counted) %>% 
      format_DT
  })
  
  
  matchup_rating_diff <- function()
  {
    (summary_data_filtered() %>% 
      filter(Team == input$matchup_team_1) %>% 
      pull(Rating_USAU)) - 
      (summary_data_filtered() %>% 
      filter(Team == input$matchup_team_2) %>% 
      pull(Rating_USAU))
  }
  
  # General Formatting Functions
  format_DT <- function(table, scrollY = "60VH", rownames = F, searching=T)
  {
    table %>% 
      datatable(rownames = rownames,
                selection = "single",
                escape = F,
                options = 
                  list(
                    paging = F, 
                    info = F,
                    scrollY = scrollY,
                    sScrollX =  "100%",
                    searching = searching,
                    scrollCollapse = T,
                    columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  )
      )
  }
  
  
  # Algorithm explainer functions
  output$rating_point_diff_plot <- renderPlotly({
    rating_point_diff_plot()
  })
  
  output$weight_point_diff_plot <- renderPlotly({
    weight_point_diff_plot()
  })
  
  output$rating_point_diff_dt <- renderDT({
    rating_point_diff() %>%  select(-score_difference) %>% 
      format_DT(scrollY = "20VH", searching=F)
  })
  
  output$weight_point_diff_dt <- renderDT({
    weight_point_diff() %>% select(-score_difference) %>% 
      format_DT(scrollY = "20VH", searching=F)
  })
}
