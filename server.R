source("algorithm/algorithm_explainer.R")
source("server/read_data.R")
source("server/string_functions.R")

function(input, output, session) {
  # Functions for Wrangling Data
  game_data_filtered <- reactive({
    game_data %>% filter_output_data
  })
  
  summary_data_filtered <- reactive({
    summary_data %>% 
      filter_output_data %>% 
      add_wildcard_to_summary
  })
  
  summary_data_filtered_eligible <- reactive({
    summary_data_filtered() %>% 
      filter(
        if(input$eligible_only == ">10 Games Only") 
          Games >= 10 
        else T
      ) %>% 
      # filter(
      #   if(!input$include_wildcard) 
      #     !Wildcard
      #   else T
      # ) %>% 
      mutate(Ranking = rank(-Rating_USAU)) %>% 
      add_ranking_no_wildcard
  })
  
  filter_output_data <- function(df) {
    df %>% filter(Division == input$division,
                  Season == input$season)
  }
  
  add_wildcard_to_summary <- function(df) {
    df %>% left_join(wildcard_data, by=c("Team", "Division", "Season")) %>% 
      mutate(Wildcard = !is.na(Wildcard_Event)) %>% 
      add_ranking_no_wildcard
      
  }
  
  add_ranking_no_wildcard <- function(df) {
    df %>% 
      group_by(Wildcard) %>% 
      mutate(Ranking_No_Wildcard = if_else(Wildcard, NA, rank(-Rating_USAU))) %>% 
      ungroup()
  }
  
  total_wildcards <- reactive({
    wildcard_data %>% 
      filter(Division == input$division) %>% 
      nrow
  })
  
  wildcards_awarded <- reactive({
    summary_data_filtered() %>% filter(Wildcard == T) %>% nrow
  })
  
  eucf_ranking_spots_guaranteed <- reactive({
    16 - total_wildcards()
  })
  
  eucf_ranking_spots_likely <- reactive({
    16 - wildcards_awarded()
  })
  
  # Formatting Functions
  
  DT_options <- function(scrollY = "100%", searching=T, hidden_rows = c()){
    list(
      paging = F, 
      info = F,
      scrollY = scrollY,
      sScrollX =  "100%",
      searching = searching,
      scrollCollapse = T,
      columnDefs = list(
        list(className = 'dt-left', targets = "_all"),
        list(visible=FALSE, targets=hidden_rows)
      )
    )
  }
  
  format_DT <- function(table, rownames = F,  options = DT_options)
  {
    table %>% 
      datatable(rownames = rownames,
                selection = "single",
                style = "default",
                escape = F,
                options = options
                    #columnDefs = list(list(visible=FALSE, targets=c("Ranking_No_Wildcard")))
                    #columnDefs = list(list(className = 'dt-left', targets = "_all"))
      )
  }
  
  # Functions for Season Tab
  output$wildcard_count <- renderText({
    paste0(wildcards_awarded(), " of ", total_wildcards())
  })
  
  output$eucf_ranking_spots_guaranteed <- renderText({
    eucf_ranking_spots_guaranteed()
  })
  
  output$eucf_ranking_spots_likely <- renderText({
    eucf_ranking_spots_likely() - eucf_ranking_spots_guaranteed()
  })
  
  output$season_ranking_table <- renderDT({
    summary_data_filtered_eligible() %>% 
      arrange(Ranking) %>% 
      mutate(Wildcard = if_else(Wildcard, Wildcard_Event, "")) %>% 
      mutate(`Record (W-L)` = paste0(Wins, " - ", Losses)) %>% 
      select(Ranking, Ranking_No_Wildcard, Team, Rating_USAU, `Record (W-L)`, Tournaments, Wildcard) %>% 
      mutate(Team = str_to_url_link(Team, input = input)) %>% 
      dplyr::rename(Rating = Rating_USAU) %>%
      format_DT(options = DT_options(hidden_rows = c("Ranking_No_Wildcard"))) %>% 
      formatStyle(
        'Ranking_No_Wildcard',
        target='row',
        backgroundColor = styleEqual(1:(eucf_ranking_spots_likely()), color_eucf_likely_light)
      ) %>% 
      formatStyle(
        'Ranking_No_Wildcard',
        target='row',
        backgroundColor = styleEqual(1:(eucf_ranking_spots_guaranteed()), color_eucf_guaranteed_light)
      ) %>% 
      formatStyle(
        'Ranking',
        'Ranking_No_Wildcard',
        backgroundColor = styleEqual(1:(eucf_ranking_spots_likely()), color_eucf_likely_dark)
      ) %>% 
      formatStyle(
        'Ranking',
        'Ranking_No_Wildcard',
        backgroundColor = styleEqual(1:(eucf_ranking_spots_guaranteed()), color_eucf_guaranteed_dark)
      ) %>% 
      formatStyle(
        'Wildcard',
        target='row',
        backgroundColor = styleEqual(wildcard_events(), '#FC0')
      )
  })
  
  wildcard_events <- reactive({
    wildcard_data %>% pull(Wildcard_Event)
  })
  
  output$season_games_table <- renderDT({
    game_data_filtered() %>% 
      mutate(Game = paste0(str_to_url_link(Team_1, input = input), " vs. ", str_to_url_link(Team_2, input = input), " : ", 
                           Score_1, "-", Score_2)) %>% 
      mutate(Game_Rating_Diff = Game_Rank_Diff_USAU %>% round(2),
             Team_Rating_Diff = Team_Rank_Diff_USAU %>% round(2)) %>% 
      mutate(Counted = ifelse(Is_Ignored_USAU, "No", "Yes")) %>% 
      select(Tournament, Date, 
             Game, 
             Game_Rating_Diff,
             Team_Rating_Diff, Counted) %>% 
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
  
  # Functions for Team Tab
  team_summary_data <- reactive({
    req(input$team)
    team_summary(input$team)
  })
  
  team_summary <- function(team)
  {
    game_data_filtered() %>% 
      filter(Team_1 == team | Team_2 == team) %>% 
      mutate(Team              = team,
             Score_Team        = if_else(Team_1 == team, Score_1, Score_2),
             Opponent          = if_else(Team_1 == team, Team_2, Team_1),
             Score_Opponent    = if_else(Team_1 == team, Score_2, Score_1),
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
  }
  
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
  
  eucf_cutoff_rating_guaranteed <- reactive({
    req(input$team)
    
    last_rating <- rating_list_no_wildcard()[eucf_ranking_spots_guaranteed()]
    ifelse(ranking_row()$Rating_USAU < last_rating,
           last_rating,
           rating_list_no_wildcard()[eucf_ranking_spots_guaranteed() + 1])
  })
  
  output$eucf_cutoff_rating_guaranteed <- renderText(eucf_cutoff_rating_guaranteed())
  
  eucf_cutoff_rating_likely <- reactive({
    req(input$team)
    
    last_rating <- rating_list_no_wildcard()[eucf_ranking_spots_likely()]
    
    ifelse(ranking_row()$Rating_USAU < last_rating,
           last_rating,
           rating_list_no_wildcard()[eucf_ranking_spots_likely() + 1])
  })
  
  output$eucf_cutoff_rating_likely <- renderText(eucf_cutoff_rating_likely())
  
  rating_list_no_wildcard <- reactive({
    summary_data_filtered_eligible() %>% 
      arrange(Ranking_No_Wildcard) %>% 
      pull(Rating_USAU)
  })
  
  team_distance_from_eucf_cutoff_rating <- reactive({
    ifelse(input$team_eucf_cutoff_type,
           eucf_cutoff_rating_likely() - ranking_row()$Rating_USAU,
           eucf_cutoff_rating_guaranteed() - ranking_row()$Rating_USAU
           )
  })
  
  output$team_rank <- renderText({
    req(input$team)
    ifelse(count(ranking_row_eligible()) == 0, 
            ifelse(ranking_row()$Wildcard,
                   "Wildcard",
                   "Not Enough Games"),
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
  
  output$color_distance_from_eucf_cutoff <- reactive({
    ifelse(input$team_eucf_cutoff_type == "Likely",
           color_eucf_likely_dark,
           color_eucf_guarnteed_dark)
  })
  
  output$team_games_table <- renderDT({
    req(input$team)
    team_summary_data() %>% 
      select(Tournament, Date, Opponent, Result, Score, Game_Rating, Opponent_Rating, Counted) %>% 
      arrange(desc(Date)) %>% 
      mutate(Opponent = str_to_input_link(Opponent)) %>% 
      format_DT(options = DT_options(scrollY = "40VH"))
  })
  
  output$team_games_plot <- renderPlotly({
    req(input$team)
    
    tournament_order <- team_summary_data() %>% 
      arrange(Date) %>% pull(Tournament) %>% unique()
    
    `Mean Rating (Counted Games Only)` = mean(team_summary_data() %>% 
                           filter(Counted == "Yes") %>% 
                           pull(Game_Rating))
    `EUCF Cutoff` = eucf_cutoff_rating_guaranteed()
    `EUCF Cutoff (Including Unassigned Wildcards)` = eucf_cutoff_rating_likely()
    
    color_data <- color_primary_dark
    
    (team_summary_data() %>% 
        mutate(Tournament = factor(Tournament, levels = tournament_order)) %>% 
        mutate(Counted = factor(Counted, levels = c("Yes", "No"))) %>% 
        ggplot(aes(x=Tournament, y=Game_Rating,
                   label = Date,
                   label1 = Opponent,
                   label2 = Score)) + 
        geom_boxplot(color = color_data,
                     outlier.shape = NA) +
        geom_jitter(aes(shape = Counted), 
                    alpha = 0.5, 
                    size=2.5, 
                    color = color_data,
                    width = 0.05) +
        scale_shape_manual(values = c(19, 4)) +
        geom_hline(yintercept = 0) + 
        geom_hline(aes(yintercept = `Mean Rating (Counted Games Only)`, 
                       linetype = "Mean Rating"), color = color_data) +
        geom_hline(aes(yintercept = `EUCF Cutoff (Including Unassigned Wildcards)`, 
                       linetype = "EUCF Cutoff (Including Unassigned Wildcards)"),
                   color = color_eucf_likely_dark) +
        geom_hline(aes(yintercept = `EUCF Cutoff`, 
                       linetype = "EUCF Cutoff"),
                   color = color_eucf_guaranteed_dark) +
        scale_linetype_manual(name="Horizontal Lines", values = c(2, 2, 3))) %>% 
      ggplotly(tooltip = c("shape", "x", "y", "label", "label1", "label2", "yintercept")) %>% hide_legend()
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
  
  
  output$matchup_game_history <- renderDT({
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
  
  output$matchup_mutual_opponents <- renderDT({
    req(input$matchup_team_1, input$matchup_team_2)
    
    opponents_1 <- team_summary(input$matchup_team_1) %>% pull(Opponent) %>% unique
    opponents_2 <- team_summary(input$matchup_team_2) %>% pull(Opponent) %>% unique
    
    common_opponents <- intersect(opponents_1, opponents_2)
    
    team_summary(input$matchup_team_1) %>% 
      mutate(Team = input$matchup_team_1) %>% 
      filter(Opponent %in% common_opponents) %>% 
      bind_rows(
        team_summary(input$matchup_team_2) %>% 
          mutate(Team = input$matchup_team_2) %>% 
          filter(Opponent %in% common_opponents)
      ) %>% 
      select(Team, Opponent, Score, Tournament, Date) %>% 
      format_DT
  })
  
  output$matchup_mutual_opponents_1 <- renderDT({
    req(input$matchup_team_1, input$matchup_team_2)
    
    opponents_1 <- team_summary(input$matchup_team_1) %>% pull(Opponent) %>% unique
    opponents_2 <- team_summary(input$matchup_team_2) %>% pull(Opponent) %>% unique
    
    common_opponents <- intersect(opponents_1, opponents_2)
    
    team_summary(input$matchup_team_1) %>% 
      mutate(Team = input$matchup_team_1) %>% 
      filter(Opponent %in% common_opponents) %>% 
      select(Team, Opponent, Score, Tournament, Date) %>% 
      arrange(Opponent) %>% 
      format_DT
  })
  
  output$matchup_mutual_opponents_2 <- renderDT({
    req(input$matchup_team_1, input$matchup_team_2)
    
    opponents_1 <- team_summary(input$matchup_team_1) %>% pull(Opponent) %>% unique
    opponents_2 <- team_summary(input$matchup_team_2) %>% pull(Opponent) %>% unique
    
    common_opponents <- intersect(opponents_1, opponents_2)
    
    team_summary(input$matchup_team_2) %>% 
      mutate(Team = input$matchup_team_2) %>% 
      filter(Opponent %in% common_opponents) %>% 
      select(Team, Opponent, Score, Tournament, Date) %>% 
      arrange(Opponent) %>% 
      format_DT
  })
  
  # Functions for Algorithm Tab
  output$rating_point_diff_plot <- renderPlotly({
    rating_point_diff_plot()
  })
  
  output$weight_point_diff_plot <- renderPlotly({
    weight_point_diff_plot()
  })
  
  output$rating_point_diff_dt <- renderDT({
    rating_point_diff() %>%  select(-score_difference) %>% 
      format_DT(DT_options(scrollY = "20VH", searching=F))
  })
  
  output$weight_point_diff_dt <- renderDT({
    weight_point_diff() %>% select(-score_difference) %>% 
      format_DT(DT_options(scrollY = "20VH", searching=F))
  })
}
