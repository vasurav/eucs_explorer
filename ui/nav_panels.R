season_nav_panel <- 
  nav_panel(
    title = "Season",
    accordion(
      accordion_panel("",
                      layout_columns(
                        fill = F, fillable = T,
                        value_box(title = card_title("Wildcards Awarded",
                                                     tooltip(bs_icon("info-circle"),
                                                             "Wildcard bids are awarded to teams who win Elite Invite and Summer Tour events regardless of ranking position. Teams in yellow are guaranteed to qualify for the EUCF regardless of ranking.")),
                                  value = textOutput("wildcard_count"),
                                  showcase = bs_icon("suit-spade-fill"),
                                  theme = "primary"),
                        value_box(title = card_title("Ranking Bids",
                                                     tooltip(bs_icon("info-circle"),
                                                             "Number of ranking spots that are guaranteed to get EUCF spots. Ranking spots in green are guaranteed to qualify teams for the EUCF.")),
                                  value = textOutput("eucf_ranking_spots_guaranteed"),
                                  showcase = bs_icon("shield-fill-check"),
                                  theme = value_box_theme(bg = color_eucf_guaranteed_dark)),
                        value_box(title = card_title("Potential Bids",
                                                     tooltip(bs_icon("info-circle"),
                                                             "The number of potential spots is equal to the number of unassigned wildcards. 
                                           Ranking spots in blue will qualify teams to the EUCF only if teams that earn the wildcards are in the top 16 of the ranking.")),
                                  value = textOutput("eucf_ranking_spots_likely"),
                                  showcase = bs_icon("patch-question-fill"),
                                  theme = value_box_theme(bg = color_eucf_likely_dark))
                        
                      )
      ),
    ),
    navset_card_tab(
      full_screen = T,
      nav_panel(
        title = "Ranking",
        DTOutput("season_ranking_table")
      ),
      nav_panel(
        title = "Games",
        DTOutput("season_games_table")
      ),
      nav_panel(
        title="Evolution",
        layout_columns(
          fill=F, fillable=F,
          numericInput("top_teams_number", "Top Teams", 16, 1, 100, 1),
          selectInput(
            "top_teams_select", 
            tooltip(trigger = list("Include which teams?",
                                   bs_icon("info-circle")),
                    "If 'Entire Season' is selected every team that was ever in the top ranks will be included. If 'Current Ranking' is selected only the teams that hold the top rank from the last ranking calculation are included."), 
            choices=c("Entire Season", "Current Ranking"),
            selected="Current Ranking")
        ),
        plotlyOutput("season_ranking_evolution_plot")
      ),
      nav_panel(
        title = "Connections",
        simpleNetworkOutput("season_network")
      )
    )
  )

team_nav_panel <- 
  nav_panel(
    title = "Team",
    uiOutput("team_selector"),
    accordion(
      accordion_panel("",
                      layout_columns(
                        widths = 1/5, fill = F, fillable = T,
                        value_box(title = 
                                    card_title("Rank"), 
                                  showcase=bs_icon("reception-4"), 
                                  value = textOutput("team_rank"),
                                  theme="primary",
                                  showcase_layout = showcase_left_center(max_height = "50%")),
                        value_box(title = 
                                    card_title("Rating"), 
                                  showcase=bs_icon("heart-half"), 
                                  value = textOutput("team_rating"), 
                                  theme="primary"),
                        value_box(title = 
                                    card_title("Record"), 
                                  showcase=bs_icon("rulers"), 
                                  value = textOutput("team_record"), 
                                  theme="primary"),
                        value_box(title = 
                                    card_title("S.O.S.",
                                               tooltip(bs_icon("info-circle"),
                                                       "Strength of Schedule: Average rating of opponent.")), 
                                  showcase=bs_icon("lightning-fill"), 
                                  value = textOutput("team_strength_of_schedule"), 
                                  theme="primary"),
                        value_box(title = 
                                    card_title("To EUCF Cutoff",
                                               tooltip(
                                                 bs_icon("info-circle"), options = list(html = T),
                                                 HTML("If positive: number of rating points needed to reach the current EUCF cutoff rating. <br> <br>
                               If negative: the amount of points the team can lose and still be above the EUCF cutoff rating.")
                                               ),
                                               popover(
                                                 bs_icon("gear-fill"),
                                                 checkboxInput("team_eucf_cutoff_type", "Include potential bids?",
                                                               value = F),
                                                 placement = "top"
                                               )
                                    ),
                                  showcase=bs_icon("ladder"),
                                  value= textOutput("distance_from_eucf_cutoff_rating"),
                                  theme = "primary")
                      )
      )
    ),
    navset_card_tab(
      full_screen = T,
      nav_panel(
        "Games",
        DTOutput("team_games_table")
      ),
      nav_panel(
        "Plot",
        plotlyOutput("team_games_plot")
      ),
      nav_panel(
        "Evolution",
        plotlyOutput("team_evolution_plot")
      )
      # nav_panel(
      #   "Roster"
      # ),
      # nav_panel(
      #   "Ranking Evolution"
      # )
    )
  )

matchup_nav_panel <- 
  nav_panel(
    title = "Matchup",
    layout_columns(
      widths = 1/2,
      fill = F, fillable = F,
      uiOutput("matchup_team_1"),
      uiOutput("matchup_team_2")
    ),
    accordion(
      accordion_panel("",
                      layout_columns(
                        widths = 1/2,
                        fill = F, fillable = T,
                        value_box(title = card_title("Rating Difference"),
                                  value = textOutput("matchup_rating_diff"),
                                  showcase = bs_icon("clipboard-heart"),
                                  theme = "primary"),
                        value_box(title = card_title("Expected Score",
                                                     tooltip(bs_icon("info-circle"),options = list(html = T),
                                                             HTML("Games of 15-7 to 15-0 are counted as 600 in the algorithm. <br>
                                           Therefore, any time there is a rating difference of more than 600, the score cannot be predicted, and is labeled blowout. <br>
                                           <br>Any teams that are within 45 points of eachother are so close that the algorithm predicts a draw between the two teams."))),
                                  value = textOutput("matchup_expected_score"),
                                  showcase = bs_icon("clipboard-data"),
                                  theme = "primary"),
                      )
      )
    ),
    navset_card_tab(
      full_screen = T,
      nav_panel(
      "Games",
      DTOutput("matchup_game_history")
      ),
      nav_panel(
        "Mutuals",
        layout_column_wrap(
          card(
            card_header(textOutput("matchup_team_text_1")),
            DTOutput("matchup_mutual_opponents_1")
          ),
          card(
            card_header(textOutput("matchup_team_text_2")),
            DTOutput("matchup_mutual_opponents_2")
          ),
        )
      )
    )
  )

algo_nav_panel <- 
  nav_panel(
    title = "Algorithm",
    
    layout_column_wrap(
      card(
        max_height = "100%",
        withMathJax(includeMarkdown("algorithm/algorithm_explainer.qmd"))),
      card(
        max_height = "100%",
        navset_card_tab(
          full_screen = T,
          title = "Rating",

          nav_panel(
            "Table",
            DTOutput("rating_point_diff_dt")
          ),
          nav_panel(
            "Plot",
            plotlyOutput("rating_point_diff_plot")
          ),
          ),
        navset_card_tab(
          full_screen = T,
          title = "Weight",
          
          nav_panel(
            "Table",
            DTOutput("weight_point_diff_dt")
          ),
          nav_panel(
            "Plot",
            plotlyOutput("weight_point_diff_plot")
          ),
        )
      )
    )
  )
