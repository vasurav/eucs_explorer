season_nav_panel <- 
  nav_panel(
    title = "Season",
    layout_columns(
      fill = F, fillable = F,
      value_box(title = card_title("Wildcards Awarded",
                                   tooltip(bs_icon("info-circle"),
                                           "Wildcard bids are awarded to teams who win Elite Invite and Summer Tour events regardless of ranking position.")),
                value = textOutput("wildcard_count"),
                showcase = bs_icon("suit-spade-fill"),
                theme = "primary"),
      value_box(title = card_title("Ranking Bids",
                                   tooltip(bs_icon("info-circle"),
                                           "Number of ranking spots that are guaranteed to get EUCF spots.")),
                value = textOutput("eucf_ranking_spots_guaranteed"),
                showcase = bs_icon("shield-fill-check"),
                theme = value_box_theme(bg = color_eucf_guaranteed)),
      value_box(title = card_title("Unassigned Wildcards",
                                   tooltip(bs_icon("info-circle"),
                                           "Ranking spots that are likely to get EUCF spots. 
                                           If wildcards are awarded to teams outside the top 16, 
                                           these spots will no longer qualify teams to the EUCF.")),
                value = textOutput("eucf_ranking_spots_likely"),
                showcase = bs_icon("patch-question-fill"),
                theme = value_box_theme(bg = color_eucf_likely))
      
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
        title = "Connections",
        simpleNetworkOutput("season_network")
      )
    )
  )

team_nav_panel <- 
  nav_panel(
    title = "Team",
    uiOutput("team_selector"),
    layout_columns(
      widths = 1/5, fill = F, fillable = F,
      value_box(title = 
                  card_title("Rank"), 
                showcase=bs_icon("reception-4"), 
                value = textOutput("team_rank"),
                theme="primary"),
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
                  card_title("Strength of Schedule",
                             tooltip(bs_icon("info-circle"),
                                     "Average rating of opponent.")), 
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
                               checkboxInput("team_eucf_cutoff_type", "Include Unassigned Wildcars?",
                                           value = F),
                               placement = "top"
                               )
                             ),
                showcase=bs_icon("ladder"),
                value= textOutput("distance_from_eucf_cutoff_rating"),
                theme = "primary")
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
      fill = F, fillable = F,
      uiOutput("matchup_team_1"),
      uiOutput("matchup_team_2")
    ),
    layout_columns(
      widths = 1/2,
      fill = F, fillable = F,
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
            DTOutput("matchup_mutual_opponents_1")
          ),
          card(
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
            "Plot",
            plotlyOutput("rating_point_diff_plot")
          ),
          nav_panel(
            "Table",
            DTOutput("rating_point_diff_dt")
          )
          ),
        navset_card_tab(
          full_screen = T,
          title = "Weight",
          nav_panel(
            "Plot",
            plotlyOutput("weight_point_diff_plot")
          ),
          nav_panel(
            "Table",
            DTOutput("weight_point_diff_dt")
          )
        )
      )
    )
  )
