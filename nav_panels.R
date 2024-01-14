season_nav_panel <- 
  nav_panel(
    title = "Season",
    card(
      tabsetPanel(
        tabPanel(
          title = "Ranking",
          DTOutput("season_ranking_table")
        ),
        tabPanel(
          title = "Games",
          DTOutput("season_games_table")
        )
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
                  card_title("Strenght of Schedule",
                             tooltip(bs_icon("info-circle"),
                                     "Average rating of opponent.")), 
                showcase=bs_icon("lightning-fill"), 
                value = textOutput("team_strength_of_schedule"), 
                theme="primary"),
      value_box(title = 
                  card_title("To EUCF",
                             tooltip(
                               bs_icon("info-circle"),
                               "If positive: number of rating points needed to qualify for the EUCF.
                               If negative: the amount of points the team can lose and still qualify for the EUCF."
                             )),
                showcase=bs_icon("ladder"),
                value= textOutput("distance_from_eucf_cutoff_rating"),
                theme = "primary")
    ),
    card(
      tabsetPanel(
        tabPanel(
          "Games",
          DTOutput("team_games_table")
        ),
        tabPanel(
          "Roster"
        ),
        tabPanel(
          "Ranking Evolution"
        )
      )
    )
  )

algo_nav_panel <- 
  nav_panel(
    title = "Algorithm",
    layout_column_wrap(
      card(
        withMathJax(includeMarkdown("algorithm_explainer.qmd"))),
      card(
        navset_card_tab(
          nav(
            "Rating - Plot",
            plotlyOutput("algo_point_diff_plot")
          ),
          nav(
            "Rating-Table",
            "Sample Text"
          )
          ),
        navset_card_tab(
          nav(
            "Weight - Plot",
            plotlyOutput("weight_point_diff_plot")
          ),
          nav(
            "Weight - Table",
            "Sample Text"
          )
        )
      )
    )
  )
