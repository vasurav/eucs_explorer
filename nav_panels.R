season_nav_panel <- 
  nav_panel(
    title = "Season",
    card(
      tabsetPanel(
        tabPanel(
          title = "Ranking",
          tags$style("#season_ranking_table td {padding-top: 1px; padding-left:5px, padding-right:5px;padding-bottom: 1px;}"),
          DTOutput("season_ranking_table")
        ),
        tabPanel(
          title = "Games",
          tags$style("#season_games_table td {padding-top: 1px; padding-left:5px, padding-right:5px;padding-bottom: 1px;}"),
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
      value_box(title = "Rank", 
                showcase=bs_icon("reception-4"), 
                value = textOutput("team_rank"), 
                theme="primary"),
      value_box(title = "Rating", 
                showcase=bs_icon("heart-half"), 
                value = textOutput("team_rating"), 
                theme="primary"),
      value_box(title = "Record", 
                showcase=bs_icon("rulers"), 
                value = textOutput("team_record"), 
                theme="primary"),
      value_box(title = "Strenght of Schedule", 
                showcase=bs_icon("lightning-fill"), 
                value = textOutput("team_strength_of_schedule"), 
                theme="primary"),
      value_box(title = "Distance from EUCF",
                showcase=bs_icon("ladder"),
                value= textOutput("distance_from_eucf_cutoff_rating"),
                theme = "primary")
    ),
    
    card(title = card_title("Games"),
         tags$style("#team_games_table td {padding-top: 1px; padding-left:5px, padding-right:5px;padding-bottom: 1px;}"),
         DTOutput("team_games_table"))
  )
