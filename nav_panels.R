season_nav_panel <- 
  nav_panel(title = "Season",
            card(tabsetPanel(
              tabPanel(title = "Ranking",
                       tags$style("#season_ranking_table td {padding: 0}"),
                       DTOutput("season_ranking_table")),
              tabPanel(title = "Game",
                       tags$style("#season_games_table td {padding: 0}"),
                       DTOutput("season_games_table"))
            ))
  )

team_nav_panel <- nav_panel(title = "Team")

# event_nav_panel <- nav_panel(title = "Event")