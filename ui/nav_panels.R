games_tooltip <- function()
{
  tooltip(trigger = list("Games", bs_icon("info-circle")),
          "Games in red are not counted because they fall under the blowout rule. See the 'Algorithm' tab above for an explanation on the blowout rule.")
}

season_nav_panel <- 
  nav_panel(
    title = "Season",
    accordion(
      accordion_panel("",
                      layout_columns(
                        fill = F, fillable = T,
                        value_box(title = span(tooltip(trigger = list("EUCF Bids",
                                                                      bs_icon("info-circle")),
                                                       "Number of total spots at EUCF. Ranking spots in green are guaranteed to qualify teams for the EUCF.")),
                                  value = textOutput("eucf_bids"),
                                  showcase = bs_icon("trophy-fill"),
                                  theme = value_box_theme(bg = color_eucf_guaranteed_dark)),
                        value_box(title = span(tooltip(trigger = list("EUCF Wildcards",
                                                  bs_icon("info-circle")),
                                                             "Wildcard bids are awarded to teams who win Elite Invite and Summer Tour events regardless of ranking position. 
                                                  Teams in yellow are guaranteed to qualify for the EUCF regardless of ranking.")),
                                  value = textOutput("wildcard_count"),
                                  showcase = bs_icon("suit-spade-fill"),
                                  theme = "primary"),
                        value_box(title = span(tooltip(trigger = list("EUCF 2 Bids",
                                                                      bs_icon("info-circle")),
                                                       "Number of ranking spots for the EUCF Division 2. Ranking Spots in blue are guaranteed to go to EUCF Division 2.")),
                                  value = textOutput("eucf2_bids"),
                                  showcase = bs_icon("2-circle-fill"),
                                  theme = value_box_theme(bg = color_eucf2_dark)),
                        value_box(title = span(tooltip(trigger = list("Potential Bids",
                                                                      bs_icon("info-circle")),
                                                       "The number of potential spots is equal to the number of unassigned wildcards. 
                                           Ranking spots in gray are not guaranteed to go to a certain EUCF event. 
                                           Gray spots between blue and green could go to EUCF or EUCF Division 2 depending on where the wildcards finish. 
                                           Gray spots below blue could go to EUCF Division 2 or not qualify for anything depending on where the wildcards finish.")),
                                  value = textOutput("eucf_potential_bids"),
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
        title =  games_tooltip(),
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
            selected="Entire Season"),
          selectInput(
            "season_evolution_type",
            "What to track?",
            choices = c("Ranking", "Rating")
          )
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
                        value_box(title = h5("Rank"), 
                                  showcase=bs_icon("reception-4"), 
                                  value = textOutput("team_rank"),
                                  theme="primary",
                                  showcase_layout = showcase_left_center(max_height = "50%")),
                        value_box(title = h5("Rating"), 
                                  showcase=bs_icon("heart-half"), 
                                  value = textOutput("team_rating"), 
                                  theme="primary"),
                        value_box(title = h5("Record"), 
                                  showcase=bs_icon("rulers"), 
                                  value = textOutput("team_record"), 
                                  theme="primary"),
                        value_box(title = span(tooltip(trigger = list("S.O.S.",
                                               bs_icon("info-circle")),
                                                       "Strength of Schedule: Average rating of opponent.")), 
                                  showcase=bs_icon("lightning-fill"), 
                                  value = textOutput("team_strength_of_schedule"), 
                                  theme="primary"),
                        value_box(title = span(tooltip(trigger = list("To EUCF Cutoff",
                                                                 bs_icon("info-circle")), 
                                                  options = list(html = T),
                                                  HTML("If positive: number of rating points needed to reach the current EUCF cutoff rating. <br> <br>
                               If negative: the amount of points the team can lose and still be above the EUCF cutoff rating."
                                                       )
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
        "Roster",
        layout_column_wrap(
          card(card_title("Master"),
               DTOutput("team_master_roster")
          ),
          card(card_title("Event"),
               uiOutput("team_event_select"),
               DTOutput("team_event_roster"))
        )
      ),
      nav_panel(
        title = games_tooltip(),
        DTOutput("team_games_table")
      ),
      nav_panel(
        "Plot",
        plotlyOutput("team_games_plot")
      ),
      nav_panel(
        "Evolution",
        plotlyOutput("team_evolution_plot")
      ),
      nav_panel(
        "Connections",
        numericInput("team_connection_depth", label = "Depth:", min=1, step =1, value=1),
        simpleNetworkOutput("team_connection")
      )
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
                        value_box(title = h5("Rating Difference"),
                                  value = textOutput("matchup_rating_diff"),
                                  showcase = bs_icon("clipboard-heart"),
                                  theme = "primary"),
                        value_box(title = span(tooltip(trigger = list("Expected Score",
                                                     bs_icon("info-circle")),
                                                  options = list(html = T),
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
        "History",
        DTOutput("matchup_game_history")
      ),
      nav_panel(
        "Games",
        checkboxInput("matchup_mutual_only",
                      "Mutual Opponents Only",
                      value = T),
        layout_column_wrap(
          card(
            card_header(htmlOutput("matchup_team_text_1")),
            DTOutput("matchup_mutual_opponents_1")
          ),
          card(
            card_header(htmlOutput("matchup_team_text_2")),
            DTOutput("matchup_mutual_opponents_2")
          ),
        )
      ),
      nav_panel("Plot",
                plotlyOutput("matchup_plot"))
    )
  )

event_nav_panel <- 
  nav_panel(
    title = "Event",
    uiOutput("event_select_ui"),
    
    accordion(
      accordion_panel("",
                      layout_column_wrap(
                        fill = F, fillable = T,
                        value_box(
                          title = h5("Location"),
                          value = textOutput("event_location"),
                          showcase = bs_icon("globe-europe-africa"),
                          theme="primary"),
                        value_box(
                          title = h5("Date"),
                          value = textOutput("event_start_date"),
                          showcase = bs_icon("calendar-date"),
                          theme="primary"),
                        value_box(
                          title = h5("Roster Deadline"),
                          value = textOutput("event_deadline_roster"),
                          showcase = bs_icon("calendar-check"),
                          theme="primary"),
                        
                      )
      )
      
    ),
    
    layout_column_wrap(
    card(
      title = "Teams",
      DTOutput("event_input_division_teams")
    ),
    card(
      title = "Rosters",
      uiOutput("event_team_select"),
      DTOutput("event_team_roster")
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


