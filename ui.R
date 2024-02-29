source("ui/nav_panels.R")

function(reqest){
  tagList(
    tags$head(
      tags$style(".dataTable td {padding-top: 0px; padding-bottom: 0px;} !important"),
      tags$style(".nav-tabs .nav-link {color: #000000; font-size: 18px;}",
                 ".nav-tabs .nav-link:hover {color: #ddaa00; font-size: 18px;}"),
      tags$style(".card-body a {color: #000000}",
                 ".card-body a:hover {color: #ddaa00}"),
      tags$style('.card-body p, ol {font-family: "Helvetica Neue",Arial,sans-serif !important}'),
      tags$style('.tooltip {font-family: "Helvetica Neue",Arial,sans-serif !important}'),
      tags$style(
        HTML("
    @media (max-width: 575.98px) {
      .main {
        padding: 0 !important;
      }
    }
  ")),
      tags$style(HTML(
        '
        .nav.navbar-nav .form-group.shiny-input-container {margin-bottom: 0; height: 38px;}
        .nav.navbar-nav .form-group.shiny-input-container > label {display: inline;}
        '
      )),
    ),
    page_navbar(
      id = "main_tab",
      # Theming
      theme = 
        bs_theme(
          bootswatch = "lux", version = 5,
          primary = "#fc0",
          secondary = "#545454",
          "navbar-bg" = "#000",
          "nav-link-color" = "#FC0 !important",
          "nav-link-font-size" = "25px",
          "nav-link-font-weight" = "normal",
          "nav-text-color" = "#fc0 !important",
          "nav-link-hover-color" = "#fc0 !important",
          base_font = font_google("Bebas Neue"),
        ),
      # Title
      title=img(
        src = "European_Ultimate_Federation_EUF_Logo.png",
        height = 60
      ),
      window_title = "EUCS Explorer",
      
      #Sidebar
      sidebar = sidebar(
        selectInput(inputId = "season", "Season:",
                    choices = game_data %>% pull(Season) %>% unique),
        selectInput(inputId = "ranking_date", "Ranking Date:",
                    choices = 
                      game_data %>% 
                      # filter(Season == input$Season) %>% 
                      pull(Ranking_Calculation_Date) %>% 
                      unique %>% 
                      sort(decreasing=T)
        ),
        # selectInput(inputId = "division", "Division:",
        #             choices = c("Mixed", "Open", "Women"), selected = "Mixed"),
        selectInput(inputId = "eligible_only", 
                    tooltip(trigger = list("Teams Counted in Ranking:",
                                           bs_icon("info-circle")),
                            "Only teams with 10 games or more are eligible to qualify for the EUCF. However, by default, all teams are shown in the ranking here."),
                    choices = c("All Teams", ">10 Games Only"))
      ),
      
      nav_item(
        collapsible = F,
        selectInput(inputId = "division", NULL,
                    choices = c("Mixed", "Open", "Women"), selected = "Mixed", width="120px")
      ),
      
      # Actual Content
      season_nav_panel,
      team_nav_panel,
      matchup_nav_panel,
      algo_nav_panel,
      
      # This spacer puts the socials on the right side of the screen
      nav_spacer(),
      
      # Socials
      nav_item(a(bs_icon(name = "instagram"),
                 href="http://www.instagram.com/ultimatefederation_eu")),
      nav_item(a(bs_icon(name = "facebook"),
                 href="https://www.facebook.com/ultimate.eu/")),
      nav_item(a(bs_icon(name = "envelope"),
                 href="mailto:competition@ultimatefederation.eu"))
    )
  )
}
