source("ui/nav_panels.R")

addResourcePath("fonts", "./fonts")

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
      tags$style(".accordion-button {padding: 5px;}"),
      tags$style(".accordion-body {padding: 0px; margin: 0px;}"),
      tags$style(
        HTML("
                @media (max-width: 575.98px) {
                  .main {
                    padding: 5px !important;
                    padding-left: 40px !important;
                  }
                }
              ")
      ),
      tags$style(HTML(
        "
        .nav.navbar-nav .form-group.shiny-input-container {margin-bottom: 0; height: 38px;}
        .nav.navbar-nav .form-group.shiny-input-container > label {display: inline;}
        "
      )),
      tags$style(HTML('.bslib-value-box .value-box-title {font-size:18px; !important}'))
    ),
    page_navbar(
      id = "main_tab",
      # Theming
      theme = 
        bs_theme(
          bootswatch = "lux", version = 5,
          primary = color_primary_light,
          secondary = "#545454",
          "navbar-bg" = "#000",
          "nav-link-color" = "#FC0 !important",
          "nav-link-font-size" = "25px",
          "nav-link-font-weight" = "normal",
          "nav-text-color" = "#fc0 !important",
          "nav-link-hover-color" = "#fc0 !important",
          base_font = font_face(family = "Bebas Neue",
                                src = "url('/fonts/BebasNeue-Regular.ttf') format('truetype')")
        ),
      # Title
      title=img(
        src = "European_Ultimate_Federation_EUF_Logo.png",
        height = 60
      ),
      window_title = "EUCS Ranking",
      
      #Sidebar
      sidebar = sidebar(
        open="open",
        bg = "#1E1E1E",
        selectInput(inputId = "division", "Division:",
                    choices = c("Mixed", "Open", "Women"), selected = "Mixed"),
        selectInput(inputId = "season", 
                    tooltip(trigger = list("Season:",
                                           bs_icon("info-circle")),
                            "The 2023 season did not use rankings to qualify teams for the EUCF. This data is here for demonstration purposes only."),
                    choices = game_data %>% pull(Season) %>% unique %>% sort(decreasing=T),
                    selected = game_data %>% pull(Season) %>% unique %>% max),
        uiOutput("select_ranking_date"),
        selectInput(inputId = "eligible_only", 
                    tooltip(trigger = list("Teams Counted in Ranking:",
                                           bs_icon("info-circle")),
                            "Only teams with 10 games or more are eligible to qualify for the EUCF. However, by default, all teams are shown in the ranking here."),
                    choices = c("All Teams", ">10 Games Only"))
      ),
    
      # Actual Content
      season_nav_panel,
      team_nav_panel,
      matchup_nav_panel,
      event_nav_panel,
      nav_menu( "EUCS",
        nav_item(a("Home",
                   href = "https://eucs.ultimatefederation.eu/",
                   target = "_blank"),
                 ),
        nav_item(a("Results",
                   href = "https://eucs-schedule.ultimatefederation.eu/",
                   target = "_blank")),
        nav_item(a("Video",
                   href = "https://live.ultimatefederation.eu/",
                   target = "_blank")),
        algo_nav_panel,
      )
    )
  )
}
