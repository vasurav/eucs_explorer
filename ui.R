source("ui/nav_panels.R")

function(reqest){
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
      ), # %>%
      # bs_add_rules(
      #   list(
      #     ".datatables {td {padding-top: 1px; padding-bottom: 1px;}}",
      #     ".nav-tabs .nav-link {color: #000000; font-size: 18px;}",
      #     ".nav-tabs .nav-link:hover {color:  #ddaa00; font-size: 18px;}",
      #     ".card-body a {color: red}",
      #     ".card-body a:hover {color: #ddaa00}",
      #     '.card-body p, ol {font-family: "Helvetica Neue",Arial,sans-serif !important}'
      #   )
      # ),
    tags$style(".datatables td {padding-top: 1px; padding-bottom: 1px;}"),
    tags$style(".nav-tabs .nav-link {color: #000000; font-size: 18px;}",
               ".nav-tabs .nav-link:hover {color: #ddaa00; font-size: 18px;}"),
    tags$style(".card-body a {color: #000000}",
               ".card-body a:hover {color: #ddaa00}"),
    tags$style('.card-body p, ol {font-family: "Helvetica Neue",Arial,sans-serif !important}'),
    
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
      selectInput(inputId = "division", "Division:",
                  choices = c("Mixed", "Open", "Women"), selected = "Mixed"),
      hr(),
      h4("Ranking"),
      selectInput(inputId = "eligible_only", "How many games?",
                  choices = c("All Teams", ">10 Games Only")),
      checkboxInput(inputId = "include_wildcard", "Include Wildcards?", value = T),
      hr(),
      numericInput("eucf_cutoff", tooltip(trigger = list("EUCF Cutoff",
                                                         bs_icon("info-circle")),
                                          "Sets the EUCF Cutoff for calculations on the Team page. Defaults to 16."),
                   value = 16,
                   min = 1, max = 30, step = 1,
                   width = "100%")
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
}
