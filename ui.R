source("nav_panels.R")

function(reqest){
  page_navbar(
    # Theming
    theme = 
      bs_theme(
        bootswatch = "lux", version = 5,
        primary = "#fc0",
        secondary = "#545454",
        "navbar-bg" = "#000",
        #"navbar-text-color" = "#fc0 !important",
        "nav-link-color" = "#FC0 !important",
        "nav-link-font-size" = "25px",
        "nav-link-font-weight" = "normal",
        "nav-text-color" = "#fc0 !important",
        "nav-link-hover-color" = "#FC0 !important",
        base_font = font_google("Bebas Neue")
      ),
    tags$style(".datatables td {padding-top: 1px; padding-bottom: 1px;}"),
    tags$style(".nav-tabs .nav-link {color: #000000; font-size: 18px;}"),
    
    # Title
    title=img(
      src = "European_Ultimate_Federation_EUF_Logo.png",
      height = 80
    ),
    window_title = "EUCS Explorer",
    
    #Sidebar
    sidebar = sidebar(
      selectInput(inputId = "season", "Season:",
                  choices = game_data %>% pull(Season) %>% unique),
      selectInput(inputId = "division", "Division:",
                  choices = c("Mixed", "Open", "Women"), selected = "Mixed"),
      selectInput(inputId = "eligible_only", "Which Teams in Ranking?",
                  choices = c("All Teams", "10+ Games Only"))
    ),
    
    # Actual Content
    season_nav_panel,
    team_nav_panel,
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
