function(reqest){
  page_navbar(theme = 
                bs_theme(
                  bootswatch = "lux", version = 5,
                  primary = "#fc0",
                  secondary = "#545454",
                  "navbar-bg" = "#000",
                  "navbar-text-color" = "#fc0 !important",
                  "nav-link-color" = "#FC0 !important",
                  "nav-link-font-size" = "25px",
                  "nav-link-font-weight" = "normal",
                  "nav-text-color" = "#fc0 !important",
                  "nav-link-hover-color" = "#FC0 !important",
                  base_font = font_google("Bebas Neue")
                  
                ),
              title=img(
                src = "European_Ultimate_Federation_EUF_Logo.png",
                height = 80
              ),
              window_title = "EUCS Explorer",
              sidebar = sidebar(
                selectInput(inputId = "year", "Year:",
                            choices = c(2024)),
                selectInput(inputId = "division", "Division:",
                            choices = c("Mixed", "Open", "Women"), selected = "Mixed")
              ),
              nav_panel(title = "Season"),
              nav_panel(title = "Team"),
              nav_panel(title = "Event"),
              nav_spacer(),
              nav_item(a(bs_icon(name = "instagram"),
                         href="http://www.instagram.com/ultimatefederation_eu")),
              nav_item(a(bs_icon(name = "facebook"),
                         href="https://www.facebook.com/ultimate.eu/")),
              nav_item(a(bs_icon(name = "envelope"),
                         href="mailto:competition@ultimatefederation.eu"))
              
              
              
  )
}
