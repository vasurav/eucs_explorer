function(reqest){
  page_navbar(theme = 
                bs_theme(
                  bootswatch = "lux", version = 5,
                  primary = "#fc0",
                  secondary = "#545454",
                  "navbar-bg" = "#000",
                  "navbar-text-color" = "#fc0 !important",
                  "nav-link-color" = "#FC0 !important",
                  "nav-link-hover-color" = "#FC0 !important"
                  
                ),
              title=img(
                src = "European_Ultimate_Federation_EUF_Logo.png",
                height = 50
              ),
              sidebar = sidebar(
                selectInput(inputId = "year", "Year:",
                            choices = c(2024)),
                selectInput(inputId = "division", "Division:",
                            choices = c("Mixed", "Open", "Women"), selected = "Mixed")
              ),
              nav_item("EUCS Explorer"),
              nav(title = "Season"),
              nav(title = "Team"),
              nav(title = "Event"),
              nav_spacer(),
              nav_item(a(bs_icon(name = "instagram"),
                         href="http://www.instagram.com/ultimatefederation_eu")),
              nav_item(a(bs_icon(name = "facebook"),
                         href="https://www.facebook.com/ultimate.eu/")),
              nav_item(a(bs_icon(name = "envelope"),
                         href="mailto:competition@ultimatefederation.eu"))
              
              
              
  )
}
