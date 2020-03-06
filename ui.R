
fluidPage(
  tags$head(includeCSS(theme)),
  fluidPage(
    titlePanel("SpotiVibe"),
    tags$script("Shiny.addCustomMessageHandler('redirect', function(url) {location.replace(url)});"),
    br(),
    div("Spotify analyzes every track in its library and summarizes each with 9 quantitaive metrics that it makes available through its",
        a("web API", href="https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/", target="_blank"),".",
        "SpotiVibe lets you explore your library (up to 10k tracks), highlighting those most played and comparing the distribution of values",
        'for each feature against the general population. This lets you see, for example, how happy/sad ("valence") your music is and',
        "how that mood changes over time."),
  ),
  uiOutput("ui")
)
