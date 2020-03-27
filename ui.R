
fluidPage(
  tags$head(includeCSS(theme)),
  fluidPage(
    title="spotivibe",
    img(src="spotivibe.png", height="64px", style="padding-top:24px"),
    tags$script("Shiny.addCustomMessageHandler('redirect', function(url) {location.replace(url)});"),
    div(id="ppitest", style="width:1in"),
    tags$script(src="resize.js"),
    br(),
    div("Spotify analyzes every track in its library and summarizes each with",
        uiOutput("metrics", inline=T),
        tippy_this("metrics", feature_definitions, maxWidth="500px", theme="gray"),
        "that it makes available through its",
        a("web API", href="https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/", target="_blank"),".",
        "SpotiVibe lets you explore your library (up to 10k tracks), highlighting those most played and comparing the distribution of values",
        "for each feature against the general population. This lets you see, for example, how happy/sad your music is and",
        "how that mood changes over time."),
    br(),
    radioButtons("dataset", NULL, c("use data from your Spotify library*"="yours", "use example data from my library"="mine"), "mine", inline=T),
    helpText(style="font-size:8pt",
             "*uses", a("oauth2.0", href="https://oauth.net/2/"), "to authenticate and only stores data in memory while the app is running"),
  ),
  uiOutput("ui")
)
