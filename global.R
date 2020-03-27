
suppressPackageStartupMessages({
  library(shiny)
  library(tippy)
  library(curl)
  library(httr)
  library(httpuv)
  library(jsonlite)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(dplyr)
  library(glue)
  library(stringr)
  library(lubridate)
  library(scales)
  library(diptest)
  library(DT)
  library(grid)
  library(ggplot2)
  library(ggrepel)
  library(ggiraph)
  library(visNetwork)
  library(showtext)
})

api <- "https://api.spotify.com/v1"
scope <- "user-library-read,user-top-read"
host <- "127.0.0.1" # whitelisted on Spotify
port <-1410

options(shiny.host=host, shiny.port=port)

endpoint <- oauth_endpoint(authorize="https://accounts.spotify.com/authorize",
                           access="https://accounts.spotify.com/api/token")

app <- oauth_app(appname="",
                 key=Sys.getenv("SPOTIFY_CLIENT_ID"),
                 secret=Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                 redirect_uri=if (interactive()) glue("http://{host}:{port}") else "https://ampu3ro.shinyapps.io/spotivibe")

features_all <- tibble(
  valence=c(330, 376, 442, 558, 515, 595, 605, 640, 555, 577, 593, 635, 530, 548, 496, 490, 475, 398, 362, 322),
  energy=c(186, 153, 202, 228, 245, 309, 340, 397, 489, 559, 598, 658, 640, 752, 769, 807, 734, 725, 712, 544),
  danceability=c(17, 35, 72, 159, 186, 252, 362, 452, 597, 754, 954, 1035, 1041, 1064, 1020, 835, 638, 339, 148, 72),
  instrumentalness=c(7647, 229, 145, 83, 104, 104, 83, 83, 83, 62, 62, 83, 83, 104, 125, 166, 249, 353, 436, 166),
  acousticness=c(3075, 838, 612, 511, 419, 369, 335, 327, 285, 277, 277, 268, 285, 277, 218, 260, 302, 310, 327, 444),
  speechiness=c(4718, 2641, 769, 423, 321, 269, 218, 167, 128, 77, 38, 38, 26, 26, 26, 26, 26, 38, 64, 192),
  liveness=c(247, 2475, 2928, 971, 653, 533, 621, 454, 191, 119, 95, 88, 103, 119, 95, 64, 80, 80, 103, 135),
  loudness=c(0, 0, 0, 15, 15, 15, 39, 46, 62, 77, 131, 193, 324, 509, 926, 1597, 2739, 2840, 586, 23),
  tempo=c(19, 5, 5, 14, 38, 165, 804, 1134, 1392, 1171, 1731, 1327, 790, 536, 532, 268, 89, 56, 9, 0)
)
# https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/
# http://www.graphreader.com/

feature_cols <- names(features_all)

feature_bins <- list(
  valence=seq(0, 1, 0.05),
  energy=seq(0, 1, 0.05),
  danceability=seq(0, 1, 0.05),
  instrumentalness=seq(0, 1, 0.05),
  acousticness=seq(0, 1, 0.05),
  speechiness=seq(0, 1, 0.05),
  liveness=seq(0, 1, 0.05),
  loudness=seq(-60, 0, 3),
  tempo=seq(0, 220, 11)
)

feature_labels <- c(
  valence="more positive (happy, cheerful, euphoric)",
  energy="more energic (fast, loud, noisy)",
  danceability="more danceable (stable tempo & rhythm)",
  instrumentalness="more instrumental, less vocal",
  acousticness="more acoustic",
  speechiness="more spoken",
  liveness="more likely live",
  loudness="psychologically louder",
  tempo="faster / higher BPM"
)

color <- list(black="#040404",
              slate="#282828",
              grey="#909090",
              valence="#16e68b",
              energy="#ccf462",
              danceability="#b02a97",
              instrumentalness="#9cf0e1",
              acousticness="#ff4633",
              speechiness="#ffcfd6",
              liveness="#ef1e31",
              loudness="#4102f7",
              tempo="#c97d55")

bull <- map2(color, names(color), function(x, y) {
  glue("<span style=color:{x};font-size:20pt;font-size:3em;line-height:22px;vertical-align:-8px>&bull;</span> <strong>{y}</strong>")
})

feature_definitions <- glue("{bull$valence} (0-1): mood, from sad/angry to happy/cheerful<br>",
                            "{bull$energy} (0-1): perceptual intensity and activity<br>",
                            "{bull$danceability} (0-1): suitability for dancing<br>",
                            "{bull$instrumentalness} (0-1): lack of vocals<br>",
                            "{bull$acousticness} (0-1): confidence that the track is acoustic<br>",
                            "{bull$speechiness} (0-1): presence of spoken word<br>",
                            "{bull$liveness} (0-1): likelihood of the presence of an audience<br>",
                            "{bull$loudness} (-60-0 dB): psychological amplitude/strength<br>",
                            "{bull$tempo} (0-220 BPM): speed, average beats per minute")

theme <- "www/spotify.css"
css_tooltip <- "background-color:gray; color:white; opacity:90%"
css_selection <- css_hover <- "fill:white; color:white"

rgba_string <- function(hex, alpha=0.8) {
  paste0("rgba(", glue_collapse(c(grDevices::col2rgb(hex)[,1], alpha), ", "), ")")
}

nova <- "ProximaNova-Medium"
font_add(family=nova, regular=glue("www/fonts/{nova}.ttf"))
showtext_auto()

if (.Platform$OS.type == "windows")
  grDevices::windowsFonts("ProximaNova-Medium"=grDevices::windowsFont(nova))

element_text_nova <- function(size=12, ...) element_text(family=nova, color="white", size=size, ...)
rect <- element_rect(fill=color$slate, color=NA)
rect_black <- element_rect(fill=color$black)
blank <- element_blank()
arrow <- grid::arrow(length=grid::unit(0.1, "inches"))

theme_spotify <- function(...) {
  theme(rect=rect,
        text=element_text_nova(),
        plot.background=rect,
        panel.background=rect,
        panel.grid=blank,
        title=element_text_nova(),
        strip.text=element_text_nova(),
        strip.background=rect,
        legend.position="top",
        legend.direction="horizontal",
        legend.key=blank,
        legend.text=element_text_nova(),
        axis.line=element_line(color="white", arrow=arrow),
        axis.title=element_text_nova(hjust=1, vjust=1),
        axis.text=blank,
        ...)
}

units_left <- 4
units_right <- 12 - units_left