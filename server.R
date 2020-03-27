
shinyServer(function(input, output, session) {
  
  react <- reactiveValues()
  
  observe({
    react$code <- parseQueryString(session$clientData$url_search)$code
  })
  
  output$metrics <- renderUI({
    span(style="text-decoration-line: underline; text-decoration-style: dotted;", "9 quantitative metrics")
  })
  
  output$ui <- renderUI({
    if (is.null(react$code) && input$dataset == "yours") {
      fluidPage(
        br(),
        actionButton("connect", "Connect to Spotify", icon=icon("sign-in-alt"))
      )
    } else if (!is.null(react$tracks)) {
      fluidPage(tags$head(includeCSS(theme)),
                hr(),
                fluidRow(
                  column(units_left,
                         br(),
                         div(class="inline",
                             "Start by selecting a single feature like"),
                         div(class="inline",
                             selectInput("feature", NULL, feature_cols, "valence", width="130px")),
                         div(style="clear:both"),
                         br(),
                         div(class="inline",
                             "Then adjust the time, using"),
                         div(class="inline",
                             selectInput("metric", NULL, c("track count"="n", "median value"="median"), width="140px")),
                         div("to find a month with a jump. Click on a point below to reflect your library at that time."),
                         div(style="clear:both"),
                         br(),
                         ggiraphOutput("month", height="auto"),
                         br(),
                         span("Bars show track counts for ranges of"),
                         textOutput("feature_text", inline=T),
                         span("values, and points are sample tracks in those ranges with top tracks shaded darker.",
                              "Click on one to hear a sample and get a feel for the feature."),
                         div(style="height:1em"),
                         uiOutput("summary")
                  ),
                  column(units_right,
                         br(),
                         ggiraphOutput("distribution", height="auto"),
                         div(style="height:0.5em"),
                         htmlOutput("player"))
                ),
                hr(),
                fluidRow(
                  column(units_left,
                         br(),
                         div("Another interesting aspect to explore is the network of collaborations between artists in your library."),
                         br(),
                         div(class="inline",
                             "Pick an artist like"),
                         div(class="inline",
                             selectInput("artist_id", NULL, NULL, width="150px")),
                         div("or find one with a higher number of direct collaborations by selecting a bar below."),
                         div(style="clear:both"),
                         ggiraphOutput("artist", height="auto"),
                         span("Nodes (bubbles representing artists) are sized relative to artist popularity and edges",
                              "(lines representing tracks) relative to track"),
                         textOutput("feature_text2", inline=T),
                         div(style="height:1em"),
                         div("Hover over the nodes to show artist genres and over the edges to show track names.",
                             "Click on an edge to play a sample of that track if available. Scroll to zoom in/out and drag nodes",
                             "around to reposition the network.")),
                  column(units_right,
                         br(),
                         visNetworkOutput("network", height="500px"),
                         div(style="height:2em"),
                         htmlOutput("player2"))
                ),
                hr()
      )
    }
  })
  
  output$feature_text <- renderText(input$feature)
  output$feature_text2 <- renderText(glue("{input$feature}."))
  
  observeEvent(input$connect, {
    url <- oauth2.0_authorize_url(endpoint, app, scope=scope)
    session$sendCustomMessage("redirect", url)
  })
  
  observe({
    if (is.null(react$code) || !is.null(react$data) || input$dataset == "mine")
      return()
    
    credentials <- oauth2.0_access_token(endpoint, app, react$code)
    token <- oauth2.0_token(endpoint, app, scope, credentials=credentials, cache=F, use_oob=F)
    
    get_json <- function(url) {
      response <- GET(url, token)
      
      if (status_code(response) == 429) {
        retry_after <- headers(response)$`Retry-After`
        setProgress(detail=glue("API query limits reached, retrying after {retry_after} seconds"))
        
        Sys.sleep(retry_after)
        response <- GET(url, token)
      }
      
      stop_for_status(response)
      fromJSON(httr::content(response, "text", encoding="UTF-8"), flatten=T)
    }
    
    split_comma <- function(x, n) {
      x <- split(x, ceiling(seq_along(x) / n))
      map_chr(x, glue_collapse, ",")
    }
    
    median_estimate <- function(y, x=feature_bins[[1]]) {
      n <- sum(y)
      m <- max(which(cumsum(y / n) <= .5)) # left median group
      x[m] + (n / 2 - cumsum(y)[m-1]) / y[m] * diff(x)[1]
    }
    #https://math.stackexchange.com/questions/2591946/how-to-find-median-from-a-histogram
    
    withProgress({
      setProgress(0.1, "retrieving track metadata")
      
      tracks_saved <- NULL
      url <- glue("{api}/me/tracks?limit=50")
      i <- 0
      
      while (!is.null(url) && i < 200) {
        json <- get_json(url)
        tracks_saved <- rbind(tracks_saved, json$items)
        url <- json$`next`
        i <- i+1
        if (i %% 10 == 0)
          setProgress(0.1 + i/100)
      }
      
      tracks_saved <- tracks_saved %>%
        as_tibble() %>%
        rename_all(~str_replace(., "track\\.", ""))
      
      tracks_top <- c("short", "medium", "long") %>%
        map_dfr(function(term) {
          glue("{api}/me/top/tracks?time_range={term}_term&limit=10") %>%
            get_json() %>%
            pluck("items") %>%
            mutate(term=paste0("top_",term))
        }) %>%
        as_tibble()
      
      setProgress(0.4, "retrieving track features")
      
      features <- c(tracks_saved$id, tracks_top$id) %>%
        unique() %>%
        split_comma(100) %>%
        map_dfr(function(ids) {
          glue("{api}/audio-features/?ids={ids}") %>%
            get_json() %>%
            pluck("audio_features")
        }) %>%
        as_tibble() %>%
        drop_na()
      
      setProgress(0.6, "retrieving artist metadata")
      
      id <- unique(bind_rows(tracks_saved$artists)$id)
      url <- paste0(api, "/artists/?ids=", split_comma(id, 50))
      artists <- as_tibble(map_dfr(url, function(url) get_json(url)$artists))
      
      setProgress(0.7, "formatting")
      
      bars_all <- features_all %>%
        mutate(bin=seq(nrow(.))) %>%
        pivot_longer(one_of(feature_cols), names_to="feature") %>%
        mutate(feature=factor(feature, feature_cols)) %>%
        group_by(feature) %>%
        mutate(scaled=value / max(value))
      
      stat_all <- features_all %>%
        summarize(valence=median_estimate(valence),
                  danceability=median_estimate(danceability),
                  energy=median_estimate(energy),
                  tempo=median_estimate(tempo, feature_bins$tempo)) %>%
        pivot_longer(everything(), names_to="feature", values_to="median")
      
      tracks_top_wide <- tracks_top %>%
        mutate(artist_name1=map_chr(artists, function(x) x$name[1]),
               count=1) %>%
        pivot_wider(one_of("name","artist_name1"), names_from=term, values_from=count)
      
      tracks <- tracks_saved  %>%
        transmute(id,
                  name,
                  added_at=as_datetime(added_at),
                  added_date=as_date(added_at),
                  added_month=floor_date(added_date, "month"),
                  artists,
                  artist_name1=map_chr(artists, function(x) x$name[1]),
                  artist_names=map_chr(artists, function(x) glue_collapse(x$name, ", ")),
                  album_name=album.name,
                  preview_url,
                  popularity,
                  tooltip=paste0(artist_names, " - ", name, ifelse(is.na(added_date), "", paste("<br>added ", added_date)))) %>%
        arrange(added_date) %>%
        distinct(name, artist_name1, .keep_all=T) %>% # different versions of the same song
        inner_join(select(features, one_of("id", feature_cols)), "id") %>%
        left_join(tracks_top_wide, c("name", "artist_name1")) %>%
        replace_na(list(top_short=0, top_medium=0, top_long=0))
      
      tracks_long <- tracks %>%
        pivot_longer(one_of(feature_cols), names_to="feature") %>%
        mutate(feature=factor(feature, feature_cols)) %>%
        group_by(feature) %>%
        mutate(bin=cut(value, 20, F)) %>%
        ungroup()
      
      tracks_long <- tracks %>%
        mutate(loudness=rescale(loudness, c(0, 1), range(feature_bins$loudness)),
               tempo=rescale(tempo, c(0, 1), range(feature_bins$tempo))) %>%
        pivot_longer(one_of(feature_cols), names_to="feature", values_to="scaled") %>%
        select(scaled) %>%
        bind_cols(tracks_long)
      
      setProgress(0.8, "creating new features")
      
      stat_long <- tracks_long$added_month %>%
        unique() %>%
        map_dfr(function(date) {
          tracks_long %>%
            filter(added_month <= date) %>%
            group_by(feature) %>%
            summarize(n=n(), median=median(value), mean=mean(value), dip=dip(value)) %>%
            mutate(added_month=date)
        }) %>%
        pivot_longer(c("n", "median", "mean"), names_to="metric") %>%
        group_by(feature, metric) %>%
        mutate(change=c(NA, diff(value))/lag(value), 
               tooltip=paste(format(added_month, "%b '%y"), ifelse(is.na(change), "", paste0(ifelse(change>0, "+", ""), percent(change, 1))))) %>%
        ungroup()
      
      track_artists <- tracks %>%
        select(track_id=id, track_name=name, artist_names, artists) %>%
        unnest(artists) %>%
        group_by(track_id) %>%
        transmute(artist_id=id, track_name, artist_names, n_artists=n(), index=seq(n())) %>%
        filter(n_artists > 1)
      
      collaborators <- track_artists$artist_id %>%
        unique() %>%
        map_dfr(function(x) {
          track_artists %>%
            ungroup() %>%
            filter(artist_id == x) %>%
            pull(track_id) %>%
            unique() %>%
            map_dfr(function(y) {
              track_artists %>%
                filter(track_id %in% y & !artist_id %in% x)
            }) %>%
            distinct(artist_id) %>%
            rename(collaborator_id=artist_id) %>%
            mutate(id=x)
        }) %>%
        left_join(artists, "id")
        
      suppressMessages({
        combinations <- track_artists$n_artists %>%
          unique() %>%
          map_dfr(function(x) {
            utils::combn(x, 2) %>%
              t() %>%
              as_tibble(.name_repair="unique") %>%
              rename(!!c(from="...1", to="...2"))
          }, .id="n") %>%
          mutate(n_artists=as.integer(n)+1)
      })
      
      edges_all <- track_artists %>%
        group_split() %>%
        map_dfr(function(x) {
          combinations %>%
            inner_join(x, c(n_artists="n_artists", from="index")) %>%
            transmute(n_artists, from=artist_id, to) %>%
            inner_join(x, c(n_artists="n_artists", to="index")) %>%
            transmute(track_id, track_name, artist_names, from, to=artist_id)
        }) %>%
        mutate(id=seq(n()))
      
      obj <- c("tracks_saved", "tracks_top", "features", "artists", "bars_all", "stat_all", "stat_long",
               "tracks", "tracks_long", "track_artists", "collaborators", "edges_all")
      
      env <- environment()
      
      react$data <- sapply(obj, get, env=env, simplify=F)
    })
  })
  
  observe({
    data <- if (input$dataset == "mine") readRDS("data.rds") else react$data
    
    react$artists <- data$artists
    react$stat_all <- data$stat_all
    react$stat_long <- data$stat_long
    react$bars_all <- data$bars_all
    react$tracks <- data$tracks
    react$tracks_long <- data$tracks_long
    react$track_artists <- data$track_artists
    react$collaborators <- data$collaborators
    react$edges_all <- data$edges_all
  })
  
  loading <- reactive(is.null(react$artists) || is.null(input$feature))
  
  observe({
    if (!loading()) {
      choices <- react$collaborators %>%
        ungroup() %>%
        distinct(id, .keep_all=T) %>%
        arrange(name)
      
      choices <- set_names(choices$id, choices$name)
      
      updateSelectInput(session, "artist_id", choices=choices)
    }
  })
  
  output$month <- renderggiraph({
    if (loading())
      return()
    
    line <- react$stat_long %>%
      filter(feature == input$feature & metric == input$metric)
    
    points <- line %>%
      arrange(desc(abs(change))) %>%
      slice(1:5) %>%
      bind_rows(slice(line, 1), slice(line, n())) %>%
      unique()
    
    gg <- ggplot()+
      geom_step(aes(added_month, value), line, color=color[[input$feature]], alpha=0.8)+
      geom_point_interactive(aes(added_month, value, tooltip=tooltip, data_id=added_month), points, color=color[[input$feature]], size=5, alpha=0.8)+
      scale_y_continuous(expand=expand_scale(0.2))+
      theme_spotify()+
      theme(plot.background=rect_black, panel.background=rect_black, axis.line=blank, axis.ticks=blank, axis.title=blank)
    
    girafe(ggobj=gg,
           height=150 / input$window$dpi) %>%
      girafe_options(opts_selection(css_selection, "single", selected=max(points$added_month)),
                     opts_tooltip(css_tooltip),
                     opts_hover(css_hover),
                     opts_toolbar(saveaspng=F),
                     opts_sizing(rescale=F))
  })
  
  output$summary <- renderUI({
    if (loading() || !input$feature %in% react$stat_all$feature)
      return()
    
    month_selected <- if (length(input$month_selected)) input$month_selected else max(react$stat_long$added_month)
    
    label <- list(spotify=glue("<span style='color:grey30'>Spotify's library</span>"),
                  your=glue("<span style='color:{color[[input$feature]]}'>your library</span>"))
    
    react$stat_long %>%
      filter(feature == input$feature & metric == "median" & added_month == month_selected) %>%
      mutate(feature=as.character(feature)) %>%
      inner_join(react$stat_all, "feature") %>%
      mutate(text=paste0("As of ", format(added_month, "%B %Y"), ", tracks in ",
                         ifelse(median > value, label$spotify, label$your),
                         " were on average <strong>",
                         percent(abs(median - value) / min(median, value), 2), " ", feature_labels[input$feature],
                         "</strong> than those in ",
                         ifelse(median > value, label$your, label$spotify),
                         ifelse(dip > 0.05, ", but yours are have more than a single mode", "."))) %>%
      pull(text) %>%
      HTML()
  })
  
  output$distribution <- renderggiraph({
    if (loading())
      return()
    
    bars_all <- react$bars_all %>%
      filter(feature == input$feature)
    
    tracks_filtered <- react$tracks_long %>%
      filter(feature == input$feature) %>%
      {if (length(input$month_selected)) filter(., added_month <= input$month_selected) else .}
    
    bars <- tracks_filtered %>%
      group_by(bin) %>%
      tally() %>%
      mutate(scaled=n / max(n))
    
    window_width <- input$window$width / input$window$dpi
    
    points <- tracks_filtered %>%
      mutate(top=top_short + top_medium + top_long > 0) %>%
      inner_join(bars, "bin", suffix=c("", "_bars")) %>%
      group_by(bin) %>%
      sample_n(n()) %>%
      arrange(desc(top), is.na(preview_url)) %>%
      mutate(index=seq(n())) %>%
      filter(index == 1 | index < 40 / 15 * window_width * scaled_bars) %>%
      arrange(added_date) %>%
      mutate(y_pos=seq(0.02, scaled_bars[1] - 0.02, length.out=n())) %>%
      mutate(y_pos=ifelse(index %% 2 == 0, lag(y_pos), y_pos)) %>%
      filter(!is.na(y_pos))
    
    your_tracks <- paste0("your tracks (", comma(sum(bars$n)), ")")
    
    gg <- ggplot()+
      geom_col(aes(bin, scaled, fill="Spotify library"), bars_all, color="grey30", width=1)+
      geom_col(aes(bin, scaled, fill=your_tracks), bars, alpha=0.5, width=0.85)+
      geom_text(aes(bin, scaled, label=comma(n)), bars, size=3, family=nova, color=color$grey, vjust=-0.2)+
      geom_jitter_interactive(aes(bin, y_pos, shape=top, tooltip=tooltip, data_id=id), points, size=5, alpha=0.3, color=color[[input$feature]], fill=color$slate, width=0.2, height=0)+
      scale_fill_manual(values=setNames(c("grey30", color[[input$feature]]), c("Spotify library", your_tracks)))+
      scale_shape_manual(values=c("TRUE"=21, "FALSE"=19), guide=F)+
      labs(x=feature_labels[input$feature], y="more tracks / time", fill="")+
      theme_spotify()
    
    print(input$window$dpi)
    
    girafe(ggobj=gg,
           width_svg=units_right / 12 * window_width,
           height_svg=530 / input$window$dpi) %>% # not sure why it doesn't convert to 500px
      girafe_options(opts_selection(css_selection, "single"),
                     opts_tooltip(css_tooltip),
                     opts_hover(css_hover),
                     opts_sizing(rescale=F))
  })
  
  output$player <- renderUI({
    if (is.null(input$distribution_selected))
      return(div(style="height:57px"))
    
    if (input$distribution_selected %in% react$edges_all$track_id)
      updateSelectInput(session, "track", selected=input$distribution_selected)
    
    play <- react$tracks %>%
      filter(id == input$distribution_selected)
    
    if (is.na(play$preview_url))
      return()
    
    glue("<audio controls autoplay src='{play$preview_url}' type='audio/mpeg' style=height:50px></audio>",
         "<div class='side'>{play$artist_names} - {play$name}</div>") %>%
      HTML()
  })
  
  output$artist <- renderggiraph({
    if (loading())
      return()
    
    gg <- react$collaborators %>%
      group_by(id, name) %>%
      tally() %>%
      ungroup() %>%
      arrange(desc(n)) %>%
      slice(1:10) %>%
      mutate(index=seq(n(), 1)) %>%
      ggplot()+
      geom_col_interactive(aes(index, n, tooltip=paste0(name, ": ", comma(n)), data_id=id), fill=color[[input$feature]], alpha=0.6)+
      coord_flip()+
      theme_spotify()+
      theme(plot.background=rect_black, panel.background=rect_black, axis.line=blank, axis.ticks=blank, axis.title=blank)
    
    girafe(ggobj=gg,
           height_svg=150 / input$window$dpi) %>%
      girafe_options(opts_selection(css_selection, "single"),
                     opts_tooltip(css_tooltip),
                     opts_hover(css_hover),
                     opts_toolbar(saveaspng=F),
                     opts_sizing(rescale=F))
  })
  
  observe({
    if (!loading() && length(input$artist_selected)) {
      updateSelectInput(session, "artist_id", selected=input$artist_selected)
    }
  })
  
  output$network <- renderVisNetwork({
    if (loading() || length(input$artist_id) == 0 || input$artist_id == "")
      return()
    
    collaborator_id <- react$collaborators %>%
      filter(id == input$artist_id) %>%
      pull(collaborator_id)
    
    edge_tracks <- react$track_artists %>%
      filter(artist_id %in% c(input$artist_id, collaborator_id)) %>%
      pull(track_id) %>%
      unique()
    
    tracks_feature <- react$tracks_long %>%
      filter(feature == input$feature) %>%
      transmute(track_id=id, value)
    
    digits <- if (diff(range(feature_bins[input$feature])) == 1) 2 else 0
    
    edges <- react$edges_all %>%
      filter(track_id %in% edge_tracks) %>%
      left_join(tracks_feature, "track_id") %>%
      mutate(title=paste0(track_name, "<br>", input$feature, ": ", round(value, digits)))
    
    nodes <- react$artists %>%
      filter(id %in% unique(c(edges$from, edges$to))) %>%
      transmute(id,
                label=name,
                title=map_chr(genres, function(x) if (length(x)) glue_collapse(x, "<br>") else ""),
                size=rescale(popularity, c(10, 30)))
    
    fire_edge <- "function(x) { Shiny.onInputChange('network_fired', x) ;}"
    
    visNetwork(nodes, edges, background=color$slate) %>%
      visNodes(color=list(background=rgba_string(color[[input$feature]]), border=rgba_string(color[[input$feature]]), highlight=rgba_string("white")),
               font=list(face=nova, color=color$grey)) %>%
      visEdges(scaling=list(min=1, max=7),
               color=list(color=color$grey, highlight=rgba_string("white", 0.5), opacity=0.2)) %>%
      visOptions(nodesIdSelection=list(enabled=T, selected=input$artist_id, style="width:0px; height:0px; border:none")) %>%
      visEvents(selectEdge=fire_edge, deselectEdge=fire_edge) %>%
      visInteraction(tooltipStyle=glue("position: fixed; visibility:hidden; {css_tooltip}"))
  })
  
  output$player2 <- renderUI({
    if (length(input$network_fired$edges) != 1 || length(input$network_fired$nodes) > 0)
      return(div(style="height:57px"))
    
    track_id <- react$edges_all %>%
      filter(id == input$network_fired$edges) %>%
      pull(track_id)

    play <- react$tracks %>%
      filter(id == track_id)
    
    if (is.na(play$preview_url))
      return()

    glue("<audio controls autoplay src='{play$preview_url}' type='audio/mpeg' style=height:50px></audio>",
         "<div class='side'>{play$artist_names} - {play$name}</div>") %>%
      HTML()
  })
  
  
})
