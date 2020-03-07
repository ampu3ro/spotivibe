
shinyServer(function(input, output, session) {
  
  react <- reactiveValues()
  
  observe({
    react$code <- parseQueryString(session$clientData$url_search)$code
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
                uiOutput("summary"),
                br(),
                fluidRow(
                  column(12,
                         div(class="inline",
                             "Start with a single feature like"),
                         div(class="inline",
                             selectInput("feature", NULL, feature_cols, "valence", width="140px")),
                         div(class="inline", 
                             "to understand how your top tracks from the last"),
                         div(class="inline",
                             selectInput("term", NULL, c("4 weeks"="top_short", "6 months"="top_medium", "several years"="top_long"), width="120px")),
                         div("stack up. Click on a point to display other tracks in your library with similar values. This should give you a feel for the quality of the feature."),
                         div(style="clear:both"),
                  )
                ),
                br(),
                fluidRow(
                  column(6, ggiraphOutput("points")),
                  column(6, DT::dataTableOutput("similar"), htmlOutput("player"))
                ),
                hr(),
                fluidRow(
                  column(12,
                         div("By counting occurences for ranges of values, you can see how all your saved tracks are distributed (green).",
                             "The gray histogram represents the distribution of all tracks on Spotify so gives a sense of how you compare on each feature.",
                             "The green line follows the median value over time, showing how your tastes have changed.",
                             "Click on a point (when you added more tracks than usual) to see the distribution at that time.")
                  )
                ),
                br(),
                fluidRow(
                  column(6, ggiraphOutput("bars")),
                  column(6, ggiraphOutput("ribbons"))
                ),
                hr(),
                fluidRow(
                  column(12,
                         div("Another way to cut the data is to break down the audio feature by prominent genres in your library, though it's important",
                             "to note that this is a rough approximation since Spotify only assigns those labels to artists, not tracks.",
                             "Click on a box to highlight that genre's share trend over time."),
                         br(),
                         htmlOutput("insight")
                  )
                ),
                br(),
                fluidRow(
                  column(6, ggiraphOutput("box")),
                  column(6, ggiraphOutput("lines"))
                ),
                br()
      )
    }
  })
  
  observeEvent(input$connect, {
    url <- oauth2.0_authorize_url(endpoint, app, scope=scope)
    session$sendCustomMessage("redirect", url)
  })
  
  observe({
    if (is.null(react$code) && input$dataset == "yours")
      return()
    
    if (input$dataset == "yours") {
      
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
          bind_rows(tracks_top) %>%
          transmute(id,
                    name,
                    added_at=as_datetime(added_at),
                    added_date=as_date(added_at),
                    added_month=floor_date(added_date, "month"),
                    artists,
                    artist_name1=map_chr(artists, function(x) x$name[1]),
                    artist_names=map_chr(artists, function(x) glue_collapse(x$name, ", ")),
                    album_name=album.name,
                    label=paste(artist_names, "-", name),
                    preview_url,
                    play=ifelse(is.na(preview_url), "", as.character(icon("play-circle")))) %>%
          arrange(added_date) %>%
          distinct(name, artist_name1, .keep_all=T) %>%
          inner_join(select(features, one_of("id", feature_cols)), "id") %>%
          left_join(tracks_top_wide, c("name", "artist_name1")) %>%
          replace_na(list(top_short=0, top_medium=0, top_long=0))
        
        tracks_long <- tracks %>%
          pivot_longer(one_of(feature_cols), names_to="feature") %>%
          mutate(feature=factor(feature, feature_cols),
                 tooltip=paste0(name,"<br>",feature," = ", round(value, 2), 
                                ifelse(feature == "loudness", "dB", ifelse(feature == "tempo", "BPM", "")))) %>%
          group_by(feature) %>%
          mutate(bin=cut(value, 20, F)) %>%
          ungroup()
        
        tracks_long <- tracks %>%
          mutate(loudness=rescale(loudness, c(0, 1), range(feature_bins$loudness)),
                 tempo=rescale(tempo, c(0, 1), range(feature_bins$tempo))) %>%
          pivot_longer(one_of(feature_cols), names_to="feature", values_to="scaled") %>%
          select(scaled) %>%
          bind_cols(tracks_long)
        
        stat <- tracks_long %>%
          group_by(feature) %>%
          summarize(median=median(value), dip=dip(value)) %>%
          mutate(feature=as.character(feature)) %>%
          inner_join(stat_all, "feature", suffix=c("_user", "_all")) %>%
          mutate(delta=median_user - median_all,
                 label=ifelse(feature == "valence",
                              ifelse(dip > 0.05 & abs(delta) < 0.1, "mixed mood",
                                     ifelse(delta > 0.2, "pretty happy",
                                            ifelse(delta > 0, "slightly happy",
                                                   ifelse(delta < 0.2, "pretty sad", "slightly sad")))),
                              ifelse(feature == "energy",
                                     ifelse(dip > 0.05 & abs(delta) < 0.1, "both higher and lower energy",
                                            ifelse(abs(delta) < 0.1, "comparable in energy",
                                                   ifelse(delta > 0.1, "higher energy", "lower energy"))),
                                     ifelse(feature == "danceability",
                                            ifelse(dip > 0.05 & abs(delta) < 0.1, "",
                                                   ifelse(delta > 0, " and more danceable", " and less danceable")),""))))
        
        setProgress(0.8, "creating new features")
        
        tracks_before <- tracks$added_date %>%
          unique() %>%
          na.omit() %>%
          map_dfr(function(date) {
            tracks_long %>%
              filter(added_date <= date) %>%
              group_by(feature) %>%
              summarize(value_lower=quantile(value, 0.25),
                        value_median=median(value),
                        value_mean=mean(value),
                        value_upper=quantile(value, 0.75)) %>%
              mutate(added_date=date)
          })
        
        tracks_before <- tracks %>%
          group_by(added_date) %>%
          tally() %>%
          inner_join(tracks_before, "added_date") %>%
          mutate(added_year=year(added_date)) %>%
          arrange(added_date)
        
        track_genres <- tracks %>%
          select(track_id=id, artists, added_month) %>%
          unnest(artists) %>%
          inner_join(select(artists, id, genres), "id") %>%
          select(id=track_id, added_month, genres) %>%
          unnest(genres) %>%
          rename(genre=genres)
        
        genres_top <- track_genres %>%
          group_by(genre) %>%
          tally() %>%
          arrange(desc(n)) %>%
          slice(1:10) %>%
          pull(genre)
        
        genres_long <-  track_genres %>%
          filter(genre %in% genres_top) %>%
          inner_join(features, "id") %>%
          select(one_of("genre", feature_cols)) %>%
          mutate(genre=factor(genre, genres_top)) %>%
          pivot_longer(one_of(feature_cols), names_to="feature")
        
        genres_before <- tracks$added_month %>%
          unique() %>%
          na.omit() %>%
          map_dfr(function(date) {
            track_genres %>%
              filter(added_month <= date) %>%
              group_by(genre) %>%
              tally() %>%
              mutate(share=n / sum(n), added_month=date)
          })
        
        genres_before <- genres_before %>%
          mutate(genre=factor(genre, genres_top), added_year=year(added_month)) %>%
          drop_na()
      })
      
      # save(tracks_saved, tracks_top, features, artists,
      #      bars_all, tracks, tracks_long, stat, tracks_before, genres_long, genres_before, file="spotify.RData")

    } else {
      load("spotify.RData")
    }
    
    react$bars_all <- bars_all
    react$tracks <- tracks
    react$tracks_long <- tracks_long
    react$stat <- stat
    react$tracks_before <- tracks_before
    react$genres_long <- genres_long
    react$genres_before <- genres_before
  })
  
  output$summary <- renderUI({
    if (is.null(react$stat))
      return()
    
    react$stat %>%
      select(feature, label) %>%
      pivot_wider(names_from="feature", values_from="label") %>%
      glue_data("Looks like your tracks are <span style=color:{color$green}>{valence}</span> relative to Spotify's full library.",
                " They're also <span style=color:{color$green}>{energy}{danceability}</span>. Explore why below!") %>%
      HTML()
  })
  
  observe({
    if (!is.null(react$tracks_long) && !is.null(input$feature)) {
      react$tracks_feature <- filter(react$tracks_long, feature == input$feature)
    }
  })
  
  output$points <- renderggiraph({
    if (is.null(react$tracks_feature))
      return()
    
    tracks_feat_top <- react$tracks_feature %>%
      filter(!!sym(input$term) > 0) %>%
      arrange(desc(scaled))
    
    gg <- ggplot(tracks_feat_top, aes(0, scaled))+
      geom_text_repel(aes(label=label), nudge_x=0.05, direction="y", hjust=0, size=5, segment.size=0.1, color="white", family=nova)+
      geom_point_interactive(aes(tooltip=tooltip, data_id=id), size=7, alpha=0.5, color=color$green)+
      xlim(0, 1)+
      ylim(0, 1)+
      labs(y=feature_labels[input$feature])+
      theme_spotify(axis.line.x=blank, axis.title.x=blank)
    
    girafe(ggobj=gg, width_svg=10, height_svg=7) %>%
      girafe_options(opts_selection(css_selection, "single"), opts_tooltip(css_tooltip), opts_hover(css_hover))
  })
  
  observe({
    if (is.null(react$tracks_feature))
      return()
    
    if (length(input$points_selected)) {
      feature_value <- react$tracks %>%
        filter(id == input$points_selected) %>%
        pull(input$feature)
      
      react$similar <- react$tracks_feature %>%
        filter(id != input$points_selected) %>%
        mutate(delta=abs(value - feature_value)) %>%
        arrange(delta) %>%
        slice(1:10)
    } else {
      react$similar <- NULL
    }
  })
  
  output$similar <- DT::renderDataTable({
    if (!is.null(react$similar)) {
      react$similar %>%
        select(play, artist_names, album_name, name) %>%
        datatable(colnames=c("", "Artists", "Album", "Track"), rownames=F, escape=F, selection="single",
                  options=list(searching=F, paging=F, info=F, ordering=F), class="compact")
    }
  })
  
  output$player <- renderUI({
    if (is.null(react$similar) || !length(input$similar_rows_selected))
      return()
    
    url <- react$similar %>%
      slice(input$similar_rows_selected) %>%
      pull("preview_url")
    
    if (is.na(url))
      return()
    
    HTML(paste0('<br><audio controls src="',url,'" type="audio/mpeg" autoplay></audio>'))
  })
  
  output$bars <- renderggiraph({
    if (is.null(react$tracks_feature))
      return()
    
    added_before <- if (length(input$ribbons_selected)) input$ribbons_selected else max(react$tracks$added_date, na.rm=T)
    bins <- feature_bins[[input$feature]]
    unit <- switch(input$feature, loudness=" dB", tempo=" BPM", "")
    
    bars <- react$tracks_feature %>%
      filter(added_date <= added_before) %>%
      group_by(bin) %>%
      tally() %>%
      mutate(scaled=n / max(n),
             tooltip=paste0(input$feature," = ", bins[bin], " - ", bins[bin] + diff(bins)[1], unit,"<br>",
                            "track count = ", comma(n)))
    
    gg <- ggplot()+
      geom_col(aes(bin, scaled, fill="Spotify library"), filter(react$bars_all, feature == input$feature), color="grey30", width=1)+
      geom_col_interactive(aes(bin, scaled, fill="your tracks", tooltip=tooltip, data_id=bin), bars, alpha=0.4, width=0.85)+
      scale_fill_manual(values=c("grey30", color$green))+
      scale_color_manual(values=color$palette, guide=F)+
      labs(x=feature_labels[input$feature], y="more tracks", fill="")+
      coord_flip()+
      theme_spotify()
    
    girafe(ggobj=gg, width_svg=10, height_svg=7) %>%
      girafe_options(opts_selection(css_selection, "none"), opts_tooltip(css_tooltip), opts_hover(css_hover))
  })
  
  output$ribbons <- renderggiraph({
    if (is.null(react$tracks_before))
      return()
    
    tracks_before <- react$tracks_before %>%
      filter(feature == input$feature)
    
    points <- tracks_before %>%
      group_by(added_year) %>%
      arrange(desc(n)) %>%
      slice(1:2) %>%
      mutate(tooltip=paste0(added_date, " (", comma(n), " tracks added)<br>",
                            "median ", feature, " = ", round(value_median, 2)))
    
    gg <- ggplot(tracks_before)+
      geom_ribbon(aes(added_date, ymin=value_lower, ymax=value_upper, fill="25th - 75th percentile"), alpha=0.2)+
      geom_step(aes(added_date, value_mean, linetype="mean"), color=color$green)+
      geom_step(aes(added_date, value_median, linetype="median"), size=1, color=color$green)+
      geom_point_interactive(aes(added_date, value_median, tooltip=tooltip, data_id=added_date), points, size=6, alpha=0.5, color=color$green)+
      scale_y_continuous(limits=range(feature_bins[input$feature]))+
      scale_fill_manual("", values=color$green)+
      scale_linetype_manual("", values=c(mean="dotted", median="solid"))+
      labs(x="time", y=feature_labels[input$feature])+
      theme_spotify(axis.title.x=blank, axis.text.x=element_text_nova())
    
    girafe(ggobj=gg, width_svg=10, height_svg=7) %>%
      girafe_options(opts_selection(css_selection, "single", selected=max(react$tracks$added_date, na.rm=T)),
                     opts_tooltip(css_tooltip),
                     opts_hover(css_hover))
  })
  
  output$insight <- renderUI({
    if (!is.null(react$genres_before)) {
      total_share <- react$genres_before %>%
        filter(added_month %in% range(added_month)) %>%
        group_by(added_month) %>%
        summarize(sum(share)) %>%
        pull(2)
      
      decreasing <- diff(total_share) < 0
      green <- glue("color:{color$green}")
      
      paste0("Because the total share of your top genres has ",
             span(style=green,
                  ifelse(decreasing, "decreased", "increased")),
             " your taste seems to be trending ",
             span(style=green,
                  ifelse(1-total_share[2]/total_share[1] < 0.2, "slightly ", ""),
                  ifelse(decreasing, "more", "less"), " eclectic")) %>%
        HTML()
    }
  })
  
  output$box <- renderggiraph({
    if (is.null(react$genres_long) || is.null(input$feature))
      return()
    
    genre_feature <- react$genres_long %>%
      filter(feature == input$feature) %>%
      group_by(genre) %>%
      mutate(n=n(), value_median=median(value)) %>%
      mutate(tooltip=paste0(genre, " (", comma(n), " tracks)<br> median ", input$feature, " = ", value_median))
    
    gg <- ggplot(genre_feature)+
      geom_boxplot_interactive(aes(genre, value, fill=genre, tooltip=tooltip, data_id=genre), outlier.color=NA, alpha=0.8)+
      scale_y_continuous(limits=range(feature_bins[input$feature]))+
      scale_fill_manual(values=rep(color$palette, 2), guide=F)+
      labs(y=feature_labels[input$feature])+
      theme_spotify(axis.title.x=blank)
    
    girafe(ggobj=gg, width_svg=10, height_svg=7) %>%
      girafe_options(opts_selection("fill:transparent; stroke:white", "single"), opts_tooltip(css_tooltip), opts_hover(css_hover))
  })
  
  output$lines <- renderggiraph({
    if (is.null(react$genres_before))
      return()
    
    genre_sel <- if (length(input$box_selected)) input$box_selected else unique(react$genres_before$genre)
    
    text <- react$genres_before %>%
      filter(added_month == min(added_month))
    
    gg <- ggplot(react$genres_before)+
      geom_line_interactive(aes(added_month, share, color=genre, alpha=genre %in% genre_sel, tooltip=genre), size=1.5)+
      geom_text_repel(aes(added_month, share, label=genre), text, size=4, hjust=1, nudge_x=0.05, direction="y",
                      segment.size=0.5, segment.alpha=0.5, color="white", family=nova)+
      scale_x_date(expand=expand_scale(c(0.2, 0.05)))+
      scale_y_continuous(labels=function(x) percent(x, 1))+
      scale_color_manual(values=rep(color$palette, 2), guide=F)+
      scale_alpha_manual(values=c("FALSE"=0.2, "TRUE"=0.8), guide=F)+
      labs(y="share of your tracks")+
      theme_spotify(axis.text.y=element_text_nova(), axis.title.x=blank, axis.text.x=element_text_nova())
    
    girafe(ggobj=gg, width_svg=10, height_svg=7) %>%
      girafe_options(opts_selection(type="none"), opts_tooltip(css_tooltip), opts_hover(css_hover))
  })
  
})
