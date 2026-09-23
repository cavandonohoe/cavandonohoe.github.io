# Saved Podcast Episodes — Shiny dashboard
# Data: static snapshot in data/saved_episodes.json, pulled from the Spotify
# Web API (current_user_saved_episodes). NOTE: that endpoint only serves the
# ~185 most recently saved episodes (capped at offset 200; the saved-status
# check is 403 for third-party apps), so this is a subset of the full
# "Your Episodes" list. Deployed to shinyapps.io.

library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)
library(scales)
library(stringr)
library(tibble)

raw <- jsonlite::fromJSON("data/saved_episodes.json", simplifyDataFrame = TRUE)
meta <- raw$meta
episodes <- raw$episodes |>
  dplyr::mutate(
    added_at = as.Date(added_at),
    added_month = floor_date(added_at, "month"),
    dur_min = as.numeric(dur_min),
    dur_hr = dur_min / 60
  )

show_levels <- episodes |>
  dplyr::count(show, sort = TRUE) |>
  dplyr::pull(show)

accent <- "#1DB954" # Spotify green, used sparingly

theme <- bslib::bs_theme(
  version = 5,
  bg = "#121212",
  fg = "#e8e8e8",
  primary = accent,
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter")
)

ui <- bslib::page_sidebar(
  title = "Saved Podcast Episodes",
  theme = theme,
  fillable = FALSE,
  sidebar = bslib::sidebar(
    width = 300,
    bslib::input_switch("only_top", "Group tail shows as \"Other\"", value = FALSE),
    selectInput(
      "shows", "Shows",
      choices = show_levels, selected = show_levels,
      multiple = TRUE
    ),
    sliderInput(
      "dur", "Episode length (min)",
      min = 0, max = ceiling(max(episodes$dur_min)),
      value = c(0, ceiling(max(episodes$dur_min))), step = 5
    ),
    dateRangeInput(
      "dates", "Date saved",
      start = min(episodes$added_at), end = max(episodes$added_at)
    ),
    textInput("q", "Search title / description", placeholder = "e.g. aliens"),
    hr(),
    tags$small(
      style = "color:#9a9a9a;",
      sprintf(
        "Snapshot: %s \u00b7 %s episodes (all the API returns)",
        meta$generated_at, meta$n_episodes
      )
    )
  ),
  bslib::layout_columns(
    fill = FALSE,
    bslib::value_box(
      "Episodes (API call limit)", textOutput("kpi_eps"),
      theme = "dark"
    ),
    bslib::value_box("Shows", textOutput("kpi_shows"), theme = "dark"),
    bslib::value_box("Total hours", textOutput("kpi_hours"), theme = "dark"),
    bslib::value_box("Avg length", textOutput("kpi_avg"), theme = "dark")
  ),
  tags$div(
    class = "alert alert-secondary",
    style = paste(
      "background:#1e1e1e; border:1px solid #2a2a2a; color:#c9c9c9;",
      "font-size:0.85rem;"
    ),
    tags$strong(sprintf("Showing %s episodes. ", meta$n_episodes)),
    "This is every episode Spotify's Web API will return: the ",
    tags$code("saved-episodes"), " endpoint is capped at offset 200 and the ",
    "saved-status check is blocked for third-party apps. Your Spotify app's ",
    "\"Your Episodes\" list is larger (it also counts downloads and ",
    "auto-added episodes), but the full number isn't available through the ",
    "public API, so everything below covers only these ",
    sprintf("%s.", meta$n_episodes)
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Episodes per show"),
      plotlyOutput("bar_count", height = 900)
    ),
    bslib::card(
      bslib::card_header("Hours per show"),
      plotlyOutput("bar_hours", height = 900)
    )
  ),
  bslib::card(
    bslib::card_header("Episodes saved over time, by show"),
    div(
      style = "overflow-x: auto; overflow-y: hidden; width: 100%;",
      plotlyOutput("line_time", height = 340, width = "100%")
    ),
    tags$small(
      style = "color:#9a9a9a;",
      "Each month's bar is split by show. Scroll horizontally to see every month."
    )
  ),
  bslib::card(
    bslib::card_header("Episodes"),
    DT::DTOutput("tbl")
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    df <- episodes |>
      dplyr::filter(
        show %in% input$shows,
        dur_min >= input$dur[1], dur_min <= input$dur[2],
        added_at >= input$dates[1], added_at <= input$dates[2]
      )
    if (nzchar(input$q)) {
      pat <- stringr::regex(input$q, ignore_case = TRUE)
      df <- df |>
        dplyr::filter(
          stringr::str_detect(name, pat) |
            stringr::str_detect(dplyr::coalesce(desc, ""), pat)
        )
    }
    df
  })

  per_show <- reactive({
    d <- filtered() |>
      dplyr::group_by(show) |>
      dplyr::summarise(
        n = dplyr::n(),
        hours = sum(dur_hr),
        avg_min = round(mean(dur_min)),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(n))
    if (isTRUE(input$only_top) && nrow(d) > 8) {
      top <- d |> dplyr::slice_head(n = 8)
      other <- d |>
        dplyr::slice(9:dplyr::n()) |>
        dplyr::summarise(
          show = "Other",
          n = sum(n), hours = sum(hours), avg_min = round(mean(avg_min))
        )
      d <- dplyr::bind_rows(top, other)
    }
    d
  })

  output$kpi_eps <- renderText(scales::comma(nrow(filtered())))
  output$kpi_shows <- renderText(dplyr::n_distinct(filtered()$show))
  output$kpi_hours <- renderText(sprintf("%.1f", sum(filtered()$dur_hr)))
  output$kpi_avg <- renderText({
    v <- filtered()$dur_min
    if (length(v) == 0) "\u2014" else sprintf("%d min", round(mean(v)))
  })

  hbar <- function(d, x, xlab) {
    d <- d |> dplyr::arrange(.data[[x]])
    # ~55px of vertical room per show so every podcaster label is legible,
    # with a tall floor; the card grows to fit.
    plot_h <- max(720, nrow(d) * 55)
    plot_ly(
      d,
      x = ~ get(x), y = ~ factor(show, levels = show),
      type = "bar", orientation = "h",
      height = plot_h,
      marker = list(color = accent),
      hovertemplate = paste0("%{y}<br>", xlab, ": %{x}<extra></extra>")
    ) |>
      layout(
        xaxis = list(title = xlab, gridcolor = "#2a2a2a"),
        yaxis = list(
          title = "", automargin = TRUE, tickfont = list(size = 13)
        ),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#e8e8e8"), margin = list(l = 10)
      ) |>
      config(displayModeBar = FALSE)
  }

  output$bar_count <- renderPlotly(hbar(per_show(), "n", "Episodes"))
  output$bar_hours <- renderPlotly({
    d <- per_show() |> dplyr::mutate(hours = round(hours, 1))
    hbar(d, "hours", "Hours")
  })

  output$line_time <- renderPlotly({
    df <- filtered()
    # Continuous monthly sequence (fill gaps) so the scrollable trend reads
    # month-by-month with no missing columns.
    if (nrow(df) == 0) {
      months <- as.Date(character())
    } else {
      months <- seq(min(df$added_month), max(df$added_month), by = "month")
    }
    # Collapse the show dimension the same way the per-show charts do: when the
    # "Other" toggle is on, keep the top shows and bucket the rest so the
    # stacked legend stays readable.
    show_order <- df |>
      dplyr::count(show, sort = TRUE) |>
      dplyr::pull(show)
    if (isTRUE(input$only_top) && length(show_order) > 8) {
      keep <- show_order[1:8]
      df <- df |>
        dplyr::mutate(show_grp = ifelse(show %in% keep, show, "Other"))
      grp_levels <- c(keep, "Other")
    } else {
      df <- df |> dplyr::mutate(show_grp = show)
      grp_levels <- show_order
    }
    counts <- df |>
      dplyr::count(added_month, show_grp, name = "n")
    plot_width <- max(720, length(months) * 90)
    p <- plot_ly(width = plot_width, height = 320)
    for (g in grp_levels) {
      gd <- counts |> dplyr::filter(show_grp == g)
      yvals <- tibble::tibble(added_month = months) |>
        dplyr::left_join(gd, by = "added_month") |>
        dplyr::mutate(n = dplyr::coalesce(n, 0L)) |>
        dplyr::arrange(added_month) |>
        dplyr::pull(n)
      p <- p |>
        add_bars(
          x = months, y = yvals, name = g,
          hovertemplate = paste0(g, "<br>%{x|%b %Y}: %{y}<extra></extra>")
        )
    }
    p |>
      layout(
        barmode = "stack",
        legend = list(font = list(size = 11)),
        xaxis = list(
          title = "Month saved", gridcolor = "#2a2a2a",
          dtick = "M1", tickformat = "%b %Y", tickangle = -45
        ),
        yaxis = list(title = "Episodes saved", gridcolor = "#2a2a2a"),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#e8e8e8"),
        margin = list(b = 70)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$tbl <- DT::renderDT({
    d <- filtered() |>
      dplyr::transmute(
        Added = added_at, Show = show, Episode = name,
        Min = dur_min, Link = url
      ) |>
      dplyr::arrange(dplyr::desc(Added))
    d$Link <- ifelse(
      is.na(d$Link), "",
      sprintf('<a href="%s" target="_blank">open</a>', d$Link)
    )
    DT::datatable(
      d,
      escape = FALSE, rownames = FALSE,
      options = list(pageLength = 15, order = list(list(0, "desc"))),
      class = "compact stripe hover"
    )
  })
}

shinyApp(ui, server)
