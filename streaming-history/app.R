# Spotify Streaming History — Shiny dashboard
# Data: tidy CSVs produced by scripts/ingest_streaming_history.R from a
# Spotify "Extended Streaming History" GDPR export (the full listening log,
# which is NOT available through the Web API). See that script's header for
# how to request and ingest the export. Deployed to shinyapps.io.

library(shiny)
library(bslib)
library(jsonlite)
library(readr)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)
library(scales)

data_dir <- "data"

meta <- jsonlite::fromJSON(file.path(data_dir, "meta.json"))
plays <- readr::read_csv(
  file.path(data_dir, "plays.csv"),
  show_col_types = FALSE
)
monthly <- readr::read_csv(
  file.path(data_dir, "monthly.csv"),
  show_col_types = FALSE
)
top_artists <- readr::read_csv(
  file.path(data_dir, "top_artists.csv"),
  show_col_types = FALSE
)
top_shows <- readr::read_csv(
  file.path(data_dir, "top_shows.csv"),
  show_col_types = FALSE
)

accent <- "#1DB954" # Spotify green

fmt_int <- function(x) format(round(x), big.mark = ",", trim = TRUE)

ui <- page_sidebar(
  title = "Spotify Streaming History",
  theme = bslib::bs_theme(
    version = 5, preset = "darkly", primary = accent
  ),
  sidebar = sidebar(
    title = "Filters",
    dateRangeInput(
      "dates", "Played between",
      start = meta$first_stream, end = meta$last_stream
    ),
    checkboxGroupInput(
      "kinds", "Kind",
      choices = c("music", "podcast", "other"),
      selected = c("music", "podcast")
    ),
    hr(),
    tags$small(
      style = "color:#9a9a9a;",
      sprintf(
        "Export generated %s \u00b7 %s streams \u00b7 %s to %s",
        meta$generated_at, fmt_int(meta$n_streams),
        meta$first_stream, meta$last_stream
      )
    )
  ),
  bslib::layout_columns(
    fill = FALSE,
    bslib::value_box("Streams", textOutput("kpi_streams"), theme = "dark"),
    bslib::value_box("Total hours", textOutput("kpi_hours"), theme = "dark"),
    bslib::value_box("Music hours", textOutput("kpi_music"), theme = "dark"),
    bslib::value_box("Podcast hours", textOutput("kpi_pod"), theme = "dark")
  ),
  bslib::layout_columns(
    col_widths = c(12),
    bslib::card(
      bslib::card_header("Listening minutes per month (music vs podcast)"),
      plotlyOutput("area_monthly", height = "320px")
    )
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Top artists by minutes"),
      plotlyOutput("bar_artists", height = "420px")
    ),
    bslib::card(
      bslib::card_header("Top podcast shows by minutes"),
      plotlyOutput("bar_shows", height = "420px")
    )
  ),
  bslib::card(
    bslib::card_header("Streams"),
    DT::dataTableOutput("tbl")
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    plays |>
      dplyr::filter(
        kind %in% input$kinds,
        date >= input$dates[1], date <= input$dates[2]
      )
  })

  output$kpi_streams <- renderText(fmt_int(nrow(filtered())))
  output$kpi_hours <- renderText(
    fmt_int(sum(filtered()$min_played, na.rm = TRUE) / 60)
  )
  output$kpi_music <- renderText({
    m <- filtered() |> dplyr::filter(kind == "music")
    fmt_int(sum(m$min_played, na.rm = TRUE) / 60)
  })
  output$kpi_pod <- renderText({
    p <- filtered() |> dplyr::filter(kind == "podcast")
    fmt_int(sum(p$min_played, na.rm = TRUE) / 60)
  })

  output$area_monthly <- renderPlotly({
    d <- filtered() |>
      dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
      dplyr::group_by(month, kind) |>
      dplyr::summarise(minutes = sum(min_played, na.rm = TRUE), .groups = "drop")
    if (!nrow(d)) return(plotly_empty())
    plot_ly(
      d, x = ~month, y = ~minutes, color = ~kind, type = "bar"
    ) |>
      layout(
        barmode = "stack", xaxis = list(title = ""),
        yaxis = list(title = "Minutes"), legend = list(orientation = "h")
      )
  })

  output$bar_artists <- renderPlotly({
    d <- filtered() |>
      dplyr::filter(kind == "music", !is.na(artist)) |>
      dplyr::group_by(artist) |>
      dplyr::summarise(minutes = sum(min_played, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(minutes)) |>
      head(20)
    if (!nrow(d)) return(plotly_empty())
    d$artist <- factor(d$artist, levels = rev(d$artist))
    plot_ly(
      d, x = ~minutes, y = ~artist, type = "bar",
      orientation = "h", marker = list(color = accent)
    ) |>
      layout(xaxis = list(title = "Minutes"), yaxis = list(title = ""))
  })

  output$bar_shows <- renderPlotly({
    d <- filtered() |>
      dplyr::filter(kind == "podcast", !is.na(show)) |>
      dplyr::group_by(show) |>
      dplyr::summarise(minutes = sum(min_played, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(minutes)) |>
      head(20)
    if (!nrow(d)) return(plotly_empty())
    d$show <- factor(d$show, levels = rev(d$show))
    plot_ly(
      d, x = ~minutes, y = ~show, type = "bar",
      orientation = "h", marker = list(color = "#4a90d9")
    ) |>
      layout(xaxis = list(title = "Minutes"), yaxis = list(title = ""))
  })

  output$tbl <- DT::renderDataTable({
    d <- filtered() |>
      dplyr::transmute(
        played_at, kind, title, source_name,
        min_played = round(min_played, 1)
      ) |>
      dplyr::arrange(dplyr::desc(played_at))
    DT::datatable(
      d, rownames = FALSE,
      options = list(pageLength = 15, order = list(list(0, "desc"))),
      class = "compact stripe hover"
    )
  })
}

shinyApp(ui, server)
