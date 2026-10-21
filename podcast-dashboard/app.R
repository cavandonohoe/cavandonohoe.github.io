# Saved Podcast Episodes: library export plus daily API additions.

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
# Membership has two sources: the last Account Data export (YourLibrary.json)
# and episodes the daily Web API refresh appended afterwards. Older files
# without export_n_episodes treat every episode as export membership.
n_total <- nrow(raw$episodes)
n_export <- min(n_total, if (is.null(meta$export_n_episodes)) n_total else meta$export_n_episodes)
n_api_added <- n_total - n_export
export_date <- meta$generated_at
api_refresh_date <- if (is.null(meta$api_refreshed_at)) "never" else meta$api_refreshed_at
episodes <- raw$episodes |>
  dplyr::mutate(
    added_at = as.Date(added_at),
    added_month = floor_date(added_at, "month"),
    dur_min = as.numeric(dur_min),
    dur_hr = dur_min / 60
  )

# Episodes Spotify reports as fully played (resume_point.fully_played, stored by
# the daily refresh) are dropped from the dashboard. Rows stay in the JSON; an
# unknown played state (null, e.g. export-only saves) is kept.
if (!"fully_played" %in% names(episodes)) episodes$fully_played <- NA
n_finished <- sum(episodes$fully_played %in% TRUE)
episodes <- episodes |>
  dplyr::filter(!(fully_played %in% TRUE))
n_shown <- nrow(episodes)

history <- jsonlite::fromJSON("data/listening_history.json")
history_meta <- history$meta
# `events` is numeric (double) here to match how jsonlite deserializes the
# populated history file, so the empty-history placeholder and the real data
# agree on column types before the left_join below.
history_rows <- tibble::tibble(
  id = character(), events = double(), listened_min = double(),
  first_played = character(), last_played = character(),
  start_reasons = character(), end_reasons = character()
)
if (length(history$episodes) > 0) {
  history_rows <- history$episodes
}
episodes <- episodes |>
  dplyr::left_join(history_rows, by = "id") |>
  dplyr::mutate(
    history_status = ifelse(is.na(events), "No matching history", "Recorded playback")
  )

known_dates <- episodes$added_at[!is.na(episodes$added_at)]
length_max <- max(c(5, episodes$dur_min), na.rm = TRUE)
date_min <- if (length(known_dates)) min(known_dates) else Sys.Date()
date_max <- if (length(known_dates)) max(known_dates) else Sys.Date()

show_levels <- episodes |>
  dplyr::count(show, sort = TRUE) |>
  dplyr::pull(show)

accent <- "#1DB954" # Spotify green, used sparingly

# Insert <br> line breaks so long podcast names wrap across multiple lines on
# the bar-chart axis instead of being truncated. Greedily packs whole words up
# to `width` characters per line.
wrap_label <- function(x, width = 24) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    words <- strsplit(s, " ", fixed = TRUE)[[1]]
    lines <- character(0)
    cur <- ""
    for (w in words) {
      cand <- if (nzchar(cur)) paste(cur, w) else w
      if (nchar(cand) > width && nzchar(cur)) {
        lines <- c(lines, cur)
        cur <- w
      } else {
        cur <- cand
      }
    }
    if (nzchar(cur)) lines <- c(lines, cur)
    paste(lines, collapse = "<br>")
  }, character(1), USE.NAMES = FALSE)
}

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
  tags$head(tags$style(HTML(paste(
    # On touch devices plotly grabs the drag gesture, so a finger swipe that
    # lands on a chart pans the plot instead of scrolling the page. Handing
    # the browser the relevant axis via touch-action keeps the page scrollable.
    ".plot-vscroll .plotly, .plot-vscroll .js-plotly-plot {",
    "  touch-action: pan-y !important;",
    "}",
    ".plot-hscroll .plotly, .plot-hscroll .js-plotly-plot {",
    "  touch-action: pan-x !important;",
    "}",
    sep = "\n"
  )))),
  sidebar = bslib::sidebar(
    width = 300,
    bslib::input_switch("only_top", "Group tail shows as \"Other\"", value = TRUE),
    selectInput(
      "shows", "Shows",
      choices = show_levels, selected = show_levels,
      multiple = TRUE
    ),
    sliderInput(
      "dur", "Episode length (min)",
      min = 0, max = ceiling(length_max),
      value = c(0, ceiling(length_max)), step = 5
    ),
    dateRangeInput(
      "dates", "Date saved",
      start = date_min, end = date_max
    ),
    checkboxInput("include_unknown", "Include unknown lengths / saved dates", TRUE),
    textInput("q", "Search title / description", placeholder = "e.g. aliens"),
    selectInput(
      "history_status", "Listening history",
      choices = c("All", "Recorded playback", "No matching history")
    ),
    hr(),
    tags$small(
      style = "color:#9a9a9a;",
      sprintf(
        paste(
          "Library export imported: %s \u00b7 Latest API refresh with new data: %s",
          "\u00b7 %s episodes (%s from export, %s added by API)",
          "\u00b7 %s finished hidden"
        ),
        export_date, api_refresh_date, scales::comma(n_total),
        scales::comma(n_export), scales::comma(n_api_added), scales::comma(n_finished)
      )
    )
  ),
  bslib::layout_columns(
    fill = FALSE,
    bslib::value_box(
      "Saved episodes", textOutput("kpi_eps"),
      theme = "dark"
    ),
    bslib::value_box("Shows", textOutput("kpi_shows"), theme = "dark"),
    bslib::value_box("Known duration (hours)", textOutput("kpi_hours"), theme = "dark"),
    bslib::value_box("Avg known length", textOutput("kpi_avg"), theme = "dark")
  ),
  tags$p(
    style = "color:#9a9a9a;",
    sprintf("Listening export: %s through %s (UTC dates). ",
            history_meta$coverage_start, history_meta$coverage_end),
    "History is matched to saved episodes by Spotify ID and may predate saving. ",
    "No matching history does not mean unplayed. Events can include partial or ",
    "repeat plays; listening time does not establish completion."
  ),
  tags$div(
    class = "alert alert-secondary",
    style = paste(
      "background:#1e1e1e; border:1px solid #2a2a2a; color:#c9c9c9;",
      "font-size:0.85rem;"
    ),
    tags$strong(sprintf("Showing %s episodes. ", scales::comma(n_shown))),
    if (n_finished > 0) {
      sprintf(
        "%s finished episodes (fully played per Spotify) are hidden. ",
        scales::comma(n_finished)
      )
    },
    sprintf(
      paste0(
        "The library has %s: %s come from the Account Data export ",
        "(YourLibrary.json, imported %s); ",
        "%s were added later by the daily Spotify API refresh (latest change %s). "
      ),
      scales::comma(n_total), scales::comma(n_export), export_date,
      scales::comma(n_api_added), api_refresh_date
    ),
    "The API refresh only adds saves, so an episode removed from the library stays ",
    "listed until the next export is imported. Titles, shows and links cover every episode. ",
    "Lengths and saved dates come from the Spotify API where available. ",
    sprintf("%s episodes have unknown lengths; %s have unknown saved dates. ",
            sum(is.na(episodes$dur_min)), sum(is.na(episodes$added_at))),
    "Duration statistics cover known values only; the saved-date chart shows ",
    "undated episodes in an \"Unknown\" column. ",
    "Episodes absent from this export are excluded from this library view; ",
    "their absence does not establish when or why they left the library."
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Episodes per show"),
      div(class = "plot-vscroll", plotlyOutput("bar_count", height = "auto"))
    ),
    bslib::card(
      bslib::card_header("Known episode duration per show"),
      div(class = "plot-vscroll", plotlyOutput("bar_hours", height = "auto"))
    )
  ),
  bslib::card(
    bslib::card_header("Episodes by month saved, by show"),
    div(
      class = "plot-hscroll",
      style = paste(
        "overflow-x: auto; overflow-y: hidden; width: 100%;",
        "touch-action: pan-x;"
      ),
      plotlyOutput("line_time", height = 340, width = "100%")
    ),
    tags$small(
      style = "color:#9a9a9a;",
      paste(
        "Older saves imported from the Account Data export have no saved date",
        "(Spotify does not provide one), so they appear in the \"Unknown\" column",
        "when \"Include unknown\" is on. Each month is split by show; scroll to see",
        "every month."
      )
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
        (isTRUE(input$include_unknown) & is.na(dur_min)) |
          (!is.na(dur_min) & dur_min >= input$dur[1] & dur_min <= input$dur[2]),
        (isTRUE(input$include_unknown) & is.na(added_at)) |
          (!is.na(added_at) & added_at >= input$dates[1] & added_at <= input$dates[2])
      )
    if (!is.null(input$history_status) && input$history_status != "All") {
      df <- df |> dplyr::filter(history_status == input$history_status)
    }
    if (nzchar(input$q)) {
      # Literal, not regex: a stray "(" or "*" typed in the search box throws a
      # pattern error that takes out every output depending on filtered().
      pat <- stringr::fixed(input$q, ignore_case = TRUE)
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
        hours = if (all(is.na(dur_hr))) NA_real_ else sum(dur_hr, na.rm = TRUE),
        avg_min = round(mean(dur_min, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(n))
    if (isTRUE(input$only_top) && nrow(d) > 8) {
      top <- d |> dplyr::slice_head(n = 8)
      other <- d |>
        dplyr::slice(9:dplyr::n()) |>
        dplyr::summarise(
          show = "Other",
          n = sum(n),
          hours = if (all(is.na(hours))) NA_real_ else sum(hours, na.rm = TRUE),
          avg_min = round(mean(avg_min, na.rm = TRUE))
        )
      d <- dplyr::bind_rows(top, other)
    }
    d
  })

  output$kpi_eps <- renderText(scales::comma(nrow(filtered())))
  output$kpi_shows <- renderText(dplyr::n_distinct(filtered()$show))
  output$kpi_hours <- renderText({
    v <- filtered()$dur_hr
    if (all(is.na(v))) return("\u2014")
    sprintf("%.1f", sum(v, na.rm = TRUE))
  })
  output$kpi_avg <- renderText({
    v <- na.omit(filtered()$dur_min)
    if (length(v) == 0) "\u2014" else sprintf("%d min", round(mean(v)))
  })

  hbar <- function(d, x, xlab) {
    d <- d |> dplyr::arrange(.data[[x]])
    # Wrap long podcast names onto multiple lines so they aren't truncated;
    # keep the raw name for the hover tooltip.
    d <- d |>
      dplyr::mutate(show_wrapped = wrap_label(show))
    wrap_levels <- d$show_wrapped
    n_lines <- vapply(
      wrap_levels, function(s) lengths(gregexpr("<br>", s, fixed = TRUE)) + 1L,
      integer(1)
    )
    # ~30px per text line per show (with a per-show floor) so wrapped, multi-line
    # podcaster labels are fully legible; the card grows to fit.
    plot_h <- max(720, sum(pmax(2L, n_lines)) * 30)
    plot_ly(
      d,
      x = ~ get(x), y = ~ factor(show_wrapped, levels = wrap_levels),
      type = "bar", orientation = "h",
      height = plot_h,
      marker = list(color = accent),
      customdata = ~show,
      hovertemplate = paste0("%{customdata}<br>", xlab, ": %{x}<extra></extra>")
    ) |>
      layout(
        xaxis = list(title = xlab, gridcolor = "#2a2a2a"),
        yaxis = list(
          title = "", automargin = TRUE, tickfont = list(size = 13)
        ),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#e8e8e8"), margin = list(l = 10),
        dragmode = FALSE
      ) |>
      config(displayModeBar = FALSE, scrollZoom = FALSE)
  }

  output$bar_count <- renderPlotly(hbar(per_show(), "n", "Episodes"))
  output$bar_hours <- renderPlotly({
    d <- per_show() |> dplyr::filter(!is.na(hours)) |> dplyr::mutate(hours = round(hours, 1))
    hbar(d, "hours", "Hours")
  })

  output$line_time <- renderPlotly({
    all_df <- filtered()
    df <- all_df |> dplyr::filter(!is.na(added_month))
    # Episodes from the Account Data export that the API never dated. Spotify
    # does not expose their save date anywhere, so they get their own
    # "Unknown" column instead of being silently dropped or guessed.
    n_unknown <- sum(is.na(all_df$added_month))
    unknown_label <- "Unknown"
    # Continuous monthly sequence (fill gaps) so the scrollable trend reads
    # month-by-month with no missing columns.
    if (nrow(df) == 0) {
      months <- as.Date(character())
    } else {
      months <- seq(min(df$added_month), max(df$added_month), by = "month")
    }
    month_labels <- format(months, "%b %Y")
    x_levels <- c(month_labels, if (n_unknown > 0) unknown_label)
    # Collapse the show dimension the same way the per-show charts do: when the
    # "Other" toggle is on, keep the top shows and bucket the rest so the
    # stacked legend stays readable.
    show_order <- all_df |>
      dplyr::count(show, sort = TRUE) |>
      dplyr::pull(show)
    if (isTRUE(input$only_top) && length(show_order) > 8) {
      keep <- show_order[1:8]
      all_df <- all_df |>
        dplyr::mutate(show_grp = ifelse(show %in% keep, show, "Other"))
      grp_levels <- c(keep, "Other")
    } else {
      all_df <- all_df |> dplyr::mutate(show_grp = show)
      grp_levels <- show_order
    }
    counts <- all_df |>
      dplyr::mutate(
        x = ifelse(is.na(added_month), unknown_label, format(added_month, "%b %Y"))
      ) |>
      dplyr::group_by(x, show_grp) |>
      dplyr::summarise(
        n = dplyr::n(),
        saved_dates = if (all(is.na(added_at))) {
          "Not provided by Spotify"
        } else {
          d <- sort(unique(added_at[!is.na(added_at)]))
          paste(sprintf("%d/%d", month(d), day(d)), collapse = ", ")
        },
        .groups = "drop"
      )
    plot_width <- max(720, length(x_levels) * 90)
    p <- plot_ly(width = plot_width, height = 320)
    for (g in grp_levels) {
      gd <- counts |> dplyr::filter(show_grp == g)
      month_data <- tibble::tibble(x = x_levels) |>
        dplyr::left_join(gd, by = "x") |>
        dplyr::mutate(
          n = dplyr::coalesce(n, 0L),
          saved_dates = dplyr::coalesce(saved_dates, "None")
        )
      p <- p |>
        add_bars(
          x = month_data$x, y = month_data$n, name = g,
          customdata = month_data$saved_dates,
          hovertemplate = paste0(
            g, "<br>%{x}: %{y}",
            "<br>Saved dates: %{customdata}<extra></extra>"
          )
        )
    }
    p |>
      layout(
        barmode = "stack",
        legend = list(font = list(size = 11)),
        xaxis = list(
          title = "Month saved", gridcolor = "#2a2a2a",
          type = "category", categoryorder = "array", categoryarray = x_levels,
          tickangle = -45
        ),
        yaxis = list(title = "Episodes saved", gridcolor = "#2a2a2a"),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#e8e8e8"),
        margin = list(b = 70),
        dragmode = FALSE
      ) |>
      config(displayModeBar = FALSE, scrollZoom = FALSE)
  })

  output$tbl <- DT::renderDT({
    d <- filtered() |>
      dplyr::transmute(
        Added = added_at, Show = show, Episode = name,
        `Length (min)` = dur_min, History = history_status,
        `Listened (min)` = round(listened_min, 1), Events = events,
        `First played (UTC)` = first_played, `Last played (UTC)` = last_played,
        `Start reasons (counts)` = start_reasons,
        `End reasons (counts)` = end_reasons, Link = url
      ) |>
      dplyr::arrange(dplyr::desc(Added))
    d$Link <- ifelse(
      is.na(d$Link), "",
      sprintf('<a href="%s" target="_blank">open</a>', d$Link)
    )
    DT::datatable(
      d,
      escape = which(names(d) != "Link"), rownames = FALSE,
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        scrollY = "65vh",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        scroller = TRUE,
        pageLength = 50,
        dom = "frti",
        order = list(list(0, "desc"))
      ),
      class = "compact stripe hover"
    )
  })
}

shinyApp(ui, server)
