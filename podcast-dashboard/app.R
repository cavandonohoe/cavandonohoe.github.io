# Saved Podcast Episodes — Shiny dashboard
# Data: static snapshot in data/saved_episodes.json, pulled from the Spotify
# Web API (current_user_saved_episodes). NOTE: that endpoint serves only
# explicitly-saved (hearted) episodes and is capped at offset 200. Auto-added
# and followed-show episodes shown under "Your Episodes" in the app are not
# exposed by the public API. The refresh script writes a meta$missingness
# block that bounds this gap (offset-cap headroom, zero-save month gaps, and
# saved-share of episodes across followed shows); the banner renders it.
# Deployed to shinyapps.io.

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

# Build the "what's missing" banner from meta$missingness when the refresh
# script has populated it; otherwise fall back to a static caveat. The
# saved-episodes endpoint only returns hearted episodes (capped at 200), so
# "Your Episodes" in the app is larger; these figures bound that gap.
missingness_note <- function(meta) {
  m <- meta$missingness
  if (is.null(m)) {
    return(tagList(
      tags$strong(sprintf("Showing %s episodes. ", meta$n_episodes)),
      "This is every episode the Spotify Web API's ", tags$code("saved-episodes"),
      " endpoint returns (explicitly-saved episodes only, capped at 200). Your ",
      "app's \"Your Episodes\" list is larger because it also counts ",
      "auto-added and followed-show episodes the public API does not expose."
    ))
  }

  fs <- m$followed_shows
  oc <- m$offset_cap
  zsm <- m$zero_save_months

  cap_msg <- if (isTRUE(oc$truncation_active)) {
    paste0(
      "Saved-episode paging hit the ", oc$cap, "-episode API cap, so the ",
      "oldest saves are truncated."
    )
  } else if (isTRUE(oc$truncation_possible)) {
    paste0(
      "Approaching the ", oc$cap, "-episode API cap (", oc$n_returned,
      " saved); older saves will start truncating soon."
    )
  } else {
    paste0(
      "Well under the ", oc$cap, "-episode API cap (", oc$n_returned,
      " saved), so no saves are being truncated."
    )
  }

  share_msg <- if (!is.null(fs) && !is.null(fs$n_followed) && fs$n_followed > 0 &&
    !is.null(fs$saved_share_of_available) &&
    !is.na(fs$saved_share_of_available)) {
    paste0(
      "Across your ", fs$n_followed, " followed shows there are about ",
      fs$total_episodes_available, " episodes available; your saved set covers ",
      sprintf("%.1f%%", 100 * fs$saved_share_of_available), " of them."
    )
  } else {
    NULL
  }

  gap_msg <- if (!is.null(zsm) && !is.null(zsm$count) && zsm$count > 0) {
    paste0(
      zsm$count, " month(s) between your first and last save have zero saved ",
      "episodes, likely listened via followed-show auto-adds rather than ",
      "hearted, so they are absent here."
    )
  } else {
    NULL
  }

  tagList(
    tags$strong(sprintf("Showing %s saved episodes. ", meta$n_episodes)),
    "Only explicitly-saved episodes are exposed by the public API. ",
    cap_msg,
    if (!is.null(share_msg)) tagList(" ", share_msg),
    if (!is.null(gap_msg)) tagList(" ", gap_msg)
  )
}

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
    missingness_note(meta)
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Episodes per show"),
      div(class = "plot-vscroll", plotlyOutput("bar_count", height = "auto"))
    ),
    bslib::card(
      bslib::card_header("Hours per show"),
      div(class = "plot-vscroll", plotlyOutput("bar_hours", height = "auto"))
    )
  ),
  bslib::card(
    bslib::card_header("Episodes saved over time, by show"),
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
        margin = list(b = 70),
        dragmode = FALSE
      ) |>
      config(displayModeBar = FALSE, scrollZoom = FALSE)
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
