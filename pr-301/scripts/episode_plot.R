`%>%` <- magrittr::`%>%`

# Shared IMDb season x episode heatmap. Sourced by imdb_rating_plot.Rmd (the
# site page) and scripts/quick_rating_plot.R (one-off local preview) so a
# quick preview looks exactly like what the site will publish.

# Build the ggplot heatmap for one show. episode_data needs season, episode,
# title, rating (votes optional). When export_png is TRUE the PNG is written to
# png_path, which defaults to imdb_rating_plot_files/<title slug>.png.
create_episode_plot <- function(episode_data, title, export_png = TRUE,
                                png_width = 8, png_height = 6,
                                png_path = episode_png_path(title)) {
  # make sure we have imdb_rating + hover text
  episode_data <- episode_data %>%
    dplyr::mutate(
      imdb_rating = rating,
      hover_text = paste0(
        "S", stringr::str_pad(season, 2, pad = "0"),
        "E", stringr::str_pad(episode, 2, pad = "0"),
        " — ", title,
        "\nRating: ", sprintf("%.1f", imdb_rating),
        if (!"votes" %in% names(episode_data)) "" else
          paste0("\nVotes: ", scales::comma(votes))
      )
    )

  episode_plot <- episode_data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = season,
        y = episode,
        fill = imdb_rating,
        text = hover_text
      )
    ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = round(imdb_rating, 1)),
      color = "black"
    ) +
    ggplot2::scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(9, "RdYlGn"),
      values = scales::rescale(c(0, 4, 6, 8, 10)),
      limits = c(0, 10),
      name = "IMDb Rating"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(episode_data$season), max(episode_data$season), 1),
      position = "top"
    ) +
    ggplot2::scale_y_reverse(
      breaks = seq(min(episode_data$episode), max(episode_data$episode), 1)
    ) +
    ggplot2::labs(
      title = "IMDb Ratings by Season and Episode",
      x = "Season",
      y = "Episode"
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_minimal()

  if (export_png) {
    dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(
      filename = png_path,
      plot = episode_plot,
      width = png_width,
      height = png_height,
      dpi = 300
    )
  }

  episode_plot
}

# PNG path the site publishes for a show title (also the raw-pngs link target).
episode_png_path <- function(title) {
  file_name <- title %>%
    tolower() %>%
    stringr::str_remove_all(":") %>%
    stringr::str_replace_all(" ", "_") %>%
    paste0(".png")
  here::here("imdb_rating_plot_files", file_name)
}

interactive_plotly <- function(gg_object, fig_width = NA, fig_height = NA) {
  px_per_in <- 96
  plot_width <- if (is.na(fig_width)) NULL else fig_width * px_per_in
  plot_height <- if (is.na(fig_height)) NULL else fig_height * px_per_in

  gg_object %>%
    plotly::ggplotly(
      tooltip = "text",
      width = plot_width,
      height = plot_height
    ) %>%
    plotly::layout(
      xaxis = list(side = "top")
    )
}
