`%>%` <- magrittr::`%>%`

log_file <- file.path("logs", "best_picture_nominees_scrape.log")
dir.create("logs", showWarnings = FALSE, recursive = TRUE)

log_line <- function(level, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] [%s] %s", timestamp, level, message)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

log_info <- function(message) log_line("INFO", message)
log_warn <- function(message) log_line("WARN", message)
log_error <- function(message) log_line("ERROR", message)

imdb_user_agent <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/120.0.0.0 Safari/537.36"
)

fetch_html <- function(url, label = NULL, retries = 3, pause_seconds = 1) {
  if (is.null(label) || !nzchar(label)) {
    label <- url
  }
  for (attempt in seq_len(retries)) {
    resp <- tryCatch(
      httr::GET(
        url,
        httr::user_agent(imdb_user_agent),
        httr::accept("text/html")
      ),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      log_warn(sprintf("%s: request error on attempt %s (%s)", label, attempt, resp$message))
    } else if (httr::status_code(resp) >= 200 && httr::status_code(resp) < 300) {
      html_text <- httr::content(resp, as = "text", encoding = "UTF-8")
      return(xml2::read_html(html_text))
    } else {
      log_warn(sprintf("%s: status %s on attempt %s", label, httr::status_code(resp), attempt))
    }

    Sys.sleep(pause_seconds)
  }

  stop(sprintf("Failed to fetch %s after %s attempts.", label, retries))
}

parse_next_data <- function(html_object) {
  script_node <- html_object %>%
    rvest::html_node("script#__NEXT_DATA__")

  if (is.na(script_node) || !length(script_node)) {
    stop("IMDb page did not include __NEXT_DATA__.")
  }

  jsonlite::fromJSON(rvest::html_text(script_node), simplifyVector = FALSE)
}

get_int <- function(value, default = NA_integer_) {
  if (is.null(value) || !length(value)) {
    return(default)
  }
  as.integer(value)
}

extract_history_years <- function(next_data) {
  history <- next_data$props$pageProps$historyEventEditions
  if (is.null(history) || !length(history)) {
    stop("Could not find historyEventEditions on the event page.")
  }

  year <- vapply(history, function(x) get_int(x$year), integer(1))
  instance <- vapply(history, function(x) get_int(x$instanceWithinYear, 1L), integer(1))

  tibble::tibble(year = year, instance = instance) %>%
    dplyr::filter(!is.na(year))
}

extract_best_picture_from_next_data <- function(next_data, year) {
  awards <- next_data$props$pageProps$edition$awards
  if (is.null(awards) || !length(awards)) {
    return(tibble::tibble())
  }

  oscars <- awards[vapply(awards, function(x) identical(x$text, "Oscar"), logical(1))]
  if (!length(oscars)) {
    oscars <- awards
  }

  categories <- oscars[[1]]$nominationCategories$edges
  if (is.null(categories) || !length(categories)) {
    return(tibble::tibble())
  }

  pattern <- paste(
    "Best Motion Picture of the Year",
    "Best Picture",
    "Outstanding Production",
    "Outstanding Picture",
    "Best Unique and Artistic Picture",
    sep = "|"
  )

  matched_categories <- categories[
    vapply(
      categories,
      function(edge) {
        category <- edge$node$category$text
        !is.null(category) && grepl(pattern, category, ignore.case = TRUE)
      },
      logical(1)
    )
  ]

  if (!length(matched_categories)) {
    return(tibble::tibble())
  }

  nomination_edges <- unlist(
    lapply(
      matched_categories,
      function(edge) edge$node$nominations$edges
    ),
    recursive = FALSE
  )

  if (!length(nomination_edges)) {
    return(tibble::tibble())
  }

  rows <- unlist(
    lapply(
      nomination_edges,
      function(edge) {
        node <- edge$node
        titles <- node$awardedEntities$awardTitles
        if (is.null(titles) || !length(titles)) {
          return(NULL)
        }

        lapply(
          titles,
          function(award_title) {
            title_info <- award_title$title
            if (is.null(title_info)) {
              return(NULL)
            }

            list(
              year = year,
              title = title_info$titleText$text,
              id = title_info$id,
              nominees_or_winner = if (isTRUE(node$isWinner)) "winner" else "nominee"
            )
          }
        )
      }
    ),
    recursive = FALSE
  )

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(lapply(rows, tibble::as_tibble))
}

get_display_sleep <- function() {
  pmset_path <- Sys.which("pmset")
  if (!nzchar(pmset_path)) {
    return(NA_integer_)
  }

  output <- suppressWarnings(system2(pmset_path, c("-g", "custom"), stdout = TRUE, stderr = TRUE))
  display_line <- grep("displaysleep", output, value = TRUE)
  if (!length(display_line)) {
    return(NA_integer_)
  }

  value <- suppressWarnings(as.integer(gsub("^.*displaysleep\\s+", "", display_line[[1]])))
  if (is.na(value)) {
    return(NA_integer_)
  }

  value
}

set_display_sleep <- function(value) {
  pmset_path <- Sys.which("pmset")
  if (!nzchar(pmset_path)) {
    return(FALSE)
  }

  result <- suppressWarnings(system2(pmset_path, c("-a", "displaysleep", value), stdout = TRUE, stderr = TRUE))
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    log_warn(sprintf("pmset failed to set displaysleep to %s", value))
    return(FALSE)
  }

  TRUE
}

keep_screen_awake <- function() {
  state <- list(method = "none", display_sleep = NA_integer_, changed = FALSE)

  display_sleep <- get_display_sleep()
  if (!is.na(display_sleep)) {
    if (set_display_sleep(0)) {
      state$method <- "pmset"
      state$display_sleep <- display_sleep
      state$changed <- TRUE
      log_info("Set displaysleep to 0 for the scrape.")
      return(state)
    }
  }

  caffeinate_path <- Sys.which("caffeinate")
  if (nzchar(caffeinate_path)) {
    system2(caffeinate_path, c("-dimsu", "-w", Sys.getpid()), wait = FALSE)
    state$method <- "caffeinate"
    log_info("Started caffeinate to keep the screen awake.")
  } else {
    log_warn("Could not keep the screen awake (pmset/caffeinate unavailable).")
  }

  state
}

restore_screen_timeout <- function(state) {
  if (!is.null(state) && state$method == "pmset" && isTRUE(state$changed)) {
    if (set_display_sleep(state$display_sleep)) {
      log_info(sprintf("Restored displaysleep to %s.", state$display_sleep))
    } else {
      log_warn("Failed to restore displaysleep; please reset manually if needed.")
    }
  }
}

send_done_email <- function(status, error_message = NULL, log_path = log_file) {
  email_to <- Sys.getenv("SCRAPE_EMAIL_TO")
  if (!nzchar(email_to)) {
    log_warn("SCRAPE_EMAIL_TO not set; skipping email notification.")
    return(invisible(FALSE))
  }

  subject <- sprintf("Best Picture nominees scrape %s", status)
  body <- paste0(
    "Best Picture nominees scrape ", status, ".\n",
    "Finished: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    if (!is.null(error_message)) paste0("Error: ", error_message, "\n") else "",
    "Log: ", normalizePath(log_path, winslash = "/", mustWork = FALSE), "\n"
  )

  gmail_notify_path <- here::here("scripts", "gmail_notify.R")
  if (file.exists(gmail_notify_path)) {
    log_info("Sending completion email via gmailr.")
    from <- Sys.getenv("SCRAPE_EMAIL_FROM", email_to)
    client_secret <- Sys.getenv("SCRAPE_GMAIL_CLIENT_SECRET", "~/.R/gmailr/client_secret.json")
    cache <- Sys.getenv("SCRAPE_GMAIL_CACHE", "~/.R/gmailr")

    result <- tryCatch({
      source(gmail_notify_path)
      send_gmail_notification(
        to = email_to,
        subject = subject,
        body = body,
        from = from,
        client_secret = client_secret,
        cache = cache
      )
      TRUE
    }, error = function(e) {
      log_warn(sprintf("gmailr notification failed: %s", conditionMessage(e)))
      FALSE
    })

    if (isTRUE(result)) {
      log_info(sprintf("Sent completion email to %s via gmailr.", email_to))
      return(invisible(TRUE))
    }
  }

  mail_cmd <- Sys.getenv("SCRAPE_MAIL_CMD", "mail")
  mail_path <- Sys.which(mail_cmd)
  if (!nzchar(mail_path)) {
    log_warn(sprintf("Mail command '%s' not found; skipping email notification.", mail_cmd))
    return(invisible(FALSE))
  }

  result <- suppressWarnings(system2(mail_path, c("-s", subject, email_to), input = body))
  status_code <- attr(result, "status")
  if (!is.null(status_code) && status_code != 0) {
    log_warn("Mail command returned a non-zero status; email may not have sent.")
    return(invisible(FALSE))
  }

  log_info(sprintf("Sent completion email to %s via mail.", email_to))
  invisible(TRUE)
}

run_scrape <- function() {
  log_info("Starting Best Picture nominees scrape.")

  url <- "https://www.imdb.com/event/ev0000003/?ref_=ev_eh"
  year_html <- fetch_html(url, label = "Oscar event page")
  year_data <- parse_next_data(year_html)
  year_tib <- extract_history_years(year_data)

  log_info(sprintf("Found %s Oscar years to fetch.", nrow(year_tib)))

  best_pic_noms_list <- lapply(seq_len(nrow(year_tib)), function(idx) {
    if (idx %% 25 == 0) {
      log_info(sprintf("Fetched %s/%s Oscar year pages.", idx, nrow(year_tib)))
    }
    year <- year_tib$year[[idx]]
    instance <- year_tib$instance[[idx]]
    year_url <- paste0(
      "https://www.imdb.com/event/ev0000003/",
      year,
      "/",
      instance,
      "/?ref_=ev_eh"
    )
    year_html <- fetch_html(year_url, label = paste("Oscar year", year))
    year_next_data <- parse_next_data(year_html)
    extract_best_picture_from_next_data(year_next_data, year)
  })

  best_pic_noms <- dplyr::bind_rows(best_pic_noms_list) %>%
    dplyr::filter(grepl(x = id, pattern = "^tt")) %>%
    dplyr::mutate(imdb_url = paste0("https://www.imdb.com/title/", id, "/"))

  log_info(sprintf("Found %s Best Picture nominees.", nrow(best_pic_noms)))

  imdb_production_companies <- function(html_object) {
    nodes <- html_object %>%
      rvest::html_nodes("#company_credits_content > ul")
    if (!length(nodes)) {
      return(NA_character_)
    }

    nodes %>%
      rvest::html_text() %>%
      .[1] %>%
      stringr::str_split(pattern = "\n") %>%
      unlist() %>%
      stringr::str_trim() %>%
      stringr::str_squish() %>%
      `[`(. != "") %>%
      paste(collapse = "|") %>%
      stringr::str_remove_all("\\s*\\([^\\)]+\\)")
  }

  imdb_company_urls <- paste0(best_pic_noms$imdb_url, "companycredits?ref_=ttfc_sa_3")

  imdb_prod_html_list <- lapply(seq_along(imdb_company_urls), function(idx) {
    if (idx %% 50 == 0) {
      log_info(sprintf("Fetched %s/%s company credit pages.", idx, length(imdb_company_urls)))
    }
    fetch_html(imdb_company_urls[[idx]], label = paste("Company credits", best_pic_noms$id[[idx]]))
  })

  production_companies <- vapply(
    imdb_prod_html_list,
    imdb_production_companies,
    FUN.VALUE = character(1)
  )

  best_pic_noms_final <- best_pic_noms %>%
    dplyr::mutate(producers = production_companies) %>%
    dplyr::distinct()

  log_info(sprintf("Built final dataset with %s rows.", nrow(best_pic_noms_final)))
  best_pic_noms_final
}

scrape_status <- "success"
error_message <- NULL
screen_state <- keep_screen_awake()

on.exit({
  restore_screen_timeout(screen_state)
  send_done_email(scrape_status, error_message, log_file)
}, add = TRUE)

best_pic_noms_final <- tryCatch(
  run_scrape(),
  error = function(e) {
    scrape_status <<- "failed"
    error_message <<- conditionMessage(e)
    log_error(sprintf("Scrape failed: %s", error_message))
    stop(e)
  }
)

# best_pic_noms_final %>% readr::write_csv(here::here("data", "best_picture_nominees.csv"))
