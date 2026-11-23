`%>%` <- magrittr::`%>%`

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

extract_imdb_id <- function(input) {
  if (grepl("^tt\\d+$", input)) {
    return(input)
  }
  id <- stringr::str_match(input, "tt\\d+")[, 1]
  if (is.na(id)) {
    stop("Could not extract IMDb ID from input: ", input)
  }
  id
}

# Episode data comes from IMDb's official dataset dumps (see
# imdb_datasets.R). IMDb's WAF 403s server-side requests to its GraphQL
# endpoint, so we no longer query it.
source(here::here("web_scraping", "imdb_datasets.R"))

get_imdb_all_episodes <- function(imdb_input) {
  get_imdb_dataset_episodes(extract_imdb_id(imdb_input))
}

# -----------------------------------------------------------------------------
# Safe per-show updater
# -----------------------------------------------------------------------------

# Update one show's CSV, with safety checks. Returns a 1-row tibble describing
# the outcome so the runner can print a summary at the end.
#
# Safety checks (any failure leaves the existing CSV untouched):
#   - Dataset download or read errors are caught (one bad show does not kill the run).
#   - The fetched data must have rows.
#   - The fetched data must have all required columns.
#   - The fetched data must not have FEWER rows than the existing CSV
#     (unless allow_shrink is TRUE, e.g. for an intentional one-off refresh).
#   - The fetched data's max season must not be LESS than the existing CSV's
#     max season (same allow_shrink override applies).
#
# The IMDB_ALLOW_SHRINK=1 env var enables shrink (use sparingly; the cron job
# never sets it, so silent shrinks are blocked in CI).
update_show <- function(name, imdb_input, csv_path,
                        allow_shrink = identical(
                          Sys.getenv("IMDB_ALLOW_SHRINK"), "1"
                        )) {
  status <- function(state, fetched_rows = NA_integer_, existing_rows = NA_integer_,
                     message = NA_character_) {
    tibble::tibble(
      name = name,
      state = state,
      fetched_rows = fetched_rows,
      existing_rows = existing_rows,
      message = message
    )
  }

  existing <- if (file.exists(csv_path)) {
    suppressMessages(readr::read_csv(csv_path, show_col_types = FALSE))
  } else {
    NULL
  }
  existing_rows <- if (is.null(existing)) 0L else nrow(existing)

  fetched <- tryCatch(
    get_imdb_all_episodes(imdb_input),
    error = function(e) e
  )

  if (inherits(fetched, "error")) {
    return(status("failed", existing_rows = existing_rows,
                  message = paste("fetch error:", conditionMessage(fetched))))
  }

  required_cols <- c("season", "episode", "title", "rating", "votes")
  if (!all(required_cols %in% names(fetched))) {
    missing <- setdiff(required_cols, names(fetched))
    return(status("failed", fetched_rows = nrow(fetched),
                  existing_rows = existing_rows,
                  message = paste("missing columns:", paste(missing, collapse = ", "))))
  }

  if (nrow(fetched) == 0) {
    return(status("failed", fetched_rows = 0L, existing_rows = existing_rows,
                  message = "fetch returned zero rows"))
  }

  if (!is.null(existing) && nrow(fetched) < existing_rows) {
    if (!isTRUE(allow_shrink)) {
      return(status("failed", fetched_rows = nrow(fetched),
                    existing_rows = existing_rows,
                    message = sprintf(
                      "fetched %d rows < existing %d rows (no-shrink guard)",
                      nrow(fetched), existing_rows
                    )))
    } else {
      message(sprintf(
        "  [allow_shrink] %s: %d -> %d rows",
        name, existing_rows, nrow(fetched)
      ))
    }
  }

  if (!is.null(existing) && existing_rows > 0 &&
      "season" %in% names(existing)) {
    new_max_season <- suppressWarnings(max(fetched$season, na.rm = TRUE))
    old_max_season <- suppressWarnings(max(existing$season, na.rm = TRUE))
    if (is.finite(new_max_season) && is.finite(old_max_season) &&
        new_max_season < old_max_season) {
      if (!isTRUE(allow_shrink)) {
        return(status("failed", fetched_rows = nrow(fetched),
                      existing_rows = existing_rows,
                      message = sprintf(
                        "fetched max season %s < existing max season %s",
                        new_max_season, old_max_season
                      )))
      } else {
        message(sprintf(
          "  [allow_shrink] %s: max season %s -> %s",
          name, old_max_season, new_max_season
        ))
      }
    }
  }

  readr::write_csv(fetched, csv_path)

  # Compare by value (column-wise) rather than identical(): readr::read_csv
  # reads ints as doubles, so a strict identical() comparison would always
  # mark the file as changed even when nothing semantically did.
  changed <- if (is.null(existing)) {
    TRUE
  } else if (!identical(dim(existing), dim(fetched)) ||
             !identical(sort(names(existing)), sort(names(fetched)))) {
    TRUE
  } else {
    !isTRUE(all.equal(
      as.data.frame(existing)[, names(fetched), drop = FALSE],
      as.data.frame(fetched),
      check.attributes = FALSE
    ))
  }
  status(
    if (changed) "updated" else "unchanged",
    fetched_rows = nrow(fetched),
    existing_rows = existing_rows
  )
}

# -----------------------------------------------------------------------------
# Show config
# -----------------------------------------------------------------------------

shows <- tibble::tribble(
  ~name,                ~imdb_id,      ~slug,
  "alice in borderland", "tt10795658", "alice_in_borderland",
  "game of thrones",    "tt0944947",   "game_of_thrones",
  "breaking bad",       "tt0903747",   "breaking_bad",
  "always sunny",       "tt0472954",   "always_sunny",
  "himym",              "tt0460649",   "himym",
  "better call saul",   "tt3032476",   "better_call_saul",
  "zerozerozero",       "tt8332438",   "zerozerozero",
  "hotd",               "tt11198330",  "hotd",
  "bluey",              "tt7678620",   "bluey",
  "westworld",          "tt0475784",   "westworld",
  "paris hilton bff",   "tt1292967",   "paris_hilton_bff",
  "stranger things",    "tt4574334",   "stranger_things",
  "bojack",             "tt3398228",   "bojack",
  "velma",              "tt14153790",  "velma",
  "suits",              "tt1632701",   "suits",
  "vampire diaries",    "tt1405406",   "vampire_diaries",
  "greys anatomy",      "tt0413573",   "greys_anatomy",
  "friends",            "tt0108778",   "friends",
  "house",              "tt0412142",   "house",
  "simpsons",           "tt0096697",   "simpsons",
  "pretty little liars", "tt1578873",  "pretty_little_liars",
  "the crown",          "tt4786824",   "crown",
  "love island (uk)",   "tt4770018",   "love_island",
  "south park",         "tt0121955",   "south_park",
  "family guy",         "tt0182576",   "family_guy",
  "bobs burgers",        "tt1561755",   "bobs_burgers",
  "invincible",         "tt6741278",   "invincible",
  "clone wars",         "tt0458290",   "clone_wars",
  "mr robot",           "tt4158110",   "mr_robot",
  "sparticus",          "tt1442449",   "sparticus",
  "attack on titan",    "tt2560140",   "attack_on_titan",
  "mad men",            "tt0804503",   "mad_men",
  "blue eye samurai",   "tt13309742",  "blue_eye_samurai",
  "six feet under",     "tt0248654",   "six_feet_under",
  "hannibal",           "tt2243973",   "hannibal",
  "the wire",           "tt0306414",   "the_wire",
  "person of interest", "tt1839578",   "person_of_interest",
  "chernobyl",          "tt7366338",   "chernobyl",
  "fleabag",            "tt5687612",   "fleabag",
  "parks and recreation", "tt1266020", "parks_and_rec",
  "new girl",           "tt1826940",   "new_girl",
  "brooklyn nine-nine", "tt2467372",   "brooklyn_nine_nine",
  "avatar",             "tt0417299",   "avatar",
  "the office us",      "tt0386676",   "the_office",
  "death note",         "tt0877057",   "death_note",
  "andor",              "tt9253284",   "andor",
  "modern love",        "tt8543390",   "modern_love",
  "my adventures with superman", "tt14681924", "my_adventures_with_superman",
  "my hero academia",   "tt5626028",   "my_hero_academia",
  "x-men 97",           "tt16026746",  "x_men_97",
  "the boys",           "tt1190634",   "the_boys",
  "the punisher",       "tt5675620",   "the_punisher",
  "schitt's creek",     "tt3526078",   "schitts_creek",
  "batman caped crusader", "tt14681596", "batman_caped_crusader",
  "batman the animated series", "tt0103359", "batman_animated_series",
  "ted lasso",          "tt10986410",  "ted_lasso",
  "rick and morty",     "tt2861424",   "rick_and_morty",
  "gravity falls",      "tt1865718",   "gravity_falls",
  "seinfeld",           "tt0098904",   "seinfeld",
  "sherlock",           "tt1475582",   "sherlock",
  "daredevil",          "tt3322312",   "daredevil",
  "the mandalorian",    "tt8111088",   "the_mandalorian",
  "the owl house",      "tt8050756",   "the_owl_house",
  "archer",             "tt1486217",   "archer",
  "heated rivalry",     "tt35495073",  "heated_rivalry",
  "scrubs",             "tt0285403",   "scrubs",
  "the last of us",     "tt3581920",   "the_last_of_us",
  "barry",              "tt5348176",   "barry",
  "community",          "tt1439629",   "community",
  "futurama",           "tt0149460",   "futurama",
  "arcane",             "tt11126994",  "arcane"
)

# -----------------------------------------------------------------------------
# Run
# -----------------------------------------------------------------------------

run_all <- function(shows_df) {
  results <- vector("list", nrow(shows_df))
  for (i in seq_len(nrow(shows_df))) {
    row <- shows_df[i, ]
    csv_path <- here::here("data", paste0(row$slug, "_ep_ratings.csv"))
    cat(sprintf("[%2d/%2d] %s ... ", i, nrow(shows_df), row$name))
    res <- update_show(row$name, row$imdb_id, csv_path)
    cat(res$state)
    if (!is.na(res$message)) cat(" (", res$message, ")", sep = "")
    cat("\n")
    results[[i]] <- res
  }
  dplyr::bind_rows(results)
}

# Only run when the script is executed (not when sourced for its function defs).
if (!interactive() && identical(sys.nframe(), 0L)) {
  summary_df <- run_all(shows)

  cat("\n==== Summary ====\n")
  state_counts <- table(summary_df$state)
  for (state in names(state_counts)) {
    cat(sprintf("  %s: %d\n", state, state_counts[[state]]))
  }

  failures <- summary_df %>% dplyr::filter(state == "failed")
  if (nrow(failures) > 0) {
    cat("\n==== Failures ====\n")
    for (i in seq_len(nrow(failures))) {
      cat(sprintf("  - %s: %s\n", failures$name[i], failures$message[i]))
    }
    quit(status = 1)
  }
}
