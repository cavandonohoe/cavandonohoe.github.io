`%>%` <- magrittr::`%>%`

# =============================================================================
# AVCA Men's Volleyball First-Team All-Americans
#
# HOW TO UPDATE EACH YEAR:
#   1. Check if AVCA updated the Excel file URL (search their awards history
#      page). If the URL changed, update `excel_url` below.
#   2. Add the new year's champion to `champions` below.
#   3. If the Excel file doesn't include the latest year yet, add the players
#      manually to `manual_additions` below. Once the Excel catches up, remove
#      that year's manual block.
#   4. Run this script: source("web_scraping/avca_all_americans.R")
#   5. Rebuild the site.
# =============================================================================

# --- CONFIG: update these each year ------------------------------------------

excel_url <- paste0(
  "https://www.avca.org/wp-content/uploads/2024/05/",
  "AVCA-NCAA-National-Collegiate-MVB-All-America-Teams-Year-by-Year.xlsx"
)

first_year <- 2016

champions <- tibble::tibble(
  year = c(2016L, 2017L, 2018L, 2019L, 2020L, 2021L, 2022L, 2023L, 2024L, 2025L),
  champion = c(
    "Ohio State University",
    "Ohio State University",
    "Long Beach State University",
    "Long Beach State University",
    NA_character_,
    "University of Hawai'i",
    "University of Hawai'i",
    "UCLA",
    "UCLA",
    "Long Beach State University"
  ),
  champion_short = c(
    "Ohio State",
    "Ohio State",
    "Long Beach State",
    "Long Beach State",
    "Canceled (COVID-19)",
    "Hawai'i",
    "Hawai'i",
    "UCLA",
    "UCLA",
    "Long Beach State"
  )
)

# Manual additions for years not yet in the AVCA Excel file.
# Remove a year's block once the Excel is updated to include it.
manual_additions <- dplyr::bind_rows(
  tibble::tibble(
    year = 2025L,
    name = c(
      "Ryan Barnett", "Hilir Henno", "Dillon Klein", "Moni Nikolov",
      "Jalen Phillips", "Cooper Robinson", "Tread Rosenthal",
      "Adrien Roure", "Andrew Rowan", "Cameron Thorne",
      "Parker Van Buren", "Skyler Varga"
    ),
    school = c(
      "Pepperdine University", "UC Irvine",
      "University of Southern California", "Long Beach State University",
      "California State University Northridge", "UCLA",
      "University of Hawai'i", "University of Hawai'i",
      "UCLA", "UCLA",
      "Loyola University Chicago", "Long Beach State University"
    ),
    position = c("OH", "OH", "OH", "S", "OPP", "OH", "S", "OH", "S", "MB", "OH", "OH"),
    class = c(
      "RS-Jr.", "Sr.", "Jr.", "Fr.", "RS-So.", "RS-Jr.",
      "So.", "Fr.", "Jr.", "Jr.", "RS-Sr.", "Jr."
    )
  )
)

# --- DOWNLOAD AND PARSE THE EXCEL FILE --------------------------------------

cache_path <- file.path(tempdir(), "avca_all_america_by_year.xlsx")

if (!file.exists(cache_path)) {
  message("Downloading AVCA Excel file...")
  utils::download.file(excel_url, cache_path, mode = "wb", quiet = TRUE)
} else {
  message("Using cached Excel file: ", cache_path)
}

sheet_names <- readxl::excel_sheets(cache_path)
year_sheets <- sort(as.integer(sheet_names[grepl("^\\d{4}$", sheet_names)]))
year_sheets <- year_sheets[year_sheets >= first_year]

message("Parsing sheets: ", paste(year_sheets, collapse = ", "))

parse_first_team <- function(sheet_year, path) {
  d <- readxl::read_excel(path, sheet = as.character(sheet_year))

  first_team_row <- which(d[[1]] == "First Team")
  second_team_row <- which(d[[1]] == "Second Team")

  if (length(first_team_row) == 0) return(NULL)
  if (length(second_team_row) == 0) second_team_row <- nrow(d) + 1

  data_start <- first_team_row + 2
  data_end <- second_team_row - 2
  if (data_start > data_end) return(NULL)

  rows <- d[data_start:data_end, ]

  tibble::tibble(
    year = as.integer(sheet_year),
    name = paste(as.character(rows[[1]]), as.character(rows[[2]])),
    school = as.character(rows[[3]]),
    position = as.character(rows[[4]]),
    class = as.character(rows[[5]])
  ) %>%
    dplyr::filter(!is.na(name), name != "NA NA")
}

excel_data <- purrr::map_dfr(year_sheets, parse_first_team, path = cache_path)

# --- MERGE MANUAL ADDITIONS -------------------------------------------------

excel_years <- unique(excel_data$year)
manual_new <- manual_additions %>%
  dplyr::filter(!year %in% excel_years)

if (nrow(manual_new) > 0) {
  message("Adding manual data for: ", paste(unique(manual_new$year), collapse = ", "))
}

all_americans <- dplyr::bind_rows(excel_data, manual_new) %>%
  dplyr::mutate(school = stringr::str_replace_all(school, "\u2018|\u2019", "'")) %>%
  dplyr::arrange(year, name)

# --- SAVE --------------------------------------------------------------------

out_path <- here::here("data", "avca_first_team_all_americans.csv")
readr::write_csv(all_americans, out_path)
message("Wrote ", nrow(all_americans), " rows to ", out_path)

champ_path <- here::here("data", "avca_champions.csv")
readr::write_csv(champions, champ_path)
message("Wrote champions to ", champ_path)

# --- SUMMARY -----------------------------------------------------------------

message("\nPlayers per year:")
print(table(all_americans$year))
