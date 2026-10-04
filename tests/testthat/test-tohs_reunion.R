source("../../scripts/refresh_tohs_reunion.R")

testthat::test_that("blank emails stay missing and private columns are discarded", {
  sheet <- data.frame(
    `First Name` = c("Header", " Ada ", "Ben", "Cara", ""),
    `Last Name` = c("Header", " Lovelace ", "Smith", "Jones", ""),
    Email = c("Header", "private@example.com", "   ", NA, ""),
    Phone = rep("private", 5), check.names = FALSE
  )
  result <- tohs_public_rows(sheet)
  testthat::expect_identical(names(result), c("first_name", "last_name", "email_bool"))
  testthat::expect_identical(result$email_bool, c(TRUE, FALSE, FALSE))
  testthat::expect_identical(result$first_name, c("Ada", "Ben", "Cara"))
  testthat::expect_false(any(grepl("@|private", unlist(result))))
})

testthat::test_that("empty or changed sheets fail rather than publish bad coverage", {
  testthat::expect_error(tohs_public_rows(data.frame(Name = "Ada")), "columns changed")
  sheet <- data.frame(`First Name` = "Header", `Last Name` = "Header", Email = "", check.names = FALSE)
  testthat::expect_error(tohs_public_rows(sheet), "no graduates")
})

testthat::test_that("V2 export rejects private fields and malformed flags", {
  public <- data.frame(first_name = "Meghan", last_name = "Conlan", email_bool = "TRUE",
                       updated_at = "2026-10-04T00:00:00.000Z", stringsAsFactors = FALSE)
  testthat::expect_true(tohs_validate_public(public, FALSE)$email_bool)
  private <- public
  private$email <- "private@example.invalid"
  testthat::expect_error(tohs_validate_public(private, FALSE), "schema changed")
  public$email_bool <- "yes"
  testthat::expect_error(tohs_validate_public(public, FALSE), "coverage flag")
  public$email_bool <- "TRUE"
  public$first_name <- "private@example.invalid"
  testthat::expect_error(tohs_validate_public(public, FALSE), "Unsafe public name")
})

testthat::test_that("stale V2 exports never replace the last good snapshot", {
  public <- data.frame(first_name = "Meghan", last_name = "Conlan", email_bool = "TRUE",
                       updated_at = "2000-01-01T00:00:00.000Z", stringsAsFactors = FALSE)
  testthat::expect_error(tohs_validate_public(public), "stale")
})
