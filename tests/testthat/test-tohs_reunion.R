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
