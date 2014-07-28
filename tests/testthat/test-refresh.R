# tests for refresh
context("refresh")

# remove rrttemp dir if exists
unlink("~/rrttemp", recursive = TRUE, force = TRUE)


test_that("refresh works as expected", {
  options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))
  path <- "~/rrttemp"
  rrt_init(path, verbose = FALSE, autosnap = TRUE)
  cat("library(bmc)", file=file.path(path, "code.R"))
  rrt_refresh(path, autosnap = TRUE, verbose = FALSE)
  
  
  expect_true(is_rrt(path, FALSE))
  expect_that(is_rrt("~/", FALSE), not(is_true()))
  expect_equal(list.files(path), "rrt")
})

test_that("refresh returns messages", {
  expect_message(rrt_refresh(path), "Checking to see if repository exists already")
})

# cleanup
unlink("~/rrttemp", recursive = TRUE, force = TRUE)