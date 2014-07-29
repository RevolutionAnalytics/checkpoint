# tests for refresh
context("refresh")

# remove rrttemp dir if exists

rrtPath <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))

test_that("refresh works as expected", {
  
  expect_false("rrt" %in% list.files(rrtPath))
  
  rrt_init(rrtPath, verbose = FALSE, autosnap = TRUE)
  cat("library(bmc)", file=file.path(rrtPath, "code.R"))
  rrt_refresh(rrtPath, autosnap = TRUE, verbose = FALSE)
  
  
  expect_true(is_rrt(rrtPath, FALSE))
  expect_that(is_rrt("~/", FALSE), not(is_true()))
  expect_true("rrt" %in% list.files(rrtPath))
})

test_that("refresh returns messages", {
  expect_message(rrt_refresh(rrtPath), 
                 "Checking to see if repository exists already")
})

# cleanup
cleanRRTfolder()