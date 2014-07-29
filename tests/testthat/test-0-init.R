# tests for initialize
context("init")



rrtPath <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))


test_that("init works as expected", {
  rrt_init(rrtPath, verbose = FALSE, autosnap = TRUE)
  expect_true(is_rrt(rrtPath, FALSE))
  expect_false(is_rrt("~/", FALSE))
  expect_true("rrt" %in% list.files(rrtPath))
})

test_that("init returns messages", {
  expect_message(rrt_init(rrtPath, autosnap = TRUE), "Checking to see if repository exists already")
})

# cleanup
cleanRRTfolder()