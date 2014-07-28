# tests for initialize
context("init")

# remove rrttemp dir if exists
unlink("~/rrttemp", recursive = TRUE, force = TRUE)


test_that("init works as expected", {
  options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))
  path <- "~/rrttemp"
  rrt_init(path, verbose = FALSE, autosnap = TRUE)
  expect_true(is_rrt(path, FALSE))
  expect_that(is_rrt("~/", FALSE), not(is_true()))
  expect_equal(list.files(path), "rrt")
})

test_that("init returns messages", {
  expect_message(rrt_init(path, autosnap = TRUE), "Checking to see if repository exists already")
})

# cleanup
unlink("~/rrttemp", recursive = TRUE, force = TRUE)