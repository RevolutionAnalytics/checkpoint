# tests for initialize
context("init")

# remove rrttemp dir if exists
unlink("~/rrttemp", recursive = TRUE, force = TRUE)

options(repos=structure(c(CRAN="http://cran.revolutionanalytics.com/")))
path <- "~/rrttemp"
rrt_init(path, verbose = FALSE)

test_that("init works as expected", {
  expect_true(is_rrt(path, FALSE))
  expect_that(is_rrt("~/", FALSE), not(is_true()))
  expect_equal(list.files(path), "rrt")
})

test_that("init returns messages", {
  expect_message(rrt_init(path), "Checking to see if repository exists already")
})

# cleanup
unlink("~/rrttemp", recursive = TRUE, force = TRUE)