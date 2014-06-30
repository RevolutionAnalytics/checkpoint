# tests for sweep
context("sweep")

# remove rrttemp dir if exists
unlink("~/rrttemp", recursive = TRUE, force = TRUE)

options(repos=structure(c(CRAN="http://cran.revolutionanalytics.com/")))
path <- "~/rrttemp"
rrt_init(path, verbose = FALSE)
cat("library(stringr)", file=file.path(path, "code.R"))
rrt_refresh(path, autosnap = TRUE, verbose = FALSE)

test_that("sweep works as expected", {
  expect_true(is_rrt(path, FALSE))
  expect_that(is_rrt("~/", FALSE), not(is_true()))
  expect_equal(list.files(path), c("code.R", "rrt"))
})

test_that("sweep returns messages", {
  expect_message(rrt_sweep(path), "Checking to make sure repository exists")
})

rrt_install(path)
installed <- list.files(rrt_libpath(path))
installed_pre <- installed[!installed %in% "src"]
src_pre <- list.files(rrt_libpath(path), "src/contrib")

rrt_sweep(path)
installed <- list.files(rrt_libpath(path))
installed_post <- installed[!installed %in% "src"]
src_post <- list.files(file.path(rrt_libpath(path), "src/contrib"))

test_that("sweep actually removes installed packages and sources", {
  expect_equal(installed_pre, c("stringr", "testthat"))
  expect_equal(installed_post, c("stingr", "testthat"))
  
  expect_equal(length(installed_post), 0)
  expect_equal(length(src_post), 0)
})

# cleanup
unlink("~/rrttemp", recursive = TRUE, force = TRUE)