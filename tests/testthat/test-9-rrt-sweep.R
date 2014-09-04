# tests for sweep
context("sweep")
opts <- options(verbose=FALSE)

rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))


test_that("sweep works as expected", {
  rrt_init(rrt_path, verbose = FALSE, autosnap = TRUE, quiet=TRUE)
  cat("library(stringr)", file=file.path(rrt_path, "code.R"))
  rrt_refresh(rrt_path, autosnap = TRUE, verbose = FALSE, quiet=TRUE)

  expect_true(is_rrt(rrt_path, FALSE))
  expect_false(is_rrt("~/", FALSE))
  expect_true(all(c("code.R", "rrt") %in% list.files(rrt_path)))
})

test_that("sweep returns messages", {
  expect_message(rrt_sweep(rrt_path), "Checking to make sure repository exists")
  expect_message(rrt_sweep(rrt_path), "Package sources removed")
  expect_message(rrt_sweep(rrt_path), "Checking to make sure rrt directory exists inside your repository")
})


test_that("sweep actually removes installed packages and sources", {
  options(verbose=FALSE)
  rrt_install(rrt_path, quiet=TRUE)
  installed <- list.files(rrtPath(rrt_path, "lib"))
  installed_pre <- installed[!installed %in% "src"]
  src_pre <- list.files(rrtPath(rrt_path, "src"))
  
  expect_message(rrt_sweep(rrt_path), "stringr")
  installed <- list.files(rrtPath(rrt_path, "lib"))
  installed_post <- installed[!installed %in% "src"]
  src_post <- list.files(rrtPath(rrt_path, "src"))
  expect_equal(installed_pre, c("stringr"))
  expect_equal(installed_post, character(0))
  expect_equal(src_post, character(0))
  
})

# cleanup
options(opts)
cleanRRTfolder()
