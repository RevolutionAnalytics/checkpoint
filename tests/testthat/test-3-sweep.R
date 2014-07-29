# tests for sweep
context("sweep")
opts <- options(verbose=FALSE)

rrtPath <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))


test_that("sweep works as expected", {
  rrt_init(rrtPath, verbose = FALSE, autosnap = TRUE, quiet=TRUE)
  cat("library(stringr)", file=file.path(rrtPath, "code.R"))
  rrt_refresh(rrtPath, autosnap = TRUE, verbose = FALSE, quiet=TRUE)

  expect_true(is_rrt(rrtPath, FALSE))
  expect_false(is_rrt("~/", FALSE))
  expect_true(all(c("code.R", "rrt") %in% list.files(rrtPath)))
})

test_that("sweep returns messages", {
  expect_message(rrt_sweep(rrtPath), "Checking to make sure repository exists")
  expect_message(rrt_sweep(rrtPath), "Package sources removed")
  expect_message(rrt_sweep(rrtPath), "Checking to make sure rrt directory exists inside your repository")
})


test_that("sweep actually removes installed packages and sources", {
  options(verbose=FALSE)
  rrt_install(rrtPath, quiet=TRUE)
  installed <- list.files(rrt_libpath(rrtPath))
  installed_pre <- installed[!installed %in% "src"]
  src_pre <- list.files(file.path(rrt_libpath(rrtPath), "src/contrib"))
  
  expect_message(rrt_sweep(rrtPath), "stringr")
  installed <- list.files(rrt_libpath(rrtPath))
  installed_post <- installed[!installed %in% "src"]
  src_post <- list.files(file.path(rrt_libpath(rrtPath), "src/contrib"))
  expect_equal(installed_pre, c("stringr"))
  expect_equal(installed_post, character(0))
  expect_equal(src_post, character(0))
  
})

# cleanup
options(opts)
cleanRRTfolder()