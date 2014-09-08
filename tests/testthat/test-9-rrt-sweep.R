# tests for sweep
context("sweep")
opts <- options(verbose=FALSE)

rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))


test_that("sweep works as expected", {
  checkpoint(repo=rrt_path, snapshotdate="2014-08-01", verbose = FALSE)
  cat("library(stringr)", file=file.path(rrt_path, "code.R"))
  checkpoint(repo=rrt_path)

  expect_true(is_rrt(rrt_path, FALSE))
  expect_false(is_rrt("~/", FALSE))
  expect_equal(
    list.files(rrt_path),
    c("code.R", "manifest.yml", "rrt")
  )
  
  installed <- list.files(rrtPath(rrt_path, "lib"))
  installed_pre <- installed[!installed %in% "src"]
  src_pre <- list.files(rrtPath(rrt_path, "src"))
  expect_equal(installed_pre, c("stringr"))

  
})


test_that("sweep returns messages", {
  expect_message(
    rrt_sweep(repo=rrt_path), 
    "Checking to make sure repository exists"
  )
  expect_message(
    rrt_sweep(repo=rrt_path), 
    "Package sources removed"
  )
  expect_message(
    rrt_sweep(repo=rrt_path),
    "Checking to make sure rrt directory exists inside your repository"
    )
  expect_equal(
    list.files(rrt_path),
    c("code.R", "manifest.yml", "rrt")
  )
  
})


test_that("sweep actually removes installed packages and sources", {
  options(verbose=FALSE)
  
  installed <- list.files(rrtPath(rrt_path, "lib"))
  installed_post <- installed[!installed %in% "src"]
  src_post <- list.files(rrtPath(rrt_path, "src"))
  expect_equal(installed_post, character(0))
  expect_equal(src_post, character(0))
  
})

# cleanup
options(opts)
cleanRRTfolder()
