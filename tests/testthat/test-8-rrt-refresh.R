# tests for refresh
context("refresh")

# remove rrttemp dir if exists

rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))


test_that("refresh works as expected", {
  
  expect_false("rrt" %in% list.files(rrt_path))
  
  rrt_init(rrt_path, verbose = FALSE, autosnap = TRUE)
  list.files(rrt_path, recursive = TRUE)
  cat("library(bmc)", file=file.path(rrt_path, "code.R"))
  
  checkpoint("2014-08-01", repo=rrt_path)
  list.files(rrt_path, recursive = TRUE)
  
  rrt_install(rrt_path, verbose = FALSE, quiet=TRUE)
  list.files(rrt_path, recursive = TRUE)
  
  
  expect_equal(list.files(rrt_path, recursive = TRUE), 
               c("code.R", "manifest.yml", "rrt/rrt_manifest.yml"))
  rrt_refresh(rrt_path, autosnap = TRUE, verbose = FALSE, quiet=TRUE)
  rrt_refresh(rrt_path, autosnap = TRUE, verbose = TRUE, quiet=FALSE)
  list.files(rrt_path, recursive = TRUE)
  
  expect_true(is_rrt(rrt_path, FALSE))
  expect_that(is_rrt("~/", FALSE), not(is_true()))
  expect_true("rrt" %in% list.files(rrt_path))
})

test_that("refresh returns messages", {
  expect_message(rrt_refresh(rrt_path), 
                 "Checking to make sure repository exists")
})

# cleanup
cleanRRTfolder()
