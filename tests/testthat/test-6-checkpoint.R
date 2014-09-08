# tests for refresh
context("refresh")

# remove rrttemp dir if exists

rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))


test_that("checkpoint in empty folder creates folder and files", {
  
  expect_false("rrt" %in% list.files(rrt_path))
  
  expect_null(
    scanRepoPackages(rrt_path)
  )
  
  
  expect_message(
    checkpoint("2014-08-01", repo=rrt_path),
    ">>> RRT refresh completed."
  )
  
  list.files(rrtPath(rrt_path, "lib"), recursive = FALSE)
  
  expect_equal(
    list.files(rrt_path, recursive = TRUE),
    c("manifest.yml", "rrt/rrt_manifest.yml")
  )
  
  
  snapshotid <- getSnapshotFromManifest(repo = rrt_path)
  expect_equal(snapshotid, "2014-08-01_0500")
  
})


test_that("checkpoint in empty folder creates folder and files", {
  
  cat("library(MASS)", file=file.path(rrt_path, "code.R"))
  
  expect_equal(
    scanRepoPackages(rrt_path),
    "MASS"
  )
  
  expect_message(
    checkpoint("2014-08-01", repo=rrt_path),
    ">>> RRT refresh completed."
  )
  
  
  list.files(rrt_path, recursive = TRUE)
  
  
  expect_equal(list.files(rrt_path, recursive = TRUE), 
               c("code.R", "manifest.yml", "rrt/rrt_manifest.yml"))
  rrt_refresh(repo=rrt_path, autosnap = TRUE, verbose = FALSE, quiet=TRUE)
  rrt_refresh(repo=rrt_path, autosnap = TRUE, verbose = TRUE, quiet=FALSE)
  list.files(rrt_path, recursive = TRUE)
  
  expect_true(is_rrt(rrt_path, FALSE))
  expect_that(is_rrt("~/", FALSE), not(is_true()))
  expect_true("rrt" %in% list.files(rrt_path))
})



test_that("checkpoint in empty folder creates folder and files", {
  
  expect_false("rrt" %in% list.files(rrt_path))
  dir.create(rrt_path)
  dir(rrt_path)
  
  rrt_init(repo=rrt_path, snapshotdate="2014-08-01", verbose = FALSE, autosnap = TRUE)
  list.files(rrt_path, recursive = TRUE)
  cat("library(MASS)", file=file.path(rrt_path, "code.R"))
  
  expect_message(
    checkpoint("2014-08-01", repo=rrt_path),
    ">>> RRT refresh completed."
  )
    
  expect_equal(
    list.files(rrt_path, recursive = TRUE),
    c("code.R", "manifest.yml", "rrt/rrt_manifest.yml")
  )
  
  snapshotid <- getSnapshotFromManifest(repo = rrt_path)
  expect_equal(snapshotid, "2014-08-01_0500")
  
  expect_equal(
    scanRepoPackages(rrt_path),
    "MASS"
  )
  
  rrt_install(rrt_path, snapshotid=snapshotid, verbose = FALSE, quiet=TRUE)
  list.files(rrt_path, recursive = TRUE)
  
  
  expect_equal(list.files(rrt_path, recursive = TRUE), 
               c("code.R", "manifest.yml", "rrt/rrt_manifest.yml"))
  rrt_refresh(repo=rrt_path, autosnap = TRUE, verbose = FALSE, quiet=TRUE)
  rrt_refresh(repo=rrt_path, autosnap = TRUE, verbose = TRUE, quiet=FALSE)
  list.files(rrt_path, recursive = TRUE)
  
  expect_true(is_rrt(rrt_path, FALSE))
  expect_that(is_rrt("~/", FALSE), not(is_true()))
  expect_true("rrt" %in% list.files(rrt_path))
})


# cleanup
cleanRRTfolder()
