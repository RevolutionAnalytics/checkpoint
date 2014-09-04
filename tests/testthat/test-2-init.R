# tests for initialize
context("init")



rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))



test_that("createRepoFolders creates correct folders", {
  createRepoFolders(rrt_path)
  dirs <- dir(rrt_path, recursive=TRUE, include.dirs=TRUE)
  dirs
  expect_equal(grep("rrt", dirs), 1:6)
  expect_equal(grep("rrt/lib", dirs), 2:6)
  expect_equal(grep("src", dirs), 5:6)
  expect_equal(grep("src/contrib", dirs), 6)
  expect_equal(grep(R.version$platform, dirs), 3:6)
  expect_equal(grep(getRversion(), dirs), 4:6)
})




test_that("init works as expected", {
  
  
  rrt_init(rrt_path, verbose = FALSE, autosnap = TRUE)
  getSnapshotFromManifest(repo=rrt_path)
  
  rrt_init(rrt_path, snapdate="2014-08-01", verbose = FALSE, autosnap = TRUE)
  expect_true(is_rrt(rrt_path, FALSE))
  expect_false(is_rrt("~/", FALSE))
  expect_equal(list.files(rrt_path, recursive=TRUE),
               c("manifest.yml", "rrt/rrt_manifest.yml"))
  
  expect_equal(getSnapshotFromManifest(repo=rrt_path), "2014-08-01_0500")
})

test_that("init returns messages", {
  #   expect_message(rrt_init(rrt_path), 
  #                  "Checking to see if repository exists already")
  
  #   expect_message(rrt_init(rrt_path, snapdate="2014-08-01", autosnap = TRUE), 
  #                  "Checking to see if repository exists already")
})



# cleanup
cleanRRTfolder()