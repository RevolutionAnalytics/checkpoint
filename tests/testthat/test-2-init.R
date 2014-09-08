# tests for initialize
context("init")



rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder(folder=rrt_path)
dir(rrt_path, recursive = TRUE, all.files = TRUE)
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
  
  createRepoFolders(rrt_path)
  
  rrt_init(rrt_path, verbose = FALSE, autosnap = TRUE)
  dir(rrt_path)
  expect_true(file.exists(rrtPath(rrt_path, "manifest")))
  snapshotId <- getSnapshotFromManifest(repo=rrt_path)
  
  rrt_init(rrt_path, snapshotdate="2014-08-01", verbose = FALSE, autosnap = TRUE)
  expect_true(is_rrt(rrt_path, verbose=FALSE))
  expect_false(is_rrt("~/", verbose=FALSE))
  expect_equal(list.files(rrt_path, recursive=TRUE),
               c("manifest.yml", "rrt/rrt_manifest.yml"))
  
  expect_equal(getSnapshotFromManifest(repo=rrt_path), "2014-08-01_0500")
})

test_that("init returns messages", {
    expect_message(rrt_init(rrt_path), 
                   "Looking for packages used in your repository...")
  
    expect_message(rrt_init(rrt_path, snapshotdate="2014-08-01", autosnap = TRUE), 
                   ">>> RRT initialization completed.")
})


# readLines(file.path(rrt_path, "manifest.yml"))
# readLines(file.path(rrt_path, "rrt/rrt_manifest.yml"))

# cleanup
cleanRRTfolder()