# tests for initialize
context("init")



rrtPath <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))



test_that("snapshot functions return correct results", {
  
  expect_is(mran_snaps(), "character")
  
  snaps <- mran_snaps("2014-06-25")
  expect_is(snaps, "character")
  expect_equal(length(snaps), 8L)
  
  set_snapshot_date(rrtPath, snapdate = "2014-08-01", autosnap = TRUE)
  expect_equal(getOption("RRT_snapshotID"), "2014-08-01_0500")
  
  snap <- getSnapshotId("2014-06-25", forceLast=TRUE)
  expect_equal(snap, "2014-06-25_2300")
  
  set_snapshot_date(rrtPath, snapdate = "2014-06-25_0500", autosnap = FALSE)
  
  expect_equal(getOption("RRT_snapshotID"), "2014-06-25_0500")
})
  

test_that("init works as expected", {
  
  
  rrt_init(rrtPath, verbose = FALSE, autosnap = TRUE)
  get_snapshot_date(repository=rrtPath)
  
  rrt_init(rrtPath, snapdate="2014-08-01", verbose = FALSE, autosnap = TRUE)
  expect_true(is_rrt(rrtPath, FALSE))
  expect_false(is_rrt("~/", FALSE))
  expect_true("rrt" %in% list.files(rrtPath))
  
  expect_equal(get_snapshot_date(repository=rrtPath), "2014-08-01_0500")
})

test_that("init returns messages", {
  expect_message(rrt_init(rrtPath), 
                 "Checking to see if repository exists already")
  
  expect_message(rrt_init(rrtPath, snapdate="2014-08-01", autosnap = TRUE), 
                 "Checking to see if repository exists already")
})

# cleanup
cleanRRTfolder()