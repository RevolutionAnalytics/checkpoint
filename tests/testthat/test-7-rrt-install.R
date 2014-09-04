# tests for initialize
context("install_mran")



rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))



test_that("rrt_install downloads files correctly", {
    
  createRepoFolders(rrt_path)  
  
  instPath <- rrtPath(rrt_path, "src")
  expect_equal(dir(instPath), character(0))
  rrt_install(repo=rrt_path, ...)
  
  
})


# cleanup
cleanRRTfolder()


