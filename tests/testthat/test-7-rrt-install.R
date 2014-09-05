# tests for initialize
context("rrt_install")

#
#
# These tests need substantial improvement 
#
#


rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))



test_that("rrt_install downloads files correctly", {
    
  createRepoFolders(rrt_path)  
  
  instPath <- rrtPath(rrt_path, "src")
  expect_equal(dir(instPath), character(0))
  expect_message(x <- rrt_install(repo=rrt_path, snapshot = "2014-08-01"),
                 "... nothing to install")
  expect_null(x)
  x
  
})


# cleanup
cleanRRTfolder()


# f <- function(){
#   message("Hello")
#   message("World")
# } 
# 
# f()
# expect_message(f(), "Hello")
# expect_message(f(), "World")
# 
# expect_message(f(), "Hello", "World")
# expect_message(f(), c("Hello", "World"))
# 
