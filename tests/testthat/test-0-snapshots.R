if(interactive()) library(testthat)
context("MRAN snapshots")

test_that("snapshot functions return correct results", {
  skip_on_cran()
  expect_warning(
    getSnapshotUrl("1972-01-01"), 
    "Unable to find snapshot on MRAN at https://mran.revolutionanalytics.com/snapshot/1972-01-01"
  )
  
  dd <- "2014-09-08"
  mm <- getSnapshotUrl(dd)
  expect_equal(paste0(mranUrl(), dd), mm)
  
  url <- mranUrl()
  if(getRversion() >= "3.2.0"){
    expect_equal(url, "https://mran.revolutionanalytics.com/snapshot/")
  } else {
    expect_equal(url, "http://mran.revolutionanalytics.com/snapshot/")
  }

})
