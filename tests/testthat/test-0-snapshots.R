if(interactive()) library(testthat)
context("MRAN snapshots")

test_that("checkpoint stops if invalid snapshotDate", {
  expect_error(
    checkpoint(), 
    "You have to specify a snapshotDate"
  )
  expect_error(
    checkpoint("2015/01/01"), 
    "snapshotDate must be a valid date using format YYYY-MM-DD"
  )
  expect_error(
    checkpoint("20150101"), 
    "snapshotDate must be a valid date using format YYYY-MM-DD"
  )
  expect_error(
    checkpoint("2014-09-16"), 
    "Snapshots are only available after 2014-09-17"
  )
  expect_error(
    checkpoint(Sys.Date() + 1), 
    "snapshotDate can not be in the future!"
  )
})


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
