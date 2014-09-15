context("MRAN snapshots")

test_that("snapshot functions return correct results", {

  expect_error(
    getSnaphostUrl("1972-01-01"))

  dd <- "2014-09-08"
  mm <- getSnapshotUrl(dd)
  expect_equal(paste0(mranUrl(), dd), mm)

})
