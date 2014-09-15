context("MRAN snapshots")

test_that("snapshot functions return correct results", {
  
  expect_error(
    getSnapshotUrl(),
    "snapshotDate not supplied"
  )
  
  ss <- getSnapshotUrl("2014-08-18")
  expect_is(ss, "character")
  expect_true(length(ss) == 1)
  
  expect_true(all(sapply(ss, nchar) >= 10))
  
  dd <- "2014-09-08"
  mm <- getSnapshotUrl(dd)
  expect_equal(paste0(mranUrl(), dd), mm)
  
})