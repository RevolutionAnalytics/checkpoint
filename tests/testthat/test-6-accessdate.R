# tests for detaching packages from search path
if(interactive()) library(testthat)

context("detach packages")
test_that("it detaches selected packages", {
  td <- make_fake_archive()
  list.files(td, all.files = TRUE, recursive = TRUE)
  x <- list.dirs(file.path(td, ".checkpoint"), full.names = FALSE, recursive = FALSE)
  
  exp <- c(
    "2099-03-01",
    "2099-01-02",
    "2099-04-01",
    "2099-02-01",
    "2099-06-01",
    "2099-02-01"
  )
  
  describe("finds archive dates", {
    it("finds last access date", {
      expect_equal(
        basename(getAccessDate(td)),
        exp
      )
    })

  })
})
