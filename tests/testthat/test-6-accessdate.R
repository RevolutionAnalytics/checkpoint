# tests for detaching packages from search path
if(interactive()) library(testthat)

context("detach packages")
test_that("it detaches selected packages", {
  td <- make_fake_archive()
  list.files(td, all.files = TRUE, recursive = TRUE)
  x <- list.dirs(file.path(td, ".checkpoint"), full.names = FALSE, recursive = FALSE)
  
  
  describe("finds archive dates", {
    exp <- c(
      "2099-03-01",
      "2099-01-02",
      "2099-04-01",
      "2099-02-01",
      "2099-06-01",
      "2099-02-01"
    )
    it("finds last access date", {
      expect_equal(
        basename(getAccessDate(td)),
        exp
      )
    })
    
    
    describe("finds archive dates", {
      exp <- c(
        "2099-01-01",
        "2099-01-02",
        "2099-01-03",
        "2099-01-04",
        "2099-01-05",
        "2099-01-06"
      )
      it("finds last access date", {
        expect_equal(
          checkpointArchives(td),
          exp
        )
      })
    })
    
  })
  
  describe("deletes archives", {
    exp <- c(
      "2099-01-01",
      "2099-01-02",
      "2099-01-03",
      "2099-01-04",
      "2099-01-05",
      "2099-01-06"
    )
    it("finds deletes individual archive", {
      expect_message(
        z <- checkpointRemove("2099-01-03", td),
        "successfully removed archive"
      )
      expect_message(
        z <- checkpointRemove("2099-01-03", td),
        "no archives removed"
      )
    })
    
    it("deletes range of archives", {
      expect_message(
        z <- checkpointRemove("2099-01-03", td, allUntilSnapshot = TRUE),
        "successfully removed archive"
      )
      expect_equal(basename(z), exp[1:2])
      expect_message(
        z <- checkpointRemove("2099-01-03", td, allSinceSnapshot = TRUE),
        "successfully removed archive"
      )
      expect_equal(basename(z), exp[4:6])
    })
  })
  
  describe("deletes archives not used since a given access date", {
    td <- make_fake_archive()
    list.files(td, all.files = TRUE, recursive = TRUE)
    exp <- c(
      "2099-01-01",
      "2099-01-02",
      "2099-01-03",
      "2099-01-04",
      "2099-01-05",
      "2099-01-06"
    )
    it("deletes archives not used since", {
      expect_message(
        z <- checkpointRemove("2099-05-01", td, notUsedSince = TRUE),
        "successfully removed archive"
      )
      expect_equal(basename(z), exp[c(1:4, 6)])
    })
    
  })
  
  
})





