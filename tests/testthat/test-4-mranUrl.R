# tests for mranUrl()
if(interactive()) library(testthat)

context("mranURL")
test_that("sets snapshot correctly", {
  describe("sets snapshot correctly", {
    it("sets snapshot correctly", {
      oldRepo <- getOption("repos")
      on.exit(options(repos = oldRepo))
      
      expect_equal(
        setSnapshot(),
        oldRepo
      )
      
      expect_message(
        setSnapshot("2017-04-01"),
        "Using CRAN mirror at"
      )
      
      expect_equal(
        getOption("repos"),
        c(CRAN = "https://mran.microsoft.com/snapshot/2017-04-01")
      )
    })
  })
})