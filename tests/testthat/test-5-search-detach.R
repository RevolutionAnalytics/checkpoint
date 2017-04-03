# tests for detaching packages from search path
if(interactive()) library(testthat)

context("detach packages")
test_that("it detaches selected packages", {
  
  describe("detaches a list of required packages", {
    old_search <- search()
    library(MASS)
    library(splines)
    library(stats4)
    
    to_find <- c("stats4", "splines", "MASS")
    
    it("finds loaded packages", {
      expect_equal(
        findInSearchPath(c(to_find, "foo_not_loaded")),
        to_find
      )
    })
    
    it("detaches these packages", {
      expect_null(detachFromSearchPath(to_find))
      expect_equal(
        findInSearchPath(to_find),
        character(0)
      )
      expect_equal(search(), old_search)
    })
    
  })
})
