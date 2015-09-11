library("testthat")
context("valid snapshot dates")

describe("getValidSnapshots finds valid dates", {
  
  it("returns a list of dates", {
    d <- getValidSnapshots()
    expect_is(d, "character")
    expect_is(as.Date(d), "Date")
  })

  
  it("suggests a reasonable alternative", {
    # On MRAN, 2015-06-04 to 2015-06-08 are missing
    expect_error(
      stopIfInvalidDate("2015-06-05"),
      "Snapshot does not exist on MRAN. Try 2015-06-03 or 2015-06-09."
      )
  })
  

})
