if(interactive()) library("testthat")
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
    
    expect_error(
      checkpoint("2015-06-05"),
      "Snapshot does not exist on MRAN. Try 2015-06-03 or 2015-06-09."
    )
  })
  
  it("works with no network connection", {
    with_mock(`getValidSnapshots` = function(...){x <- character(0); class(x) = "error"; x}, {
      expect_message(stopIfInvalidDate("2015-06-05"),
                     "Unable to connect to MRAN. Skipping some date validations."
      )
      with_mock(`getValidSnapshots` = function(...){x <- character(0); class(x) = "error"; x}, {
        expect_message(stopIfInvalidDate("2015-06-05"),
                       "Unable to connect to MRAN. Skipping some date validations."
        )
      })
    })
  })
  
  it("works on local file", {
    msg <- "Ensure you use the correct http://,  https:// or file:/// prefix."
    
    localMRAN <- system.file("tests/localMRAN", package = "checkpoint")
    expect_error(getValidSnapshots(localMRAN), msg)
    getValidSnapshots(paste0("file:///", localMRAN))
    
  })
  
})
