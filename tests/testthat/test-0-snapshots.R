if(interactive()) library(testthat)
context("MRAN snapshots")

describe("Validate snapshotDate argument",{
  it("stops if missing snapshotDate", {
    expect_error(
      checkpoint(), 
      "You have to specify a snapshotDate"
    )
  })
  it("stops if invalid snapshotDate format", {
    expect_error(
      checkpoint("2015/01/01"), 
      "snapshotDate must be a valid date using format YYYY-MM-DD"
    )
    expect_error(
      checkpoint("20150101"), 
      "snapshotDate must be a valid date using format YYYY-MM-DD"
    )
  })
  it("stops if snapshotDate doesn't exist on MRAN", {
    
    expect_error(
      checkpoint("2014-09-16"), 
      "Snapshots are only available after 2014-09-17"
    )
    expect_error(
      checkpoint(Sys.Date() + 1), 
      "snapshotDate can not be in the future!"
    )
  })
})

test_that("set http/https correctly", {
  skip_on_cran()
  describe("set http/https correctly", {
    it("resolves to http/https based on R version number", {
      if(getRversion() >= "3.2.0"  && httpsSupported()){
        expect_warning(
          getSnapshotUrl("1972-01-01"), 
          "Unable to find snapshot on MRAN at https://mran.revolutionanalytics.com/snapshot/1972-01-01"
        )
      } else {
        expect_warning(
          getSnapshotUrl("1972-01-01"), 
          "Unable to find snapshot on MRAN at http://mran.revolutionanalytics.com/snapshot/1972-01-01"
        )
      }
      
      dd <- "2014-09-08"
      mm <- getSnapshotUrl(dd)
      expect_equal(paste0(mranUrl(), dd), mm)
      
      url <- mranUrl()
      if(getRversion() >= "3.2.0"  && httpsSupported()){
        expect_equal(url, "https://mran.revolutionanalytics.com/snapshot/")
      } else {
        expect_equal(url, "http://mran.revolutionanalytics.com/snapshot/")
      }
      
    })
  })
})  



context("is.404")
test_that("is.404", {
  describe("is.404 works with http", {
    it("works on http", {
      expect_true(is.404("http://mran.revolutionanalytics.com/snapshot/1972-01-01"))
      expect_false(is.404("http://mran.revolutionanalytics.com/snapshot"))
      expect_false(is.404("http://mran.revolutionanalytics.com/snapshot/2015-05-01"))
    })
  })
  
  describe("is.404 works with https", {
    
    it("works on https", {
      if(!httpsSupported()) skip("https not supported")
      expect_true(is.404("https://mran.revolutionanalytics.com/snapshot/1972-01-01"))
      expect_false(is.404("https://mran.revolutionanalytics.com/snapshot"))
      expect_false(is.404("https://mran.revolutionanalytics.com/snapshot/2015-05-01"))
      
    })
  })
  
  describe("is.404 gracefully deals with https URLs when https not supported", {
    
    it("works even when https is not supported", {
      with_mock(`checkpoint:::httpsSupported` = function(mran) FALSE, {
        # if(!httpsSupported()) skip("https not supported")
        expect_true(is.404("https://mran.revolutionanalytics.com/snapshot/1972-01-01"))
        expect_true(is.404("https://mran.revolutionanalytics.com/snapshot"))
        expect_true(is.404("https://mran.revolutionanalytics.com/snapshot/2015-05-01"))
      })
      
    })
  })
})
