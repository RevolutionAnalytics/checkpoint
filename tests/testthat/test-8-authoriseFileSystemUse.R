# tests for checking file system authorization
if(interactive()) library(testthat)

context("file system authorization")
test_that("file system use is properly authorized", {
  
  describe("stops if not authorized", {
    td <- file.path(tempdir(), "checkpoint_not_auth")
    unlink(td, recursive = TRUE)
    
    it("stops without authorization in interactive mode", {
      with_mock(
        `base::readline` = function(prompt)"n",
        expect_error(
          authorizeFileSystemUse(td, interactive=TRUE),
          "Cannot proceed without access to checkpoint directory"
        )
      )
    })
    
    it("continues with authorization in interactive mode", {
      with_mock(
        `base::readline` = function(prompt)"y",
        expect_equal(
          authorizeFileSystemUse(td, interactive=TRUE),
          td
        )
      )
    })

    it("stops if folder doesn't exist in batch mode", {
        expect_error(
          authorizeFileSystemUse(td, interactive=FALSE),
          "The .checkpoint folder does not exist. Please try again after creating the folder at"
      )
    })
    
  })
})

