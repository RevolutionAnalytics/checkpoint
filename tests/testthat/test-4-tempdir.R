if(interactive()) library(testthat)

context("tempdir")

# CRAN policy does not allow any folders to be written anywhere other than the session temporary folder tempdir().
# Ensure that no .checkpoint files are written one level higher in the tempdir() tree

test_that("checkpointLocation uses session temp folder", {
  expect_char0 <- function(object){
    expect_is(object, "character")
    expect_length(object, 0)
  }
  expect_char0(
    dir(dirname(tempdir()), all.files = TRUE, pattern = ".checkpoint")
  )
})
