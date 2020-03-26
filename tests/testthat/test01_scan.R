context("Scan project files")


test_that("Scanning R works",
{
    expect_is(res <- scan_project_files("../project", scan_r_only=TRUE, scan_rprofile=FALSE), "list")
    expect_identical(res$pkg, c("rlang", "darts"))
})


test_that("Scanning R+Rmd works",
{
    expect_is(res <- scan_project_files("../project", scan_r_only=FALSE, scan_rprofile=FALSE), "list")
    expect_identical(res$pkg, c("magrittr", "rlang", "darts", "rmarkdown"))
})

