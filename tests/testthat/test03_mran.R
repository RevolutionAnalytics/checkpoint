context("MRAN")

skip_on_cran()

mran <- getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
snapshot <- "2020-01-01"
repos <- getOption("repos")

test_that("Snapshot list works",
{
    lst <- list_mran_snapshots(mran)
    expect_type(lst, "character")
    expect_true(length(lst) > 1000)
})


test_that("Use MRAN works",
{
    expect_error(use_mran_snapshot("1970-01-01", mran, validate=TRUE))

    expect_silent(use_mran_snapshot(snapshot, validate=TRUE))
    expect_identical(getOption("repos")[1], c(CRAN=file.path(mran, "snapshot", snapshot)))
})


teardown({
    options(repos=repos)
})
