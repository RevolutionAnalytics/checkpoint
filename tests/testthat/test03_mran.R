context("MRAN")

skip_on_cran()

mran <- getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
snapshot <- "2020-01-01"
repos <- getOption("repos")

test_that("Snapshot URL works",
{
    expect_identical(snapshot_url(mran), "https://mran.microsoft.com/snapshot")
    expect_identical(snapshot_url("file://localdir"), "file://localdir/snapshot")
    expect_identical(snapshot_url("http://hostname"), "http://hostname/snapshot")

    expect_identical(snapshot_url(mran, snapshot), "https://mran.microsoft.com/snapshot/2020-01-01")
    expect_identical(snapshot_url("file://localdir", snapshot), "file://localdir/snapshot/2020-01-01")
    expect_identical(snapshot_url("http://hostname", snapshot), "http://hostname/snapshot/2020-01-01")
})


test_that("Snapshot list works",
{
    lst <- list_mran_snapshots(mran)
    expect_type(lst, "character")
    expect_true(length(lst) > 1000)
})


test_that("Use MRAN works",
{
    expect_error(use_mran_snapshot("1970-01-01", mran, validate=TRUE))
    expect_error(use_mran_snapshot("2020-01-01", "dummy://host"))

    expect_silent(use_mran_snapshot(snapshot, validate=TRUE))
    expect_identical(getOption("repos")[1], c(CRAN=file.path(mran, "snapshot", snapshot)))
})


teardown({
    options(repos=repos)
})
