context("Checkpointing 2")

skip_on_cran()

mran <- getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
snapshot <- "2020-01-01"
snapshot2 <- "2019-01-01"
checkpoint_loc <- tempfile()

repos <- getOption("repos")
libs <- .libPaths()

if(!dir.exists(checkpoint_loc)) dir.create(checkpoint_loc)

pkgcache::pkg_cache_delete_files()

test_that("Checkpoint umbrella function works for create",
{
    expect_true(dir.exists(checkpoint_loc))
    expect_false(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    expect_error(checkpoint(snapshot, r_version="0.0.1"))

    inst <- checkpoint(snapshot, project_dir="../project", checkpoint_location=checkpoint_loc, scan_r_only=TRUE)
    expect_is(inst, "pkg_installation_proposal")
    expect_true(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    checkpoint_dir <- checkpoint_dir(snapshot, checkpoint_loc, getRversion())
    expect_true(dir.exists(checkpoint_dir))
    expect_true(length(dir(checkpoint_dir)) > 0)

    pkg_srcs <- list_pkgsrc(checkpoint_dir)
    expect_true(all(grepl(snapshot, pkg_srcs)))

    expect_identical(getOption("repos")[1], c(CRAN=file.path(mran, "snapshot", snapshot)))
    expect_identical(.libPaths()[1], checkpoint_dir(snapshot, checkpoint_loc, getRversion()))
})


test_that("Checkpoint umbrella function works for use",
{
    uncheckpoint()
    expect_identical(getOption("repos"), repos)
    expect_identical(.libPaths(), libs)

    checkpoint(snapshot, checkpoint_location=checkpoint_loc)
    expect_identical(getOption("repos")[1], c(CRAN=file.path(mran, "snapshot", snapshot)))
    expect_identical(.libPaths()[1], checkpoint_dir(snapshot, checkpoint_loc, getRversion()))
})


test_that("Deleting checkpoint works",
{
    uncheckpoint()
    expect_identical(getOption("repos"), repos)
    expect_identical(.libPaths(), libs)

    delete_checkpoint(snapshot, checkpoint_location=checkpoint_loc)
    expect_false(dir.exists(checkpoint_dir(snapshot, checkpoint_loc, getRversion())))
})


teardown({
    unlink(checkpoint_loc, recursive=TRUE)
    options(repos=repos)
    .libPaths(libs)
})
