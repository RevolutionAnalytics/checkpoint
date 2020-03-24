context("Checkpointing")

skip_on_cran()

mran <- getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
snapshot <- "2020-01-01"
checkpoint_loc <- tempfile()

repos <- getOption("repos")
libs <- .libPaths()

if(!dir.exists(checkpoint_loc)) dir.create(checkpoint_loc)


test_that("Creating checkpoint works",
{
    pkgcache::pkg_cache_delete_files()

    expect_true(dir.exists(checkpoint_loc))
    expect_false(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    inst <- create_checkpoint(snapshot, project_dir="../project", checkpoint_location=checkpoint_loc,
                              scan_now=FALSE)
    expect_null(inst)
    expect_true(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    checkpoint_dir <- checkpoint_dir(snapshot, checkpoint_loc, getRversion())
    expect_true(dir.exists(checkpoint_dir))

    inst <- create_checkpoint(snapshot, project_dir="../project", checkpoint_location=checkpoint_loc,
                              scan_now=TRUE,
                              use_now=FALSE)
    expect_is(inst, "pkg_installation_proposal")
    expect_true(length(dir(checkpoint_dir)) > 0)

    pkg_srcs <- list_pkgsrc(checkpoint_dir)
    expect_true(all(grepl(snapshot, pkg_srcs)))
})


test_that("Using checkpoint works",
{
    use_checkpoint("2020-01-01", checkpoint_location=checkpoint_loc)
    expect_identical(getOption("repos")[1], c(CRAN=file.path(mran, "snapshot", snapshot)))
    expect_identical(.libPaths()[1], checkpoint_dir(snapshot, checkpoint_loc, getRversion()))
})


test_that("Uncheckpointing works",
{
    uncheckpoint_session()
    expect_identical(getOption("repos"), repos)
    expect_identical(.libPaths(), libs)
})


test_that("Deleting checkpoint works",
{
    delete_checkpoint(snapshot, checkpoint_location=checkpoint_loc)
    expect_false(dir.exists(checkpoint_dir(snapshot, checkpoint_loc, getRversion())))
})


teardown({
    unlink(checkpoint_loc, recursive=TRUE)
    options(repos=repos)
    .libPaths(libs)
})
