context("Checkpointing 1")

skip_on_cran()

mran <- getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
snapshot <- "2020-01-01"
snapshot2 <- "2019-03-01"
checkpoint_loc <- tempfile()

repos <- getOption("repos")
libs <- .libPaths()

if(!dir.exists(checkpoint_loc)) dir.create(checkpoint_loc)

pkgcache::pkg_cache_delete_files()

test_that("Creating checkpoint works",
{
    expect_true(dir.exists(checkpoint_loc))
    expect_false(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    expect_error(expect_warning(create_checkpoint(snapshot, r_version="0.0.1",
                                                  checkpoint_location=checkpoint_loc, scan_r_only=TRUE)))

    inst <- create_checkpoint(snapshot, project_dir="../project", checkpoint_location=checkpoint_loc,
                              scan_now=FALSE)
    expect_null(inst)
    expect_true(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    checkpoint_dir <- checkpoint_dir(snapshot, checkpoint_loc, getRversion())
    expect_true(dir.exists(checkpoint_dir))

    inst <- create_checkpoint(snapshot, project_dir="../project", checkpoint_location=checkpoint_loc,
                              scan_now=TRUE, scan_r_only=TRUE)
    expect_is(inst, "pkg_installation_proposal")
    expect_true(length(dir(checkpoint_dir)) > 0)

    pkg_srcs <- list_pkgsrc(checkpoint_dir)
    expect_true(all(grepl(snapshot, pkg_srcs)))
})


test_that("Creating checkpoint with different snapshot works",
{
    checkpoint_dir2 <- checkpoint_dir(snapshot2, checkpoint_loc, getRversion())

    inst <- create_checkpoint(snapshot2, project_dir="../project", checkpoint_location=checkpoint_loc,
                              scan_now=TRUE, scan_r_only=TRUE)
    expect_is(inst, "pkg_installation_proposal")
    expect_true(length(dir(checkpoint_dir2)) > 0)

    pkg_srcs <- list_pkgsrc(checkpoint_dir2)
    expect_true(all(grepl(snapshot2, pkg_srcs)))
})


test_that("Using checkpoint works",
{
    use_checkpoint(snapshot, checkpoint_location=checkpoint_loc)
    expect_identical(getOption("repos")[1], c(CRAN=file.path(mran, "snapshot", snapshot)))
    expect_identical(.libPaths()[1], checkpoint_dir(snapshot, checkpoint_loc, getRversion()))
})


test_that("Uncheckpointing works",
{
    uncheckpoint()
    expect_identical(getOption("repos"), repos)
    expect_identical(.libPaths(), libs)
})


test_that("Updating checkpoint works",
{
    expect_identical(getOption("repos"), repos)
    expect_identical(.libPaths(), libs)

    expect_false(file.exists("../project/script2.R"))
    writeLines("library(R6)", "../project/script2.R")

    inst <- create_checkpoint(snapshot, checkpoint_location=checkpoint_loc, project_dir="../project",
                              scan_now=TRUE, scan_r_only=TRUE)
    dl <- inst$get_downloads()
    expect_true(sum(!is.na(dl$filesize)) == 1)
})


test_that("Deleting checkpoint works",
{
    delete_checkpoint(snapshot, checkpoint_location=checkpoint_loc, confirm=FALSE)
    expect_false(dir.exists(checkpoint_dir(snapshot, checkpoint_loc, getRversion())))
})


teardown({
    file.remove("../project/script2.R")
    unlink(checkpoint_loc, recursive=TRUE)
    options(repos=repos)
    .libPaths(libs)
})
