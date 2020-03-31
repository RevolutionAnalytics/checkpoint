context("Checkpointing with different R version")

skip_on_cran()

mran <- getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
snapshot <- "2018-01-01"
rver <- "3.4.0"
checkpoint_loc <- tempfile()

repos <- getOption("repos")
libs <- .libPaths()

if(!dir.exists(checkpoint_loc)) dir.create(checkpoint_loc)

pkgcache::pkg_cache_delete_files()

test_that("Creating checkpoint works",
{
    expect_true(package_version(rver) != getRversion())
    expect_true(dir.exists(checkpoint_loc))
    expect_false(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    expect_warning(inst <- create_checkpoint(snapshot, project_dir="../project", r_version=rver,
                                             checkpoint_location=checkpoint_loc, scan_r_only=TRUE))
    expect_is(inst, "pkg_installation_proposal")

    checkpoint_dir <- checkpoint_dir(snapshot, checkpoint_loc, rver)
    expect_true(length(dir(checkpoint_dir)) > 0)

    pkg_srcs <- list_pkgsrc(checkpoint_dir)
    expect_true(all(grepl(snapshot, pkg_srcs)))
})


teardown({
    unlink(checkpoint_loc, recursive=TRUE)
    options(repos=repos)
    .libPaths(libs)
})
