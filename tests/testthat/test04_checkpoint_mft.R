context("Checkpointing with manifest")

skip_on_cran()

mran <- getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
rver <- "3.6"
snapshot <- "2020-01-01"
checkpoint_loc <- tempfile()

repos <- getOption("repos")
libs <- .libPaths()

if(!dir.exists(checkpoint_loc)) dir.create(checkpoint_loc)

pkgcache::pkg_cache_delete_files()

test_that("Creating checkpoint works",
{
    expect_false(package_version(rver) == getRversion())
    expect_true(dir.exists(checkpoint_loc))
    expect_false(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    expect_warning(inst <- create_checkpoint(snapshot, project_dir="../project_mft", checkpoint_location=checkpoint_loc,
        r_version=rver, scan_now=TRUE, scan_r_only=TRUE))
    expect_is(inst, "pkg_installation_proposal")
    expect_true(dir.exists(file.path(checkpoint_loc, ".checkpoint")))

    checkpoint_dir <- checkpoint_dir(snapshot, checkpoint_loc, rver)

    pkg_srcs <- list_pkgsrc(checkpoint_dir)
    pkg_refs <- list_pkgref(checkpoint_dir)
    is_snapshot <- grepl(snapshot, pkg_srcs)
    is_gh <- grepl("^RemotePkgRef: github::", pkg_refs)
    expect_true(all(is_snapshot | is_gh))
})


teardown({
    unlink(checkpoint_loc, recursive=TRUE)
    options(repos=repos)
    .libPaths(libs)
})
