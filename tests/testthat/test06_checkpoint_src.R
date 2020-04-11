context("Checkpointing from source")

skip_on_cran()

os <- Sys.info()["sysname"]
if(!(os %in% c("Windows", "Darwin"))) skip("Skipping source checkpointing tests, not on Windows/MacOS")

rver <- "3.6"
snapshot <- "2018-01-01"
checkpoint_loc <- tempfile()

repos <- getOption("repos")
libs <- .libPaths()

if(!dir.exists(checkpoint_loc)) dir.create(checkpoint_loc)

pkgcache::pkg_cache_delete_files()

test_that("Checkpointing from source works",
{
    expect_warning(inst <- create_checkpoint(snapshot, checkpoint_location=checkpoint_loc,
        project_dir="../project_old", r_version=rver, config=list(platforms="source")))

    checkpoint_dir <- checkpoint_dir(snapshot, checkpoint_loc, rver)
    expect_true(dir.exists(checkpoint_dir))
    expect_true("R6" %in% dir(checkpoint_dir))
})


teardown({
    unlink(checkpoint_loc, recursive=TRUE)
    options(repos=repos)
    .libPaths(libs)
})
