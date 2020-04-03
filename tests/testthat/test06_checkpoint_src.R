context("Checkpointing from source")

skip_on_cran()

os <- Sys.info()["sysname"]
if(!(os %in% c("Windows", "Darwin"))) skip("Skipping source checkpointing tests, not on Windows/MacOS")

mran <- getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
snapshot <- "2018-01-01"
checkpoint_loc <- tempfile()

repos <- getOption("repos")
libs <- .libPaths()

if(!dir.exists(checkpoint_loc)) dir.create(checkpoint_loc)

pkgcache::pkg_cache_delete_files()

test_that("Checkpointing from source works",
{
    #expect_error(create_checkpoint(snapshot, checkpoint_location=checkpoint_loc, project_dir="../project_old"))

    inst <- create_checkpoint(snapshot, checkpoint_location=checkpoint_loc, project_dir="../project_old",
                              config=list(platforms="source"))
    expect_is(inst, "pkg_installation_proposal")
    checkpoint_dir <- checkpoint_dir(snapshot, checkpoint_loc, getRversion())
    expect_true(dir.exists(checkpoint_dir))
})


teardown({
    unlink(checkpoint_loc, recursive=TRUE)
    options(repos=repos)
    .libPaths(libs)
})
