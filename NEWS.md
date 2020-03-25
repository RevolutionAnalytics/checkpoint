# checkpoint 0.9.9.9000 (to be 1.0.0)

This is a **major refactoring/rewrite** of checkpoint, aimed at solving many long-standing issues.

- Switch to [pkgdepends](https://github.com/r-lib/pkgdepends) for the backend, replacing custom-written code that calls `install.packages` and the like. This brings the following benefits:
  - Support for parallel installs.
  - Caching of downloaded packages.
  - Allow installing packages which are in use, without having to unload them first.
  - Comprehensive reporting of all aspects of the install process: dependency resolution, creating an install plan, downloading packages, and actual installation.
  - Reliable detection of installation outcomes (no more having to screen-scrape the R terminal!)
- New functions `create_checkpoint` and `use_checkpoint`, reflecting the two main objectives of the package. The `checkpoint` function calls out to these, based on whether the checkpoint directory exists.
  - A side-effect of this is that calling `checkpoint` in your home directory should result in checkpointing very many packages only once.
- New functions `uncheckpoint_session` (which is the reverse of `use_checkpoint`), `delete_checkpoint` and `delete_all_checkpoints` to manage checkpointing. They replace `checkpointRemove`, `checkpointArchives` and `unCheckpoint`.
- Function `getValidSnapshots` renamed to `list_mran_snapshots` to clarify that it lists the snapshots on the MRAN server. Similarly, `setSnapshot` renamed to `use_mran_snapshot` for consistency with other function names.
- Function `scanForPackages` renamed to `scan_project_files` to match grammatical pattern of other function names; now automatically includes rmarkdown (not knitr) in the list of dependencies if Rmarkdown-based files are found.
- Consistent use of snake_case for all object names and function arguments.
- Remove obsolete workarounds for lack of HTTPS support in ancient versions of R. (Note that MRAN now only supports HTTPS access.)
- Other bug fixes and general tidying.

# checkpoint 0.4.9

* Make vignettes static to avoid CRAN check issues.

# checkpoint 0.4.8

* Added a `use.lock` argument to `checkpoint`. When this is `FALSE`, packages are installed without the use of locks (via the `--no-lock` argument to `R CMD INSTALL`). The default is `TRUE`, which is needed to pass checks on R-devel; set this to `FALSE` to replicate the previous behaviour of `checkpoint`.
* Fix build failures with the main checkpoint vignette on R-devel.
* Restored Andrie de Vries and Microsoft to Authors@R in the `DESCRIPTION` file.

# checkpoint 0.4.7

* Fixes for bugs uncovered by R CMD check.
* `unCheckpoint` now automatically restores library paths to their pre-checkpoint state. This fixes the bug where library paths specified by means other than the `R_LIBS_USER` environment variable, eg via code in `.Rprofile`, were not restored. The `new` argument is no longer used, and a warning is printed if it is supplied.
* Skip all tests on CRAN, to avoid issues when MRAN is down.
* Handle an empty local MRAN gracefully (#253).


For package updates in previous releases of checkpoint, please see the releases page at https://github.com/RevolutionAnalytics/checkpoint/releases