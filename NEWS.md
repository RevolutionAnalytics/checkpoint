# checkpoint 0.4.8

* Added a `use.lock` argument to `checkpoint`. When this is `FALSE`, packages are installed without the use of locks (via the `--no-lock` argument to `R CMD INSTALL`). The default is `TRUE`, which is needed to pass checks on R-devel; set this to `FALSE` to replicate the previous behaviour of `checkpoint`.
* Restored Andrie de Vries and Microsoft to Authors@R in the `DESCRIPTION` file.

# checkpoint 0.4.7

* Fixes for bugs uncovered by R CMD check.
* `unCheckpoint` now automatically restores library paths to their pre-checkpoint state. This fixes the bug where library paths specified by means other than the `R_LIBS_USER` environment variable, eg via code in `.Rprofile`, were not restored. The `new` argument is no longer used, and a warning is printed if it is supplied.
* Skip all tests on CRAN, to avoid issues when MRAN is down.
* Handle an empty local MRAN gracefully (#253).


For package updates in previous releases of checkpoint, please see the releases page at https://github.com/RevolutionAnalytics/checkpoint/releases