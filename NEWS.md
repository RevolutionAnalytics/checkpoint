# checkpoint 0.4.7

* Fixes for bugs uncovered by R CMD check.
* `unCheckpoint` now automatically restores library paths to their pre-checkpoint state. This fixes the bug where library paths specified by means other than the `R_LIBS_USER` environment variable, eg via code in `.Rprofile`, were not restored. The `new` argument is no longer used, and a warning is printed if it is supplied.
* Skip all tests on CRAN, to avoid issues when MRAN is down.


For package updates in previous releases of checkpoint, please see the releases page at https://github.com/RevolutionAnalytics/checkpoint/releases