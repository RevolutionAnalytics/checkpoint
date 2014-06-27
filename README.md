RRT
===

## Reproducibile R Toolkit

master: [![](https://api.travis-ci.org/RevolutionAnalytics/RRT.png?branch=master)](https://travis-ci.org/RevolutionAnalytics/RRT)
dev: [![](https://api.travis-ci.org/RevolutionAnalytics/RRT.png?branch=dev)](https://travis-ci.org/RevolutionAnalytics/RRT)


### Installation

On Linux, get xml C library first (on the command line)

```
(sudo) apt-get update
(sudo) apt-get install r-cran-xml
```

You may need libcurl too. Do report in the issues tab if you run into this problem.

Get dependency `miniCRAN` that is not on CRAN

In an `R` session

```coffee
install.packages("devtools")
library("devtools")
```

```coffee
devtools::install_github("andrie/miniCRAN")
```

<!-- _Optionally_, install `git2r` to use git from within R. `git2r` is in Enhances in `RRT`, so you don't need it to install `RRT`

```coffee
devtools::install_github("ropensci/git2r")
``` -->

Then install `RRT`

```coffee
devtools::install_github("RevolutionAnalytics/RRT")
library("RRT")
```

### Examples

See the [RRT wiki](https://github.com/RevolutionAnalytics/RRT/wiki/Examples) for examples.

### Bugs or feature requests

Post an issue on the [Issue tracker](https://github.com/RevolutionAnalytics/RRT/issues)

### Made by

[Revolution Analytics](http://www.revolutionanalytics.com/)
