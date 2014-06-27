---
layout: page
title: Installation
---

**Installation**

* [Linux/OSX](#linux---osx)
* [Windows](#windows)
* [git](#git)

To install R, go to the [CRAN home page](http://cran.r-project.org/)

### Linux - OSX

On Linux, you may need to install xml C library first (on the command line) (if you don't have them already). You may need libcurl too. Do report in the issues tab if you run into this problem. On OSX, I believe xml and curl C libraries are installed by default on your machine.

```
(sudo) apt-get update
(sudo) apt-get install r-cran-xml
```

Get dependency `miniCRAN` that is not on CRAN

In an `R` session

```coffee
install.packages("devtools")
library("devtools")
```

```coffee
install_github("andrie/miniCRAN")
```

Then install `RRT`

```coffee
install_github("RevolutionAnalytics/RRT")
library("RRT")
```

### OSX


asdfadf

### Windows

* Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)
* Install the [devtools R package](https://github.com/hadley/devtools). You can install from source, or just install via `install.packages("devtools")`, or choose devtools from the R GUI.
* Install `miniCRAN`

```coffee
install_github("andrie/miniCRAN")
```

* Then install `RRT`

```coffee
install_github("RevolutionAnalytics/RRT")
library("RRT")
```

### git

_Optionally_, install `git2r` to use git from within R. `git2r` is in Enhances in `RRT`, so you don't need it to install `RRT`

```coffee
install_github("ropensci/git2r")
```
