---
layout: page
title: RRT Installation
---

**RRT Installation**

* [Ubuntu Linux](#Ubuntu Linux)
* [Mac OS X](#Mac OS X)
* [Windows](#windows)
* optional [git integration](#git integration)

To install R, the best place to start is the [CRAN home page](http://cran.r-project.org/)  
Optionally:  
Ubuntu users can  

```coffee
apt-get update ; apt-get install r-base
```  

Mac [Homebrew](http://brew.sh) users can 

```coffee
brew update ; brew tap homebrew/science ; brew install R
```

### Ubuntu Linux

On Ubuntu Linux, you may need to install two system dependencies:  
-XML C library  
-libcurl-dev

```coffee
apt-get install r-cran-xml libcurl-dev
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

RRT has been tested on Ubuntu 14.04.  
Please create a brief item in the [RRT issue tracker](https://github.com/RevolutionAnalytics/RRT/issues) 
if you run into issues with system dependencies or general install issues.

### Mac OS X

XML and curl C libraries should be installed by default on your machine.
RRT has been tested on Mac OS X Mavericks.  
Please create a brief item in the [RRT issue tracker](https://github.com/RevolutionAnalytics/RRT/issues) 
if you run into issues on other versions of Mac OS X and system dependencies.  

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

### Windows

* Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)
* Install the [devtools R package](https://github.com/hadley/devtools). You can install from source, 
or just install via `install.packages("devtools")`, or choose devtools from the R GUI.
* Install `miniCRAN`

```coffee
install_github("andrie/miniCRAN")
```

* Then install `RRT`

```coffee
install_github("RevolutionAnalytics/RRT")
library("RRT")
```

### git integration

_Optionally_, install `git2r` to use git capabilities from within R / RRT. `git2r` is an enhancement in `RRT`,
so you do not need git2r to install `RRT`. RRT users are encouraged to try the git integration features of RRT
to simplify sharing R code repositories with other R users.

```coffee
install_github("ropensci/git2r")
```
