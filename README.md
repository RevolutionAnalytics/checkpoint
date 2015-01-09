# checkpoint - Install packages from snapshots on the checkpoint server for reproducibility

## Overview

The goal of `checkpoint` is to solve the problem of package reproducibility in R. Specifically, `checkpoint` solve the problems that occur when you don't have the correct versions of R packages.  Since packages get updated on CRAN all the time, it can be difficult to recreate an environment where all your packages are consistent with some earlier state.

To solve this, `checkpoint` allows you to install package from a specific snapshot date.  In other words, `checkpoint` makes it possible to install packages from a specific date in the past, as if you had a CRAN time machine.

To achieve reproducibility, once a day we create a complete snapshot of CRAN, on the "Managed R archived network" (MRAN) server.  At midnight (UTC) MRAN mirrors all of CRAN and saves a snapshot.  This allows you to install packages from a snapshot date, thus "going back in time" to this date, by installing packages as they were at that snapshot date.


Together, the `checkpoint` package and the MRAN server act as a CRAN time machine. The `checkpoint()` function installs the packages to a local library exactly as they were at the specified point in time. Only those packages are available to your session, thereby avoiding any package updates that came later and may have altered your results. In this way, anyone using `checkpoint()` can ensure the reproducibility of your scripts or projects at any time.



Using `checkpoint` is simple:

- `checkpoint` has only a single function, `checkpoint()` where you specify the snapshot date.

## Using the checkpoint function

When you create a checkpoint, the `checkpoint()` function performs the following:

- Creates a snapshot folder to install packages. This library folder is located at `~/.checkpoint`
- Scans your project folder for all packages used. Specifically, it searches for all instances of `library()` and `requires()` in your code.
- Installs these packages from the MRAN snapshot into your snapshot folder using `install.packages()`
- Sets options for your CRAN mirror to point to a MRAN snapshot, i.e. modify `options(repos)`

This means the remainder of your script will run with the packages from a specific date.


## Sharing your scripts for reproducibility

Sharing your script to be reproducible is as easy as:

- Load the `checkpoint` package using `library(checkpoint)`
- Ensure you specify `checkpoint()` with your checkpoint date, e.g. `checkpoint("2014-09-17")`

Then send this script to your collaborators.  When they run this script on their machine, `checkpoint` will perform the same steps of installing the necessary packages, creating the `checkpoint` snapshot folder and producing the same results.


## Resetting the checkpoint

To reset the checkpoint, simply restart your R session.



## Worked example

```

# Create temporary project and set working directory

example_project <- paste0("~/checkpoint_example_project_", Sys.Date())

dir.create(example_project, recursive = TRUE)
oldwd <- setwd(example_project)


# Write dummy code file to project

cat("library(MASS)", "library(foreach)",
    sep="\n", 
    file="checkpoint_example_code.R")


# Create a checkpoint by specifying a snapshot date

library(checkpoint)
checkpoint("2014-09-17")

# Check that CRAN mirror is set to MRAN snapshot
getOption("repos")

# Check that library path is set to ~/.checkpoint
.libPaths()

# Check which packages are installed in checkpoint library
installed.packages()

# cleanup
unlink(example_project, recursive = TRUE)
setwd(oldwd)
```



## Installation

To install `checkpoint` directly from CRAN, use:

```
install.packages("checkpoint")
library("checkpoint")
```

To install `checkpoint` directly from github, use the `devtools` package.  In your R session, try:

```
install.packages("devtools")
devtools::install_github("RevolutionAnalytics/checkpoint", ref="v0.3.3")
library("checkpoint")
```



## More information

### Issues

Post an issue on the Issue tracker at https://github.com/RevolutionAnalytics/checkpoint/issues


### Project website

http://projects.revolutionanalytics.com/rrt/

### Checkpoint server

https://github.com/RevolutionAnalytics/checkpoint-server

### Made by

[Revolution Analytics](http://www.revolutionanalytics.com/)
