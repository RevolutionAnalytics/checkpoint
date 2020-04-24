
# checkpoint - Install Packages from Snapshots on the Checkpoint Server for Reproducibility

[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
![R-CMD-check](https://github.com/RevolutionAnalytics/checkpoint/workflows/R-CMD-check/badge.svg?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/checkpoint)](https://cran.r-project.org/package=checkpoint)

## Overview

The goal of `checkpoint` is to solve the problem of package
reproducibility in R. Specifically, `checkpoint` solve the problems that
occur when you don’t have the correct versions of R packages. Since
packages get updated on CRAN all the time, it can be difficult to
recreate an environment where all your packages are consistent with some
earlier state.

To solve this, `checkpoint` allows you to install packages from a
specific snapshot date. In other words, `checkpoint` makes it possible
to install package versions from a specific date in the past, as if you
had a CRAN time machine.

Version 1.0 of checkpoint is a **major refactoring/rewrite**, aimed at resolving many long-standing issues. You can provide feedback by [opening an issue](https://github.com/RevolutionAnalytics/checkpoint/issues) or by [contacting me](mailto:hongooi@microsoft.com).

### Checkpoint Features

With the `checkpoint` package, you can easily:

  - Write R scripts or projects using package versions from a specific
    point in time;
  - Write R scripts that use older versions of packages, or packages
    that are no longer available on CRAN;
  - Install packages (or package versions) visible only to a specific
    project, without affecting other R projects or R users on the same
    system;
  - Manage multiple projects that use different package versions;
  - Share R scripts with others that will automatically install the
    appropriate package versions;
  - Write and share code R whose results can be reproduced, even if new
    (and possibly incompatible) package versions are released later.


## Using checkpoint

The `checkpoint` package has 3 main functions.

The `create_checkpoint` function

- Creates a checkpoint directory to install packages. This directory is located underneath `~/.checkpoint` by default, but you can change its location.
- Scans your project directory for all packages used. Specifically, it looks in your code for instances of `library()` and `require()` calls, as well as the namespacing operators `::` and `:::`.
- Installs these packages from the MRAN snapshot for a specified date, into your checkpoint directory.

The `use_checkpoint` function

- Sets the CRAN mirror for your R session to point to a MRAN snapshot, i.e. modifies `options(repos)`
- Sets your library search path to point to the folder created by `create_checkpoint`, i.e. modifies `.libPaths()`

This means the remainder of your script will run with the packages from your specified date.

Finally, the `checkpoint` function serves as a unified interface to `create_checkpoint` and `use_checkpoint`. It looks for a pre-existing checkpoint directory, and if not found, creates it with `create_checkpoint`. It then calls `use_checkpoint` to put the checkpoint into use.


## Sharing your scripts and projects for reproducibility

Sharing a script to be reproducible is as easy as placing the following snippet at the top:

```r
library(checkpoint)
checkpoint("2020-01-01")  # replace with desired date
```

Then send this script to your collaborators.  When they run this script on their machine for the first time, `checkpoint` will perform the same steps of scanning for package dependencies, creating the checkpoint directory, installing the necessary packages, and setting your session to use the checkpoint. On subsequent runs, `checkpoint` will find and use the created checkpoint, so the packages don't have to be installed again.

If you have more than one script in your project, you can place the above snippet in every standalone script. Alternatively, you can put it in a script of its own, and run it before running any other script.

### Note on projects

The `checkpoint` package is designed to be used with _projects_, which are directories that contain the R code and output associated with the tasks you're working on. If you use RStudio, you will probably be aware of the concept, but the same applies for many other programming editors and IDEs including Visual Studio Code, Notepad++ and Sublime Text.

When it is run, `create_checkpoint` scans all R files inside a given project to determine what packages your code requires. The default project is the current directory `"."`.

If you do not have an actual project open, this will usually expand to your R user directory (`~/<username>` on Unix/Linux and MacOS, or `C:\Users\<username>\Documents` on Windows). For most people, this means that the function will scan through _all_ the projects they have on their machine, which can lead to checkpointing a very large number of packages. Because of this, you should ensure that you are not in your user directory when you run `checkpoint`. A mitigating factor is that this should happen only once, as long as the checkpoint directory remains intact.

### Checkpointing the R version

For an even _more stringent_ form of reproducibility, you can use the following:

```r
library(checkpoint)
checkpoint("2020-01-01", r_version="3.6.2")  # replace with desired date and R version
```

This requires that anyone running the script must be using the specified version of R. The benefit of this is because changes in R over time can affect reproducibility just like changes in third-party packages, so by restricting the script to only one R version, we remove another possible source of variation. However, R itself is usually very stable, and requiring a specific version can be excessively demanding especially in locked-down IT environments. For this reason, specifying the R version is optional.

### Using knitr and rmarkdown with checkpoint

`checkpoint` will automatically add the `rmarkdown` package as a dependency if it finds any Rmarkdown-based files (those with extension `.Rmd`, `.Rpres` or `.Rhtml`) in your project. This allows you to continue working with such documents after checkpointing.

## Resetting your session

To reset your session to the way it was before checkpointing, call `uncheckpoint()`. Alternatively, you can simply restart R.

## Managing checkpoints

To update an existing checkpoint, for example if you need new packages installed, call `create_checkpoint()` again. Any existing packages will remain untouched.

The functions `delete_checkpoint()` and `delete_all_checkpoints()` allow you to remove checkpoint directories that are no longer required. They check that the checkpoint(s) in question are not actually in use before deleting.

Each time `create_checkpoint()` is run, it saves a series of json files in the main checkpoint directory. These are outputs from the `pkgdepends` package, which `checkpoint` uses to perform the actual package installation, and can help you debug any problems that may occur.

1. `<date>_<time>_refs.json`: Packages to be installed into the checkpoint
2. `<date>_<time>_config.json`: Configuration parameters for the checkpoint
3. `<date>_<time>_resolution.json`: Dependency resolution result
4. `<date>_<time>_solution.json`: Solution to package dependencies
5. `<date>_<time>_downloads.json`: Download result
6. `<date>_<time>_install_plan.json`: Package install plan
7. `<date>_<time>_installs.json`: Final installation result

For more information, see the help for `pkgdepends::pkg_installation_proposal`.


## Installation

`checkpoint` is on CRAN:

``` r
install.packages("checkpoint")
```

The development version of `checkpoint` is on GitHub:

``` r
install.packages("devtools")
devtools::install_github("RevolutionAnalytics/checkpoint")
```

## More information

### Project website

https://github.com/RevolutionAnalytics/checkpoint

### Issues

Post an issue on the Issue tracker at
https://github.com/RevolutionAnalytics/checkpoint/issues

### Checkpoint server

https://github.com/RevolutionAnalytics/checkpoint-server

### Made by

[Microsoft](https://mran.microsoft.com/)

## Code of conduct

This project has adopted the [Microsoft Open Source Code of
Conduct](https://opensource.microsoft.com/codeofconduct/). For more
information see the [Code of Conduct
FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or contact
<opencode@microsoft.com> with any additional questions or comments.
