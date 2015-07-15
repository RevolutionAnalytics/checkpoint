# tests for initialize
if(interactive()) library(testthat)
context("checkpoint")

Sys.setenv("R_TESTS" = "") # Configure Travis for tests https://github.com/RevolutionAnalytics/checkpoint/issues/139

# MRAN.start = as.Date("2014-09-17")

current.R <- local({ x = getRversion(); paste(x$major, x$minor, sep=".")})

test.start <- switch(current.R,
       "3.1" =  as.Date("2014-10-01"),
       "3.2" =  as.Date("2015-04-20"),
       as.Date("2015-04-20")
       )

MRAN.default = test.start[1]


MRAN.dates = as.Date(test.start:(Sys.Date()-1), origin = as.Date("1970-01-01"))

packages.to.test.base = c("MASS", "plyr", "httr", "XML", "checkpoint", "stats", "stats4", "compiler")
packages.to.test.knitr = c("foreach")
checkpointLocation = dirname(tempdir())
dir.create(file.path(checkpointLocation, ".checkpoint"), showWarnings = FALSE)

MRAN.length <- length(MRAN.dates)
MRAN.sample <- MRAN.dates[sample(MRAN.length, min(2, MRAN.length), replace = FALSE)]
for(snap_date in unique(as.character(c(MRAN.default, MRAN.sample)))) {
  # snap_date <- as.character(c(MRAN.default, MRAN.sample))[1] ### <<< use only for interactive testing

  packages.to.test = if(require("knitr")) c(packages.to.test.base, packages.to.test.knitr) else packages.to.test.base

  project_root <- file.path(tempfile(), "checkpointtemp")
  dir.create(project_root, recursive = TRUE)

  test_that(paste("snapshot functions work correctly with snapshot", snap_date), {
    skip_on_cran()
    message("\n", "Snapshot date: ", snap_date)

    checkpoint:::cleanCheckpointFolder(snap_date, checkpointLocation = checkpointLocation)

    expect_equal(
      checkpoint:::getSnapshotUrl(snap_date),
      paste0("https://mran.revolutionanalytics.com/snapshot/", snap_date))

    expect_message(
      checkpoint(snap_date, checkpointLocation = checkpointLocation, project = project_root),
      "No packages found to install")

    # Write dummy code file to project
    code = paste("library('", packages.to.test.base, "')", sep ="", collapse ="\n")
    cat(code, file = file.path(project_root, "code.R"))

    # Write dummy knitr code file to project
    code = sprintf("```{r}\n%s\n```", paste("library('", packages.to.test.knitr, "')", sep ="", collapse ="\n"))
    cat(code, file = file.path(project_root, "code.Rmd"))

    expect_message(
      checkpoint(snap_date, checkpointLocation = checkpointLocation, project = project_root),
      "Installing packages used in this project")

    # Does not display message whan scanForPackages=FALSE
    expect_false(
      isTRUE(
        shows_message("Scanning for packages used in this project")(
          checkpoint(snap_date, checkpointLocation = checkpointLocation, project = project_root, scanForPackages=FALSE))
      ))

    x <- installed.packages(fields = "Date/Publication", noCache = TRUE)

    base.packages <- unname(installed.packages(priority = "base", lib.loc = .Library)[, "Package"])
    packages.expected <- sort(unique(unlist(

      sapply(setdiff(packages.to.test, c("checkpoint", base.packages)), function(p){
        z <- tools::pkgDepends(p)
        c(z$Depends, z$Imports)
      }, USE.NAMES = FALSE)
    )))


    expect_true(
      all(setdiff(packages.to.test, c("checkpoint", base.packages)) %in% unname(x[, "Package"])))

    expect_true(
      all(setdiff(packages.expected, c("checkpoint", base.packages)) %in% unname(x[, "Package"])))

    expect_true(
      all(
        na.omit(
          x[, "Date/Publication"]) <=
          as.POSIXct(snap_date, tz="UTC")))

    expect_true(
      all(
        sapply(setdiff(packages.to.test, "checkpoint"), function(x){
          if(!base::requireNamespace(x, quietly = TRUE)) {
            message(paste("Unable to load package:", x))
            FALSE
          } else TRUE
        })
      ))

    expect_equal(
      getOption("repos"),
      paste0("https://mran.revolutionanalytics.com/snapshot/", snap_date))

    expect_equal(
      checkpoint:::checkpointPath(snap_date, checkpointLocation, type = "lib"),
      normalizePath(.libPaths()[1], winslash = "/"))

  })
  # cleanup
  checkpoint:::cleanCheckpointFolder(snap_date, checkpointLocation = checkpointLocation)
}
