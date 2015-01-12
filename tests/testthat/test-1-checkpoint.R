# tests for initialize
context("checkpoint")

Sys.setenv("R_TESTS" = "") # Configure Travis for tests https://github.com/RevolutionAnalytics/checkpoint/issues/139

MRAN.start = as.Date("2014-09-17")
MRAN.default = as.Date("2014-10-01")
MRAN.dates = as.Date(MRAN.start:(Sys.Date()-1), origin = as.Date("1970-01-01"))

packages.to.test = c("MASS", "plyr", "XML", "httr","checkpoint", "stats", "stats4", "compiler")

for(snap_date in as.character(c(MRAN.default, MRAN.dates[sample(length(MRAN.dates), 10, replace = FALSE)]))) {
  project_root <- file.path(tempfile(), "checkpointtemp")
  dir.create(project_root, recursive = TRUE)
  
  test_that(paste("snapshot functions work correctly with snapshot", snap_date), {
    skip_on_cran()
    message("\n", "Snapshot date: ", snap_date)
    
    checkpoint:::cleanCheckpointFolder(snap_date)
    
    expect_equal(
      checkpoint:::getSnapshotUrl(snap_date),
      file.path("http://mran.revolutionanalytics.com/snapshot", snap_date))
    
    expect_message(
      checkpoint(snap_date, project = project_root),
      "No packages found to install")
    
    # Write dummy code file to project
    code = paste("library('", packages.to.test, "')", sep ="", collapse ="\n")
    cat(code, file = file.path(project_root, "code.R"))
    
    expect_message(
      checkpoint(snap_date, project = project_root),
      "Installing packages used in this project")
    
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
      all(sapply(setdiff(packages.to.test, "checkpoint"), require, character.only = TRUE, quietly = TRUE)))

    expect_equal(
      getOption("repos"),
      file.path("http://mran.revolutionanalytics.com/snapshot", snap_date))
    
    expect_equal(
      checkpoint:::checkpointPath(snap_date, "lib"),
      normalizePath(.libPaths()[1]))})
  # cleanup
  checkpoint:::cleanCheckpointFolder(snap_date)
}