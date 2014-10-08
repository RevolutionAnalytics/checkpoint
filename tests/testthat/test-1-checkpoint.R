# tests for initialize
context("checkpoint")

MRAN.start = as.Date("2014-09-17")
MRAN.default = as.Date("2014-10-01")
MRAN.dates = as.Date(MRAN.start:(Sys.Date()-1), origin = as.Date("1970-01-01"))
for(snap_date in as.character(c(MRAN.default, MRAN.dates[sample(length(MRAN.dates), 10, replace = FALSE)]))) {
  project_root <- file.path(tempfile(), "checkpointtemp")
  dir.create(project_root, recursive = TRUE)

  test_that("snapshot functions return correct results", {
    skip_on_cran()
    Sys.setenv("R_TESTS" = "") # Need this line to configure Travis for tests https://github.com/RevolutionAnalytics/checkpoint/issues/139
    
    checkpoint:::cleanCheckpointFolder(snap_date)

    expect_equal(
      checkpoint:::getSnapshotUrl(snap_date),
      file.path("http://mran.revolutionanalytics.com/snapshot", snap_date))

    expect_message(
      checkpoint(snap_date, project = project_root),
      "No packages found to install")

    # Write dummy code file to project
    cat("library(MASS)", "library(plyr)", "library(XML)", "library('httr')",
        sep="\n",
        file = file.path(project_root, "code.R"))

    expect_message(
      checkpoint(snap_date, project = project_root),
      "Installing packages used in this project")

    x <- installed.packages(fields = "Date/Publication")
    expect_equivalent(
      sort(x[, "Package"]),
      sort(c("bitops", "digest", "httr", "jsonlite", "MASS", "plyr", "Rcpp",
             "RCurl", "stringr", "XML")))

    expect_true(
      all(
        na.omit(
          x[, "Date/Publication"]) <=
          as.POSIXct(snap_date, tz="UTC")))

    expect_equal(
      getOption("repos"),
      file.path("http://mran.revolutionanalytics.com/snapshot", snap_date))

    expect_equal(
      checkpoint:::checkpointPath(snap_date, "lib"),
      normalizePath(.libPaths()[1]))})
  # cleanup
  checkpoint:::cleanCheckpointFolder(snap_date)
}
