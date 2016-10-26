# tests for initialize
if(interactive()) library(testthat)

Sys.setenv("R_TESTS" = "") # Configure Travis for tests https://github.com/RevolutionAnalytics/checkpoint/issues/139

current.R <- local({ x = getRversion(); paste(x$major, x$minor, sep=".")})

test.start <- switch(current.R,
                     "3.1" = "2014-10-01",
                     "3.2" = "2015-05-01",
                     "3.3" = "2015-09-01",
                     "2015-09-01"
)

MRAN.default = test.start[1]

packages.to.test.base <- c("MASS", "plyr", "httr", "XML", "checkpoint", "stats", "stats4", "compiler")
packages.to.test.base <- c("MASS", "chron", "checkpoint", "stats", "stats4", "compiler")
packages.to.test.knitr <- c("foreach")
checkpointLocation <- tempdir()
dir.create(file.path(checkpointLocation, ".checkpoint"), showWarnings = FALSE)



#  ------------------------------------------------------------------------


test_checkpoint <- function(https = FALSE, snap.dates){
  
  originalLibPaths <- .libPaths()
  resetLibPaths <- function(old){
    assign(".lib.loc", old, envir = environment(.libPaths))
  }
  on.exit(resetLibPaths(originalLibPaths))
  
  url_prefix <- if(https) "https://" else "http://"
  for(snap_date in snap.dates) {
    # url_prefix <- "http://"
    # snap_date <- MRAN.default ### <<< use only for interactive testing
    
    test_that(paste("checkpoint -", url_prefix, "@", snap_date), {
      if(!interactive()) skip_on_cran()
      describe(paste("checkpoint -", url_prefix, "@", snap_date), {
        
        packages.to.test = if(suppressWarnings(require("knitr", quietly = TRUE)))
          c(packages.to.test.base, packages.to.test.knitr) else 
            packages.to.test.base
        
        project_root <- file.path(tempfile(), "checkpointtemp")
        dir.create(project_root, recursive = TRUE)
        
        
        cleanCheckpointFolder(snap_date, checkpointLocation = checkpointLocation)
        
        it("finds correct MRAN URL", {
          expect_equal(
            getSnapshotUrl(snap_date),
            paste0(url_prefix, "mran.microsoft.com/snapshot/", snap_date))
          
        })
        
        it("prints message if no packages found", {
          expect_message(
            checkpoint(snap_date, checkpointLocation = checkpointLocation, project = project_root),
            "No packages found to install")
          
        })
        
        # Write dummy code file to project
        code = paste("library('", packages.to.test.base, "')", sep ="", collapse ="\n")
        cat(code, file = file.path(project_root, "code.R"))
        
        # Write dummy knitr code file to project
        code = sprintf("```{r}\n%s\n```", 
                       paste("library('", packages.to.test.knitr, "')", sep ="", collapse ="\n"))
        cat(code, file = file.path(project_root, "code.Rmd"))
        
        
        it("prints progress message", {
          expect_message(
            checkpoint(snap_date, checkpointLocation = checkpointLocation, project = project_root),
            "Installing packages used in this project")
        })
        
        it("does not display message whan scanForPackages=FALSE", {
          expect_false(
            isTRUE(
              shows_message("Scanning for packages used in this project",
                checkpoint(snap_date, checkpointLocation = checkpointLocation,
                           project = project_root, scanForPackages=FALSE)
              )
            ))
        })

        it("installs all packages correctly in local lib", {
          pdbMRAN <- available.packages(contriburl = contrib.url(repos = getSnapshotUrl(snap_date)))
          pdbLocal <- installed.packages(fields = "Date/Publication", noCache = TRUE)
          
          pkgNames <- function(pdb)unname(pdb[, "Package"])
          
          base.packages <- pkgNames(utils::installed.packages(priority = "base", 
                                                              lib.loc = .Library,
                                                              noCache = TRUE))
          
          expected.packages <- setdiff(packages.to.test, c("checkpoint", base.packages))
          
          expect_true(
            all(expected.packages %in% pkgNames(pdbLocal))
          )
          
          messageMissingPackages <- function(exp, avail){
            if(!all(exp %in% avail)) {
              msg <- paste(
                "\n",
                paste0("Expected:", paste(exp, collapse = ", ")),
                paste0("Actual  :", paste(avail, collapse = ", ")),
                paste0("Missing :", paste(setdiff(exp, avail), collapse = ", ")),
                "\n",
                sep = "\n")
              cat(msg)
            }
          }
          messageMissingPackages(expected.packages, pkgNames(pdbLocal))
          
          expect_true(
            all(
              na.omit(
                pdbLocal[, "Date/Publication"]) <=
                as.POSIXct(snap_date, tz="UTC"))
          )
          
        })

        it("uses correct MRAN url", {
          expect_equal(
            getOption("repos"),
            paste0(url_prefix, "mran.microsoft.com/snapshot/", snap_date)
          )
        })
        
        it("uses correct library location", {
          expect_equal(
            checkpointPath(snap_date, checkpointLocation, type = "lib"),
            normalizePath(.libPaths()[1], winslash = "/")
          )
        })
      })
    })
    
    # cleanup
    cleanCheckpointFolder(snap_date, checkpointLocation = checkpointLocation)
    resetLibPaths(originalLibPaths)
    expect_identical(originalLibPaths, .libPaths())
  }
}



#  ------------------------------------------------------------------------


MRAN.dates <- getValidSnapshots()
MRAN.sample <- sample(MRAN.dates, 2, replace = FALSE)

initialUrl <- getOption("checkpoint.mranUrl")

if(getRversion() >= "3.2.0" && httpsSupported()){
  context("https")
  
  options(checkpoint.mranUrl = "https://mran.microsoft.com/")
  test_checkpoint(http = TRUE, snap.dates = MRAN.default)
  
  options(checkpoint.mranUrl = NULL)
}

context("http")
options(checkpoint.mranUrl = "http://mran.microsoft.com/")
test_checkpoint(http = FALSE, snap.dates = MRAN.default)
options(checkpoint.mranUrl = initialUrl)




