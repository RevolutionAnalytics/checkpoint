## ----setup-1, include=FALSE----------------------------------------------

## Write dummy code file to project
example_code <- '
library(darts)
'
dir.create(tempdir(), recursive = TRUE)
cat(example_code, file = file.path(tempdir(), "managing_checkpoint_example_code.R"))

## ----checkpoint, results="hide", message=FALSE, warning=FALSE------------
dir.create(file.path(tempdir(), ".checkpoint"), recursive = TRUE, showWarnings = FALSE)
options(install.packages.compile.from.source = "no")
oldLibPaths <- .libPaths()

## Create a checkpoint by specifying a snapshot date
library(checkpoint)
checkpoint("2015-04-26", project = tempdir(), checkpointLocation = tempdir())


## ----archives-1----------------------------------------------------------
# List checkpoint archives on disk.
checkpointArchives(tempdir())

## ----archives-2----------------------------------------------------------
checkpointArchives(tempdir(), full.names = TRUE)

## ----access--------------------------------------------------------------
# Returns the date the snapshot was last accessed.
getAccessDate(tempdir())


## ----remove-1, eval=FALSE------------------------------------------------
#  # Remove singe checkpoint archive from disk.
#  checkpointRemove("2015-04-26")

## ----remove-2, eval=FALSE------------------------------------------------
#  # Remove range of checkpoint archives from disk.
#  checkpointRemove("2015-04-26", allSinceSnapshot = TRUE)
#  checkpointRemove("2015-04-26", allUntilSnapshot =  = TRUE)
#  

## ----remove-3, eval=FALSE------------------------------------------------
#  # Remove snapshot archives that have not been used recently
#  checkpointRemove("2015-04-26", notUsedSince = TRUE)
#  

## ----logfile-1-----------------------------------------------------------
dir(file.path(tempdir(), ".checkpoint"))

## ----logfile-2-----------------------------------------------------------

log_file <- file.path(tempdir(), ".checkpoint", "checkpoint_log.csv")
log <- read.csv(log_file)
head(log)

## ----uncheckpoint-1------------------------------------------------------
.libPaths()

## ----uncheckpoint-2------------------------------------------------------
# Note this is still experimental
unCheckpoint(oldLibPaths)
.libPaths()

## ----cleanup, include=TRUE-----------------------------------------------
## cleanup
unlink("manifest.R")
unlink(file.path(tempdir(), "managing_checkpoint_example_code.R"))
unlink(file.path(tempdir(), ".checkpoint"), recursive = TRUE)

