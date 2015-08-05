## ---- eval=FALSE---------------------------------------------------------
#  library(checkpoint)
#  checkpoint("2015-04-26", checkpointLocation = tempdir())

## ---- eval=FALSE---------------------------------------------------------
#  library(checkpoint)
#  checkpoint("2015-04-26", checkpointLocation = tempdir())
#  
#  library(MASS)
#  hist(islands)
#  truehist(islands)

## ----setup, include=FALSE------------------------------------------------

## Create temporary project and set working directory

example_project <- tempdir()

dir.create(example_project, recursive = TRUE)
unlink("~/.checkpoint/2015-04-26", recursive=TRUE)
oldRepos <- getOption("repos")
oldLibPaths <- .libPaths()


## Write dummy code file to project

example_code <- '
library(checkpoint)
checkpoint("2015-04-26", checkpointLocation = tempdir())

library(MASS)
hist(islands)
truehist(islands)
'

cat(example_code, file = file.path(example_project, "checkpoint_example_code.R"))


## ----checkpoint----------------------------------------------------------
## Create a checkpoint by specifying a snapshot date

library(checkpoint)
dir.create(file.path(tempdir(), ".checkpoint"))
checkpoint("2015-04-26", project = example_project, checkpointLocation = tempdir())

## ----inspect-1-----------------------------------------------------------
getOption("repos")

## ----inspect-2-----------------------------------------------------------
normalizePath(.libPaths(), winslash = "/")

## ----inspect-3-----------------------------------------------------------
installed.packages()[, "Package"]

## ----cleanup, include=FALSE----------------------------------------------
## cleanup

unlink(example_project, recursive = TRUE)
options(repos = oldRepos)
.libPaths(oldLibPaths)

