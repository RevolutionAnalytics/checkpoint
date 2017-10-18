## ----setup, include=FALSE, fig.asp=4/3-----------------------------------

## Create temporary project and set working directory


example_project <- tempdir()

dir.create(example_project, recursive = TRUE, showWarnings = FALSE)
oldRepos <- getOption("repos")
oldLibPaths <- .libPaths()


## Write dummy code file to project

example_code <- '
library(checkpoint)
checkpoint("2015-04-26", checkpointLocation = tempdir())

# Example from ?darts
library(darts)
x = c(12,16,19,3,17,1,25,19,17,50,18,1,3,17,2,2,13,18,16,2,25,5,5,
      1,5,4,17,25,25,50,3,7,17,17,3,3,3,7,11,10,25,1,19,15,4,1,5,12,17,16,
      50,20,20,20,25,50,2,17,3,20,20,20,5,1,18,15,2,3,25,12,9,3,3,19,16,20,
      5,5,1,4,15,16,5,20,16,2,25,6,12,25,11,25,7,2,5,19,17,17,2,12)
mod = simpleEM(x, niter=100)
e = simpleExpScores(mod$s.final)
oldpar <- par(mfrow=c(1, 2))
drawHeatmap(e)
drawBoard(new=TRUE)
drawAimSpot(e, cex = 5)
par(oldpar)
'

cat(example_code, file = file.path(example_project, "checkpoint_example_code.R"))


## ----checkpoint, warning=FALSE-------------------------------------------
## Create a folder to contain the checkpoint
## This is optional - the default is to use ~/.checkpoint

dir.create(file.path(tempdir(), ".checkpoint"), recursive = TRUE, showWarnings = FALSE)

## Create a checkpoint by specifying a snapshot date

library(checkpoint)
checkpoint("2017-04-01", project = example_project, 
           checkpointLocation = tempdir())

## ----inspect-1-----------------------------------------------------------
getOption("repos")

## ----inspect-2-----------------------------------------------------------
normalizePath(.libPaths(), winslash = "/")

## ----inspect-3, eval=FALSE-----------------------------------------------
#  installed.packages(.libPaths()[1])[, "Package"]

## ----cleanup, include=TRUE-----------------------------------------------
## cleanup

unlink(example_project, recursive = TRUE)
unlink(file.path(tempdir(), "checkpoint_example_code.R"))
unlink(file.path(tempdir(), ".checkpoint"), recursive = TRUE)
options(repos = oldRepos)
unCheckpoint(oldLibPaths)
.libPaths()

