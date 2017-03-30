## ---- warning=FALSE------------------------------------------------------
td <- tempdir()
dir.create(file.path(td, ".checkpoint"))

## ------------------------------------------------------------------------
library(checkpoint)
checkpoint("2017-03-28", checkpointLocation = td)

## ------------------------------------------------------------------------
.libPaths()
installed.packages(noCache = TRUE)[, "Package"]

## ------------------------------------------------------------------------
library(MASS)
hist(islands)

## ---- include=FALSE------------------------------------------------------
# clean up

detach("package:MASS", unload = TRUE)
unlink(file.path(td, ".checkpoint"), recursive = TRUE)

