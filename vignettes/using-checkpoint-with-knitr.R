## ---- eval = FALSE-------------------------------------------------------
#  # demo script
#  library(MASS)
#  hist(islands)

## ---- echo=FALSE, comment=""---------------------------------------------
cat("```{r, include=FALSE}", '
# write a manifest to local folder
cat("
library(MASS)
",
file = "manifest.R")
', "```", sep = "")

## ---- echo=FALSE, comment=""---------------------------------------------
cat("```{r, include=FALSE}", '
# Create .checkpoint folder (in tempdir for this example)
td <- tempdir()
dir.create(file.path(td, ".checkpoint"), recursive = TRUE)

# Create the checkpoint
library(checkpoint)
checkpoint("2017-03-28", checkpointLocation = td)
', "```", sep = "")

## ----checkpoint, warning=FALSE-------------------------------------------
# write a manifest to local folder
cat('
library(MASS)
',
file = "manifest.R")

# Create .checkpoint folder (in tempdir for this example)
dir.create(file.path(tempdir(), ".checkpoint"), recursive = TRUE)

# Create the checkpoint
library(checkpoint)
checkpoint("2017-03-28", checkpointLocation = tempdir())


## ---- eval=FALSE---------------------------------------------------------
#  .libPaths()
#  ## [1] ".../Temp/RtmpIVB6bI/.checkpoint/2017-03-28/lib/x86_64-w64-mingw32/3.3.2"
#  ## [2] ".../Temp/RtmpIVB6bI/.checkpoint/R-3.3.2"

## ------------------------------------------------------------------------
installed.packages()[, "Package"]

## ---- warning=FALSE------------------------------------------------------
library(MASS)
hist(islands)

## ---- include=FALSE------------------------------------------------------
# clean up

detach("package:MASS", unload = TRUE)
unlink("manifest.R")
unlink(file.path(tempdir(), ".checkpoint"), recursive = TRUE)

