# checkpoint test common functions
cleanCheckpointFolder <- function(snapshotDate) {
  folder = checkpointPath(snapshotDate, type = "snapshot")
  unlink(folder, recursive = TRUE, force = TRUE)}
