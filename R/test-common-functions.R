# checkpoint test common functions
cleanCheckpointFolder <- function(snapshotDate) {
  folder = checkpointPath(snapshotDate, "snapshotDir")
  unlink(folder, recursive = TRUE, force = TRUE)}
