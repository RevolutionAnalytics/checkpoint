# RRT test common functions
cleanRRTfolder <- function(snapshotDate) {
  folder = rrtPath(snapshotDate, "snapshotDir")
  unlink(folder, recursive = TRUE, force = TRUE)}
