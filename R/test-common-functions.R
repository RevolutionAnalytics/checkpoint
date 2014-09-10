
# RRT test common functions
cleanRRTfolder <- function(folder = "~/rrttemp") {
  files <- list.files(folder, recursive = TRUE, all.files = TRUE, full.names = TRUE)
#   file.remove(folder)
  unlink(folder, recursive = TRUE, force = TRUE)
}

