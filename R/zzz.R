.onAttach <- function(...) {
  msg <- paste(
    "",
    "checkpoint: Part of the Reproducible R Toolkit from Revolution Analytics",
    "http://projects.revolutionanalytics.com/rrt/",
    sep="\n")
  packageStartupMessage(msg)
}

