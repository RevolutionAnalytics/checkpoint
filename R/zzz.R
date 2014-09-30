.onAttach <- function(...) {
  msg <- paste(
    "",
    "checkpoint: the Reproducible R Toolkit from Revolution Analytics",
    "https://github.com/RevolutionAnalytics/checkpoint/wiki",
    sep="\n")
  packageStartupMessage(msg)
}

