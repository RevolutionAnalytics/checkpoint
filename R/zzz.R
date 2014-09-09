.onLoad <- function(...) {
  msg <- paste(
    "",
    "RRT: the Reproducible R Toolkit from Revolution Analytics",
    "https://github.com/RevolutionAnalytics/RRT/wiki", 
    sep="\n")
  packageStartupMessage(msg)
}

