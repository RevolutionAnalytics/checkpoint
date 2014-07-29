.onAttach <- function(...) {
  Sys.setenv(MRAN_SERVER = 'http://mran.revolutionanalytics.com')
  msg <- paste(
    "",
    "New to RRT?",
    "Help: https://github.com/RevolutionAnalytics/RRT/wiki", 
    "citation(package='RRT') for the citation",
    sep="\n")
  packageStartupMessage(msg)
}

