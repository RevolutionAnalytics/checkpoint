.onAttach <- function(...) {
  Sys.setenv(MRAN_SERVER = 'http://mran.revolutionanalytics.com/')
  packageStartupMessage("\n\nNew to RRT? Help: https://github.com/RevolutionAnalytics/RRT/wiki & see package vignettes \ncitation(package='RRT') for the citation \n")
}