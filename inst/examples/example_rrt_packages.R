\dontrun{
   # dependencies for the repo in the current working dir
   rrt_packages()
  
   # dependencies for an repo in another directory
   rrt_packages("~/newrepo")
  
   # include only certain file extensions
   rrt_packages(fileext=c('Rmd'))
  
   # exclude some file extensions
   rrt_packages(fileext=c('-Rmd'))
   rrt_packages(fileext=c('-Rmd','Rnw'))
  
   # suppress messages
   rrt_packages(verbose=FALSE)
}

