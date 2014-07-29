\dontrun{
   # dependencies for the repo in the current working dir
   rrt_deps()
  
   # dependencies for an repo in another directory
   rrt_deps("~/newrepo")
  
   # include only certain file extensions
   rrt_deps(fileext=c('Rmd'))
  
   # exclude some file extensions
   rrt_deps(fileext=c('-Rmd'))
   rrt_deps(fileext=c('-Rmd','Rnw'))
  
   # suppress messages
   rrt_deps(verbose=FALSE)
}

