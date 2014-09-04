\dontrun{
   # dependencies for the repo in the current working dir
   scanRepoPackages()
  
   # dependencies for an repo in another directory
   scanRepoPackages("~/newrepo")
  
   # include only certain file extensions
   scanRepoPackages(fileext=c('Rmd'))
  
   # exclude some file extensions
   scanRepoPackages(fileext=c('-Rmd'))
   scanRepoPackages(fileext=c('-Rmd','Rnw'))
  
   # suppress messages
   scanRepoPackages(verbose=FALSE)
}

