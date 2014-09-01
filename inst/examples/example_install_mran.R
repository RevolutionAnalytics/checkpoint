\dontrun{
  libPath <- tempdir()
  instPath <- file.path(libPath, "src/contrib")
  if(file.exists(instPath)) unlink(instPath, recursive = TRUE)
  if(!file.exists(instPath)) dir.create(instPath)
  dir(instPath)
  
  install_mran(pkg="plyr", date="2014-08-23", lib=instPath)
  
  dir(instPath)
  unlink(instPath, recursive = TRUE)
}



getSnapshotId(date='2014-08-04')
download_pkgs_mran(date='2014-08-04', pkgs=c("plyr","ggplot2"), lib=libPath)
dir(instPath)
