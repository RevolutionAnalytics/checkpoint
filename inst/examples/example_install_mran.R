\dontrun{
  repo_path <- tempdir()
  createRepoFolders(repo_path)
  
  install_mran(repo=repo_path, pkg="plyr", snapshotdate="2014-08-01")
  
  dir(instPath)
  unlink(instPath, recursive = TRUE)
}



