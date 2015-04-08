checkpointBasePkgs <- function(checkpointLocation = "~/"){
  checkpointPath(checkpointLocation = checkpointLocation, type = "base")
}

basePkgLocations <- function(lib = .Library){
  pkgFolder <- function(x) dirname(normalizePath(base::system.file(package=x, lib.loc=lib)))
  bp <- installed.packages(priority="base", lib.loc = lib)[, "Package"]
  locations <- vapply(bp, pkgFolder, FUN.VALUE = character(1))
  w <- locations != pkgFolder("base")
  w["compiler"] <- TRUE # Ensure compiler is always copied
  locations[w]
}

installMissingBasePackages <- function(){
  missingBase <- basePkgLocations()
  checkpointRoot = authorizeFileSystemUse(dirname(checkpointBasePkgs()))
  Rver = basename(checkpointBasePkgs())
  if(length(missingBase)){
    for(i in seq_along(missingBase)){
      pkg <- missingBase[i]
      to.dir <- file.path(checkpointRoot, Rver, names(pkg))
      dir.create(to.dir, showWarnings = FALSE, recursive = TRUE)
      file.copy(to = dirname(to.dir), from = file.path(unname(pkg), names(pkg)), recursive = TRUE)
    }
  }
}
