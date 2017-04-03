# Temporary fix for rstudio bug reported at https://support.rstudio.com/hc/communities/public/questions/203052576-setting-libpath-in-Rstudio
#
# To replicate the error, use:
# loc <- normalizePath(tempdir())
# assign(".lib.loc", loc, envir = environment(.libPaths))


# This is the original RStudio code
original.listInstalledPackages <- function (){
  
  .rs.uniqueLibraryPaths <- .rs.pathPackage <- .rs.packageVersion <- 
    .rs.createAliasedPath <- .rs.addFunction <- NULL
  rm(.rs.uniqueLibraryPaths,  .rs.pathPackage,  .rs.packageVersion,  
     .rs.createAliasedPath,  .rs.addFunction)
  
  uniqueLibPaths <- .rs.uniqueLibraryPaths()
  x <- suppressWarnings(library(lib.loc = uniqueLibPaths))
  x <- x$results[x$results[, 1] != "base", ]
  pkgs.name <- x[, 1]
  pkgs.library <- x[, 2]
  pkgs.desc <- x[, 3]
  pkgs.url <- file.path("help/library", pkgs.name, "html", 
                        "00Index.html")
  loaded.pkgs <- .rs.pathPackage()
  pkgs.loaded <- !is.na(match(normalizePath(paste(pkgs.library, 
                                                  pkgs.name, sep = "/")), loaded.pkgs))
  instPkgs <- as.data.frame(installed.packages(), stringsAsFactors = F)
  pkgs.version <- character(length = length(pkgs.name))
  for (i in 1:length(pkgs.name)) {
    pkgs.version[[i]] <- .rs.packageVersion(pkgs.name[[i]], 
                                            pkgs.library[[i]], instPkgs)
  }
  pkgs.library <- .rs.createAliasedPath(pkgs.library)
  packages = data.frame(name = pkgs.name, library = pkgs.library, 
                        version = pkgs.version, desc = pkgs.desc, url = pkgs.url, 
                        loaded = pkgs.loaded, check.rows = TRUE, stringsAsFactors = FALSE)
  packages[order(packages$name), ]
}


# This is the replacement code
replacement.listInstalledPackages <- function(){
  .rs.uniqueLibraryPaths <- .rs.pathPackage <- .rs.packageVersion <- 
    .rs.createAliasedPath <- .rs.addFunction <- NULL
  rm(.rs.uniqueLibraryPaths,  .rs.pathPackage,  .rs.packageVersion,  
     .rs.createAliasedPath,  .rs.addFunction)
  
  # calculate unique libpaths
  uniqueLibPaths <- .rs.uniqueLibraryPaths()
  
  # get packages
  x <- suppressWarnings(library(lib.loc=uniqueLibPaths))
  x <- x$results[x$results[, 1] != "base", , drop=FALSE] # Add drop=FALSE
  
  
  # extract/compute required fields 
  pkgs.name <- x[, 1]
  pkgs.library <- x[, 2]
  pkgs.desc <- x[, 3]
  pkgs.url <- file.path("help/library",
                        pkgs.name, 
                        "html", 
                        "00Index.html")
  loaded.pkgs <- .rs.pathPackage()
  pkgs.loaded <- !is.na(match(normalizePath(
    paste(pkgs.library,pkgs.name, sep="/")),
    loaded.pkgs))
  
  
  # build up vector of package versions
  instPkgs <- as.data.frame(installed.packages(), stringsAsFactors=F)
  pkgs.version <- character(length=length(pkgs.name))
  for (i in 1:length(pkgs.name)) {
    pkgs.version[[i]] <- .rs.packageVersion(pkgs.name[[i]],
                                            pkgs.library[[i]],
                                            instPkgs)
  }
  # alias library paths for the client
  pkgs.library <- .rs.createAliasedPath(pkgs.library)
  
  # return data frame sorted by name
  packages = data.frame(name=pkgs.name,
                        library=pkgs.library,
                        version=pkgs.version,
                        desc=pkgs.desc,
                        url=pkgs.url,
                        loaded=pkgs.loaded,
                        check.rows=FALSE, # Modify check.rows=FALSE
                        stringsAsFactors=FALSE)
  
  # sort and return
  packages[order(packages$name),]
}


fixRstudioBug <- function(reset = FALSE){
  .rs.uniqueLibraryPaths <- .rs.pathPackage <- .rs.packageVersion <- 
    .rs.createAliasedPath <- .rs.addFunction <- NULL
  rm(.rs.uniqueLibraryPaths,  .rs.pathPackage,  .rs.packageVersion,  
     .rs.createAliasedPath,  .rs.addFunction)
  
  if(!"tools:rstudio" %in% search()) return(NULL)
  
  if(reset){
    .rs.addFunction("listInstalledPackages", original.listInstalledPackages)
  } else{
    if(
      isTRUE(
        all.equal(original.listInstalledPackages, get(".rs.listInstalledPackages"))
      )
    ) {
      .rs.addFunction("listInstalledPackages", replacement.listInstalledPackages)
    }
  }
  
}

