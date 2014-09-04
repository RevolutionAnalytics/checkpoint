# install_github_pkgs <- function(lib, pkgs){
#   allPkgs <- list.files(file.path(lib, "src/contrib"), full.names = TRUE)
#   names(allPkgs) <- gsub("_[0-9].+", "", list.files(file.path(lib, "src/contrib")))
#   allPkgs <- allPkgs[!grepl("PACKAGES", allPkgs)]
#   pkgsWithPath <- unname(sapply(pkgs, function(x) allPkgs[grepl(x, names(allPkgs))]))
#   pkgsWithPath <- pkgsWithPath[!sapply(pkgsWithPath, length) == 0]
#   
#   gh_install <- pkgsWithPath[grep("\\.zip", pkgsWithPath)]
#   zipFunction <- function(x) {
#     splitx <- strsplit(x, '/')[[1]]
#     sub("\\.zip", "", splitx[length(splitx)])
#   }
#   gh_install_names <- vapply(gh_install,
#                              zipFunction,
#                              character(1), USE.NAMES = FALSE)
#   for(i in seq_along(gh_install_names)){
#     install_other(pkg=gh_install_names[i], lib=lib)
#   }
# }
# 


# #' Download github packages.
# #' 
# #' @keywords internal
# #' @param repo Repository path
# #' @param pkgs Vector of package names
# #' @param lib Library path
# download_github_pkgs <- function(repo, pkgs, lib, verbose=TRUE){
#   mssg(verbose, "Downloading github packages")
#   yaml_file <- rrtPath(repo, "manifest")
#   githubPaths <- if(file.exists(yaml_file)){
#     yaml.load_file(yaml_file)$Github
#   } else NULL
#   if(is.null(githubPaths)) { character(0) } else {
#     toinstall <- sapply(pkgs, function(x) grep(x, githubPaths, value = TRUE), USE.NAMES = FALSE)
#     for(i in seq_along(toinstall)){
#       pathsplit <- strsplit(toinstall[i], "/")[[1]]
#       download_one_github_pkg(pkg=pathsplit[[2]], username=pathsplit[[1]], lib=lib)
#     }
#   }
# }
# 
