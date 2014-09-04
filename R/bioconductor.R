# #' Determine if a package is on Bioconductor or not.
# #' 
# #' @keywords internal
# #' @param pkgs Vector of package names
# #' @return A logical vector of same length as input vector
# is.biocPackage <- function(pkgs){
#   file <- file.path(tempdir(), "rrt_biockgs.rda")
#   bioc_pkgs <- suppressWarnings(tryCatch(load(file), error = function(e) e))
#   if(is(bioc_pkgs, "simpleError")){
#     bioc_pkgs <- all_group()
#     save(bioc_pkgs, file = file)
#   } else { load(file) }
#   pkgs %in% bioc_pkgs
# }
# 
# 
# 
# download_bioconductor_pkgs <- function(lib, pkgs, repo, verbose=TRUE){
#   mssg(verbose, "Downloading BioConductor packages")
#   bioc_pkgs <- all_group()
#   pkgs_bioc <- pkgs[pkgs %in% bioc_pkgs]
#   if(!length(pkgs_bioc) == 0){
#     source("http://bioconductor.org/biocLite.R")
#     BiocInstaller::biocLite(pkgs_bioc, lib = lib, 
#                             destdir = file.path(lib, "src/contrib"), 
#                             dependencies=FALSE, suppressUpdates = TRUE, 
#                             suppressAutoUpdate = TRUE)
#   }
# }