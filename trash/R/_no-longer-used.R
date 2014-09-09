# #' Set the MRAN snapshot to get packages from.
# #'
# #' @import httr RJSONIO
# #' @export
# #'
# #' @param snapshot A MRAN snapshot. Defaults to most recent snapshot
# #' @param repo Repository path.
# #'
# #' @family mran
# #' 
# #' @examples \dontrun{
# #' mran_set()
# #' mran_set(snapshot="2014-07-01_2000")
# #' }
# mran_set <- function(snapshot=NULL, repo=getwd(), verbose=TRUE){
#   if(is.null(snapshot)){
#     gg <- mranSnapshots(verbose=FALSE)
#     snapshot <- gg[length(gg)] # get the latest snapshot (latest date that is)
#   }
#   
#   url <- sprintf("%s/%s/", file.path(mranServerUrl(), 'snapshots'), snapshot)
#   res <- GET(url)
#   if(res$status_code >= 300){
#     msg <- "%s - snapshot not found, you don't have an internet connection, or other error..."
#     stop(sprintf(msg, res$status_code))
#     
#   }
#   
#   # check for folder, and rrt folder inside
#   createRepoFolders(repo)
#   
#   # write MRAN snapshot id
#   mssg(verbose, "Writing new snapshotID...")
#   writeManifest(repo, lib, packs = NULL, repoid = repoDigest(repo), snapshot = snapshot)
#   mssg(verbose, "...Done")
# }


# #' Function to download packages
# #'
# #' @param x (character) A vector of package names. If NULL, none installed, and message prints
# #' @param repo Repository path
# #' @param lib (character) Library location, a directory
# #' @param recursive (logical) Recursively install packages?
# #' @param verbose (logical) Inherited from call to rrt_init or rrt_refresh
# #' @param install (logical) Install packages or just download packages. Not used yet...
# #' @param mran (logical) If TRUE, packages are installed from the MRAN server. See
# #' \url{http://mran.revolutionanalytics.com} for more information.
# #' @param snapshotdate Date of MRAN snapshot to use. E.g. "2014-06-20"
# #' @param snapshotid MRAN snapshotID to use. E.g. "2014-06-30_1700"
# #'
# #' @keywords internal
# #'
# #' @examples \dontrun{
# #' getPkgs("<path to RRT repo>")
# #' }
# 
# getPkgs <- function(x, repo, lib, recursive=FALSE, verbose=TRUE, install=TRUE, mran=FALSE){
#   # check for existence of pkg, subset only those that need to be installed
#   mssg(verbose, "... running getPkgs()")
#   if(is.null(x)){ NULL } else {
# 
#     pkgslist <- list.files(file.path(lib, "src/contrib"))
#     if(length(pkgslist) == 0) { pkgs2get <- x } else {
#       gotpkgs <- vapply(pkgslist, function(x) strsplit(x, "_")[[1]][[1]], character(1), USE.NAMES = FALSE)
#       pkgs2get <- sort(x)[!sort(x) %in% sort(gotpkgs)]
#     }
# 
#     # Make local repo of packages
#     if(!is.null(pkgs2get) || length(pkgs2get) == 0){
#       if(!mran){
#         NULL
#       } else {
#         pkgloc <- file.path(lib, "src/contrib")
#         setwd(lib)
#         on.exit(setwd(repo))
#         dir.create("src/contrib", showWarnings = FALSE, recursive = TRUE)
#         snapdateid <- getOption('RRT_snapshotID')
#         downloadPackageFromMran(repo=repo, lib=lib, snapshotid = snapdateid, pkgs=pkgs2get, outdir=pkgloc)
#       }
#       
#       # download pkgs from Github
#       get_github_pkgs(lib, pkgs2get, repo)
#     } else {
#       return(mssg(verbose, "No packages found - none downloaded"))
#     }
#   }
# }
# 
# get_github_pkgs <- function(lib, pkgs, repo){
#   mssg(TRUE, "get_github_packages in getPkgs.R")
#   availcranpkgs <- row.names(availCRANpkgs())
#   notoncran <- pkgs[!pkgs %in% availcranpkgs]
#   message(sprintf("Not found on CRAN:\n%s", paste0(notoncran, collapse = ", ")))
#   githubpaths <- yaml.load_file(file.path(repo, "rrt/rrt_manifest.yml"))$Github
#   toinstall <- sapply(notoncran, function(x) grep(x, githubpaths, value = TRUE), USE.NAMES = FALSE)
#   for(i in seq_along(toinstall)){
#     pathsplit <- strsplit(toinstall[i], "/")[[1]]
#     get_github(lib=lib, pkg=pathsplit[[2]], username=pathsplit[[1]])
#   }
# }
# 
# availCRANpkgs <- function (repos = getOption("repos"), type = getOption("pkgType"), ...){
#   if (!grepl("^file", repos) && file.exists(repos)) {
#     repos <- paste0("file:///", repos)
#   }
#   available.packages(contrib.url(repos, type = type))
# }