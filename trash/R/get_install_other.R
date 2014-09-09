#' Install packages from locations other than CRAN, its mirrors, or MRAN
#'
#' @import httr devtools
#' @keywords internal
#' @examples \dontrun{
#' download_one_github_pkg(lib=tempdir(), pkg="cowsay", username="sckott")
#' dir(file.path(tempdir, "src/contrib"))
#' }

download_one_github_pkg <- function(pkg, username, lib, ...){
  refurl <- devtools:::github_get_conn(pkg, username, ...)
  res <- GET(refurl$url)
  stop_for_status(res)
  instPath <- file.path(lib, "src/contrib")
  if(!file.exists(instPath)) dir.create(instPath, recursive = TRUE)
  instFile <- file.path(instPath, paste0(pkg, ".zip"))
  writeBin(content(res), instFile)
}



#' Install packages that were downloaded from non-CRAN like places
#'
#' @import devtools
#' @param pkg Package name
#' @param lib Path to packages library
#' @keywords internal
#' @examples \dontrun{
#' install_other(pkg="cowsay", lib=lib)
#' }

install_other <- function(pkg, lib){
  inst <- file.path(lib, "src/contrib", sprintf("%s.zip", pkg))
  install_local(inst, args=sprintf("--library=%s", lib), dependencies=FALSE)
}
