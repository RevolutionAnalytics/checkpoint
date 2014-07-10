#' Install packages from locations other than CRAN, its mirrors, or MRAN
#'
#' @import httr devtools
#' @keywords internal
#' @examples \dontrun{
#' get_github(lib=lib, pkg="cowsay", username="sckott")
#' }

get_github <- function(pkg, username, lib, ...){
  refurl <- devtools:::github_get_conn(pkg, username, ...)
  res <- GET(refurl$url)
  stop_for_status(res)
  installtmpfile <- file.path(lib, "src/contrib", paste0(pkg, ".zip"))
  writeBin(content(res), installtmpfile)
#   unzip(sprintf("%s/tmp.zip", installtmpfile), exdir = file.path(lib, "src/contrib"))
#   unlink(installtmpdir)
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
  #   unzip(sprintf("%s/tmp.zip", installtmpfile), exdir = file.path(lib, "src/contrib"))
  #   unlink(installtmpdir)
  inst <- file.path(lib, "src/contrib", sprintf("%s.zip", pkg))
  install_local(inst, args=sprintf("--library=%s", lib))
}
