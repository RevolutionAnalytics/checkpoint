# These functions mock installation of packages, for fast testing purposes only

is_mock_environment <- function()Sys.getenv("checkpoint.mock.install") == TRUE
set_mock_environment <- function()Sys.setenv("checkpoint.mock.install" = TRUE)
reset_mock_environment <- function()Sys.setenv("checkpoint.mock.install" = FALSE)

lib_in_tempdir <- function(lib){
  np <- function(x)normalizePath(x, winslash = "/")
  grepl(np(tempdir()), np(lib))
}

install.packages <- function(pkgs, lib = .libPaths()[1], repos = getOption("repos"), ...){
  if(is_mock_environment() && lib_in_tempdir(lib)){
    mock.install.packages(pkgs = pkgs, lib = lib, repos = repos, ...)
  } else {
    capture.output(
      z <- utils::install.packages(pkgs = pkgs, lib = lib, repos = repos, ...)
    )
    invisible()
  }
  
}

installed.packages <- function(lib.loc = .libPaths()[1], ...){
  if(is_mock_environment() && lib_in_tempdir(lib.loc)){
    mock.installed.packages(lib.loc = lib.loc,  ...)
  } else {
    utils::installed.packages(lib.loc = lib.loc,  ...)
  }
  
}


# mocks install.packages()
# writes a description file with three lines
mock.install.packages <- function(pkgs, lib = .libPaths()[1], repos = getOption("repos"), ...){
  np <- function(x)normalizePath(x, winslash = "/")
  stopifnot(grepl(np(tempdir()), np(lib)))
  p <- available.packages()[pkgs, ]  
  msg <- paste0("Package: ", pkgs, "\n", "Version: ", p["Version"], "\n", "Description: ", pkgs)
  fp <- file.path(lib, pkgs)
  # unlink(fp, recursive = TRUE)
  dir.create(fp, recursive = TRUE, showWarnings = FALSE)
  message("Mocking Content type 'application/zip' length 0 bytes (0 KB)")
  cat(msg, file = file.path(fp, "DESCRIPTION"))
  invisible()
}


# mocks installed.packages()
mock.installed.packages <- function(lib.loc = .libPaths()[1], priority, ...){
  if(!missing(priority) && priority == "base") {
    utils::installed.packages(lib.loc = lib.loc, priority = priority, ...)
  }
  f <- list.files(lib.loc, pattern = "DESCRIPTION$", recursive = TRUE, full.names = FALSE)
  f <- f[grepl("^\\w*/DESCRIPTION$", f)]
  p <- dirname(f)
  pdb <- available.packages()
  pdb[pdb[, "Package"] %in% p, ]
}
