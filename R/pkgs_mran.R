#' Download R packages from the MRAN server
#'
#' This function uses rsync on *unix machines, which is faster than the method (wget)
#' \code{install.packages} uses by default. On Windows we use your default method of downloading
#' files. This function does not install packages, but only downloads them to your machine.
#'
#' @export
#' @param repo Repository path
#' @param lib (character) Library location, a directory
#' @param date Date as "year-month-day" (YY-MM-DD)
#' @param snapshotid Optional. You can give the exact snapshot ID insetad of a date.
#' @param outdir Output directory
#' @param pkgs Packages to install with version numbers, e.g. plyr_1.8.1
#' @param quiet Passed to \code{\link[utils]{install.packages}}
#' @param verbose (logical) Whether to print messages or not (Default: FALSE).
#' @examples \dontrun{
#' # By default installs most recent version
#' pkgs_mran(date='2014-08-04', pkgs=c("plyr","ggplot2"), outdir="~/mran_snaps/")
#'
#' pkgs_mran(date='2014-06-19', pkgs=c("plyr_1.8.1","ggplot2_1.0.0"), outdir="~/mran_snaps/stuff/")
#' pkgs_mran(date='2014-06-19', pkgs="rgbif_0.6.2", outdir="~/mran_snaps")
#' }

pkgs_mran <- function(repo=NULL, lib=NULL, date=NULL, snapshotid=NULL, pkgs=NULL, outdir=NULL, verbose=FALSE, quiet=FALSE)
{
  if(is.null(outdir)) stop("You must specify a directory to download packages to")
  if(is.null(pkgs)) stop("You must specify one or more packages to get")

  # get available snapshots
  snapshot_use <- if(is.null(snapshotid)) getsnapshotid(date) else snapshotid

  # parse versions from pkgs
  get_pkg_versions <- function(x){
    vers <- tryCatch(mran_pkg_versions(snapshot=snapshot_use, package=x[[1]]), error=function(e) e)
    if("error" %in% class(vers)){
      sprintf("%s/__notfound__", x[[1]])
    } else {
      splitvers <- vapply(vers, strsplit, list(1), "\\.|-")
      tmp <- lapply(splitvers, function(x) data.frame(rbind(x), stringsAsFactors = FALSE))
      lengths <- vapply(tmp, length, numeric(1))
      toadd <- max(lengths) - min(lengths)
      if(toadd > 0){
        tmp[vapply(tmp, length, numeric(1)) < max(lengths)] <-
          lapply(tmp[vapply(tmp, length, numeric(1)) < max(lengths)],
                 function(b){
                   ss <- data.frame(b, t(rep(NA, max(lengths)-NCOL(b))), stringsAsFactors = FALSE)
                   names(ss) <- paste0('X', 1:max(lengths))
                   ss
                })
      }
      df <- do.call(rbind, tmp)
      df[is.na(df)] <- 0
      row.names(df) <- names(splitvers)
      df <- suppressWarnings(colClasses(df, "numeric"))
      df <- if(NCOL(df) == 4){
        sort_df(df, c("X1","X2","X3","X4"))
      } else if(NCOL(df) == 3) {
        sort_df(df, c("X1","X2","X3"))
      } else {
        sort_df(df, c("X1","X2"))
      }
      pkgver <- tryCatch(x[[2]], error=function(e) e)
      if('error' %in% class(pkgver)) {
        pkgveruse <- row.names(df[nrow(df),])
      } else {
        pkgveruse <- if(pkgver %in% vers) pkgver else unname(verssorted[length(verssorted)])
      }
      sprintf("%s/%s_%s.tar.gz", x[[1]], x[[1]], pkgveruse)
    }
  }

  pkgs <- lapply(pkgs, function(x) strsplit(x, "_")[[1]])
  pkgpaths <- sapply(pkgs, get_pkg_versions)

  notonmran <- grep("__notfound__", pkgpaths, value = TRUE)
  pkgpaths <- pkgpaths[!grepl("__notfound__", pkgpaths)]

  if(!.Platform$OS.type == "unix"){
    for(i in seq_along(pkgpaths)){
      windows_install(pkgpaths[[i]], lib=lib, snapshotid=snapshotid, quiet=quiet)
    }
  } else {
    setwd(outdir)
    tmppkgsfileloc <- "_rsync-file-locations.txt"
    cat(pkgpaths, file = tmppkgsfileloc, sep = "\n")

    if(length(pkgpaths > 0)){

      mssg(verbose, "... Downloading package files")
      url <- mran_server_url()
      url <- sub("http://", "", url)
      cmd <- sprintf('rsync -rt --progress --files-from=%s %s::MRAN-src-snapshots/%s .', tmppkgsfileloc, url, snapshot_use)
      system(cmd, intern=TRUE)

      mvcmd <- sprintf("mv %s ./", paste(pkgpaths, collapse = " "))
      system(mvcmd)

      rmcmd <- sprintf("rm -rf %s", paste(
        sapply(pkgpaths, function(x) strsplit(x, "/")[[1]][[1]], USE.NAMES = FALSE), collapse = " ")
      )
      system(rmcmd)
      system(sprintf("rm %s", tmppkgsfileloc))
    }

  }
}

windows_install <- function(x, lib, snapshotid, quiet=FALSE){
  pkg <- strsplit(x, "/")[[1]]
  url <- sprintf("%s/snapshots/src/%s/%s", mran_server_url(), snapshotid, x)
  destfile <- file.path(lib, 'src/contrib', pkg[[2]])
  download.file(url, destfile=destfile, quiet=quiet)
}

colClasses <- function (d, colClasses)
{
  colClasses <- rep(colClasses, len = length(d))
  d[] <- lapply(seq_along(d), function(i) switch(colClasses[i],
            numeric = as.numeric(d[[i]]), character = as.character(d[[i]]),
            Date = as.Date(d[[i]], origin = "1970-01-01"), POSIXct = as.POSIXct(d[[i]],
                  origin = "1970-01-01"), factor = as.factor(d[[i]]),
            as(d[[i]], colClasses[i])))
  d
}

sort_df <- function (data, vars = names(data)){
  if (length(vars) == 0 || is.null(vars))
    return(data)
  data[do.call("order", data[, vars, drop = FALSE]), , drop = FALSE]
}

getsnapshotid <- function(date, forcelast=FALSE){
  # get available snapshots
  availsnaps <- suppressMessages(mran_snaps())

  if(is.null(date)) date <- Sys.Date()
  snapshots <- grep(date, availsnaps, value = TRUE)
  if(length(snapshots) > 1){
    if(!forcelast){
      print(data.frame(snapshots))
      message("\nMore than one snapshot matching your date found \n",
              "Enter rownumber of snapshot (other inputs will return 'NA'):\n")
      take <- scan(n = 1, quiet = TRUE, what = 'raw')
      if(is.na(take)){ message("No snapshot found or you didn't select one") }
      snapshots[as.numeric(take)]
    } else { snapshots[length(snapshots)] }
  } else { snapshots }
}
