#' Download R packages from the MRAN server
#'
#' This function uses rsync, which is faster than the method (wget) \code{install.packages} uses
#' by default. This function does not install packages, but only downloads them to your machine.
#'
#' @export
#' @param repo Repository path
#' @param lib (character) Library location, a directory
#' @param date Date as "year-month-day" (YY-MM-DD)
#' @param snapshotid Optional. You can give the exact snapshot ID insetad of a date.
#' @param outdir Output directory
#' @param pkgs Packages to install with version numbers, e.g. plyr_1.8.1
#' @examples \dontrun{
#' # By default installs most recent version
#' pkgs_mran(date='2014-07-08', pkgs=c("plyr","ggplot2"), outdir="~/mran_snaps/")
#'
#' pkgs_mran(date='2014-06-19', pkgs=c("plyr_1.8.1","ggplot2_1.0.0"), outdir="~/mran_snaps/stuff/")
#' pkgs_mran(date='2014-06-19', pkgs="rgbif_0.6.2", outdir="~/mran_snaps/stuff/")
#' }

pkgs_mran <- function(repo=NULL, lib=NULL, date=NULL, snapshotid=NULL, pkgs=NULL, outdir=NULL)
{
  if(is.null(outdir)) stop("You must specify a directory to download packages to")
  if(is.null(pkgs)) stop("You must specify one or more packages to get")

  # get available snapshots
  snapshot_use <- if(is.null(snapshotid)) getsnapshotid(date) else snapshotid

  # parse versions from pkgs
  get_pkg_versions <- function(x){
    vers <- tryCatch(mran_pkg_avail(snapshot=snapshot_use, package=x[[1]]), error=function(e) e)
    if("error" %in% class(vers)){
      sprintf("%s/__notfound__", x[[1]])
    } else {
      splitvers <- vapply(vers, strsplit, list(1), "\\.")
      tmp <- lapply(splitvers, function(x) data.frame(rbind(x), stringsAsFactors = FALSE))
      lengths <- vapply(tmp, length, numeric(1))
      toadd <- max(lengths) - min(lengths)
      if(toadd > 0){
#         gg <- tmp[vapply(tmp, length, numeric(1)) < max(lengths)]
        tmp[vapply(tmp, length, numeric(1)) < max(lengths)] <-
          lapply(tmp[vapply(tmp, length, numeric(1)) < max(lengths)],
                 function(b){
                   ss <- data.frame(b, t(rep(NA, max(lengths)-NCOL(b))), stringsAsFactors = FALSE)
                   names(ss) <- paste0('X', 1:max(lengths))
                   ss
                })
      }
      df <- do.call(rbind, tmp)
      #
#       df <- data.frame(do.call(rbind.fill, lapply(splitvers, function(x) data.frame(rbind(x), stringsAsFactors = FALSE))), stringsAsFactors = FALSE)
      df[is.na(df)] <- 0
      row.names(df) <- names(splitvers)
      df <- suppressWarnings(colClasses(df, "numeric"))
      if(NCOL(df) == 3){ df <- sort_df(df, c("X1","X2","X3")) } else {
        df <- sort_df(df, c("X1","X2"))
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

  if(length(notonmran) > 0) {
    gg <- vapply(notonmran, function(x) strsplit(x, "/")[[1]][[1]], character(1), USE.NAMES = FALSE)
    message(sprintf("Not found on MRAN:\n%s", paste0(gg, collapse = ", ")))
    githubpaths <- yaml.load_file(file.path(repo, "rrt/rrt_manifest.yml"))$Github
    toinstall <- sapply(gg, function(x) grep(x, githubpaths, value = TRUE), USE.NAMES = FALSE)
    for(i in seq_along(toinstall)){
      pathsplit <- strsplit(toinstall[i], "/")[[1]]
      get_github(lib=lib, pkg=pathsplit[[2]], username=pathsplit[[1]])
    }
  }

  setwd(outdir)
  tmppkgsfileloc <- "_rsync-file-locations.txt"
  cat(pkgpaths, file = tmppkgsfileloc, sep = "\n")

  if(length(pkgpaths > 0)){

    message("... Downloading package files")
    url <- mran_server_url()
    url <- sub("http://", "", url)
    cmd <- sprintf('rsync -rt --progress --files-from=%s %s::MRAN-snapshots/%s .', tmppkgsfileloc, url, snapshot_use)
    system(cmd, intern=TRUE)
  
  #   cpcmd <- sprintf("cp %s .", paste(pkgpaths, collapse = " "))
  #   system(cpcmd)
  
    mvcmd <- sprintf("mv %s ./", paste(pkgpaths, collapse = " "))
    system(mvcmd)
  
    rmcmd <- sprintf("rm -rf %s", paste(
      sapply(pkgpaths, function(x) strsplit(x, "/")[[1]][[1]], USE.NAMES = FALSE), collapse = " ")
      )
    system(rmcmd)
    system(sprintf("rm %s", tmppkgsfileloc))
  #   message("... Generating PACKAGES index file")
  #   tools::write_PACKAGES(dir=".", type="source")
}
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

getsnapshotid <- function(date){
  # get available snapshots
  availsnaps <- suppressMessages(mran_snaps())

  if(is.null(date)) date <- Sys.Date()
  snapshots <- grep(date, availsnaps, value = TRUE)
  if(length(snapshots) > 1){
    print(data.frame(snapshots))
    message("\nMore than one snapshot matching your date found \n",
            "Enter rownumber of snapshot (other inputs will return 'NA'):\n")
    take <- scan(n = 1, quiet = TRUE, what = 'raw')
    if(is.na(take)){ message("No snapshot found or you didn't select one") }
    snapshots[as.numeric(take)]
  } else { snapshots }
}
