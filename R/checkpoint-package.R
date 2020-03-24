#' Install packages from snapshots on the checkpoint server for reproducibility
#'
#' The goal of checkpoint is to solve the problem of package reproducibility in R. Specifically, checkpoint allows you to install packages as they existed on CRAN on a specific snapshot date as if you had a CRAN time machine.
#'
#' To achieve reproducibility, the [`create_checkpoint`] function installs the packages required or called by your project and scripts to a local library exactly as they existed at the specified point in time. Only those packages are available tot your project, thereby avoiding any package updates that came later and may have altered your results. In this way, anyone using checkpoint can ensure the reproducibility of your scripts or projects at any time.
#'
#' To create the snapshot archives, once a day (at midnight UTC) we refresh the Austria CRAN mirror, on the checkpoint server (https://mran.microsoft.com/).  Immediately after completion of the `rsync` mirror process, we take a snapshot, thus creating the archive.  Snapshot archives exist starting from 2014-09-17.
#'
#' `checkpoint` exposes the following functions:
#'
#' * [`create_checkpoint`]: Creates a checkpoint by scanning a project folder and downloading and installing any packages required from MRAN.
#' * [`use_checkpoint`]: Uses a previously created checkpoint, by setting the library search path to the checkpoint path, and the CRAN mirror to MRAN.
#' * [`delete_checkpoint`]: Deletes an existing checkpoint.
#' * [`delete_all_checkpoints`]: Deletes _all_ existing checkpoints.
#' * [`uncheckpoint_session`]: Stops using a checkpoint, restoring the library search path and CRAN mirror to their original state.
#' * [`scan_project_files`]: Scans a project for any required packages.
#' * [`list_mran_snapshots`]: Returns all valid snapshot dates found on MRAN.
#'
#' @name checkpoint-package
#' @docType package
#' @keywords package
NULL
