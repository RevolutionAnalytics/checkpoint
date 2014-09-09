# tests for initialize

repo_root <- file.path(tempdir(), "rrttemp")

snap_date <- "2014-09-08"

checkpoint(snap_date, repo = repo_root)

# Write dummy code file to repo
cat("library(MASS)", "library(plyr)", "library(XML)", "library('httr')", 
    sep="\n", 
    file = file.path(repo_root, "code.R")
)

repoScanPackages(repo_root)

repoInstalledPackages(repo_root)



# cleanup
cleanRRTfolder(repo_root)

