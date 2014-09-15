# tests for initialize

project_root <- file.path(tempdir(), "rrttemp")
if(!file.exists(project_root)) dir.create(project_root)

snap_date <- "2014-09-08"

checkpoint(snap_date, repo = project_root)

# Check that CRAN mirror is set to MRAN snapshot
getOption("repos")

# Check that library path is set to ~/.rrt
.libPaths()
installed.packages()

# Write dummy code file to repo
cat("library(MASS)", "library(XML)",
    sep="\n",
    file = file.path(project_root, "code.R")
)


checkpoint(snap_date, repo = project_root)
installed.packages()

# cleanup
unlink(project_root, recursive=TRUE)

