\dontrun{

# Create temporary project and set working directory

example_project <- paste0("~/checkpoint_example_project_", Sys.Date())

dir.create(example_project, recursive = TRUE)


# Write dummy code file to project

cat("
library(MASS)
library(foreach)
", file="checkpoint_example_code.R")


# Create a checkpoint by specifying a snapshot date
# recommended practice is to specify the R version explicitly
ver <- getRversion()
create_checkpoint("2014-09-17", r_version=ver, project_dir=example_project)
use_checkpoint("2014-09-17", r_version=ver)

# more terse alternative is checkpoint(), which is equivalent to
# calling create_checkpoint() and then use_checkpoint() in sequence
checkpoint("2014-09-17", r_version=ver, project_dir=example_project)


# Check that CRAN mirror is set to MRAN snapshot
getOption("repos")


# Check that (1st) library path is set to ~/.checkpoint
.libPaths()


# Check which packages are installed in checkpoint library
installed.packages()


# restore initial state
uncheckpoint_session()

}

