# Empty date field returns current repo

oldRepos <- getOption("repos")
setSnapshot()

# Valid snapshot date
setSnapshot("2014-11-16")

# Invalid snapshot date (in future), returns error
\dontrun{
setSnapshot("2100-01-01")
}

options(repos = oldRepos)
