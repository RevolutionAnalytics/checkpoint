if(interactive()) library(testthat)

context("scan for library, require, :: and :::")


collapse <- function(...) paste(..., sep="\n", collapse="\n")
foo <- function(token, insert){
  collapse(sprintf(token, insert))
}

project_root <- file.path(tempdir(), "checkpoint-test-temp")
dir.create(project_root, recursive = TRUE, showWarnings = FALSE)
# sink(file = file.path(tempdir(), "checkpoint_sink.txt"))

code <- collapse(
  foo("library(%s)", letters[1:2]),
  foo("require(%s)", letters[3:4]),
  foo("%s::foo()", letters[5:6]),
  foo("%s:::bar()", letters[7:8]),
  "track <- setClass('track', slots = c(x='numeric', y='numeric'))"
)
test_that("scanRepoPackages finds dependencies", {
  
  # "finds packages in R scripts"
  # Write dummy code file to project
  
  codefile <- file.path(project_root, "code.R")
  cat(code, file = codefile)
  
  found <- checkpoint:::projectScanPackages(project = project_root)
  expect_equal(found$pkgs, sort(c(letters[1:8], "methods")))
  
  file.remove(codefile)
})

test_that("finds packages in Rmarkdown", {
  if(!knitr.is.installed()) skip("knitr not available")
  
  # Write dummy knitr code file to project
  knit <- sprintf("```{r}\n%s\n```", code)
  knitfile <- file.path(project_root, "knit.Rmd")
  cat(knit, file = knitfile)
  
  found <- projectScanPackages(project = project_root, use.knitr = TRUE)
  expect_equal(found$pkgs, c(letters[1:8], "methods"))
  file.remove(knitfile)
})

test_that("auto-installs knitr and rmardown", {
  if(!knitr.is.installed()) skip("knitr not available")
  
  # Write dummy knitr code file to project
  knit <- sprintf("```{r}\n%s\n```", code)
  knitfile <- file.path(project_root, "knit.Rmd")
  cat(knit, file = knitfile)
  
  found <- projectScanPackages(project = project_root, use.knitr = TRUE, 
                               auto.install.knitr = TRUE)
  expect_equal(found$pkgs, c(letters[1:8], "methods", "knitr", "rmarkdown"))
  file.remove(knitfile)
})



# "scanRepoPackages allows switching between knitr and Sweave"
code <- collapse(
  "\\documentclass{article}",
  "\\begin{document}",
  "\\SweaveOpts{concordance=TRUE}",
  "Some text",
  "<<foo, cache = FALSE>>=",
  "  library(abc)",
  "  x <- FALSE",
  "@",
  "",
  "<<bar, eval = x>>=",
  "  1+1",
  "@",
  "Nothing interesting here",
  "\\end{document}"
)
codefile <- file.path(project_root, "code.Rnw")
cat(code, file = codefile)

test_that("Sweave scans Rnw files with eval=FALSE chunks", {
  found <- projectScanPackages(project = project_root, use.knitr = FALSE, 
                               scan.rnw.with.knitr = FALSE)
  expect_equal(found$pkgs, "abc")
  expect_equal(found$error, character(0))
})

# test_that("knitr scans Rnw files with eval=FALSE chunks", {
#   found <- projectScanPackages(project = project_root, use.knitr = TRUE, 
#                                scan.rnw.with.knitr = TRUE)
#   expect_equal(found$pkgs, "abc")
#   expect_equal(found$error, character(0))
# })


test_that("lapplyProgressBar behaves like lapply()", {
  if(!interactive()) skip("Not interactive()")
  expect_output(lapplyProgressBar(1:5, identity), "===")
})

test_that("lapplyProgressBar returns the value of lapply", {
  expect_equal(lapplyProgressBar(1:5, identity), lapply(1:5, identity))
})


unlink(codefile)
unlink(project_root)
# Ensure sink() gets reset
for(i in seq_len(sink.number())) sink()

