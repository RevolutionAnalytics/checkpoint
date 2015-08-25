library("testthat")
context("scanRepoPackages")

describe("scan recognizes library, require, :: and :::", {
  
  
  collapse <- function(...) paste(..., sep="\n", collapse="\n")
  
  require("knitr", quietly = TRUE)
  
  foo <- function(token, insert){
    collapse(sprintf(token, insert))
  }
  
  project_root <- file.path(tempdir(), "checkpoint-test-temp")
  dir.create(project_root, showWarnings = FALSE)
  
  code <- collapse(
    foo("library(%s)", letters[1:2]),
    foo("require(%s)", letters[3:4]),
    foo("%s::foo()", letters[5:6]),
    foo("%s:::bar()", letters[7:8]),
    "track <- setClass('track', slots = c(x='numeric', y='numeric'))"
  )
  
  it("finds packages in R scripts", {
    # Write dummy code file to project
    
    codefile <- file.path(project_root, "code.R")
    cat(code, file = codefile)
    
    found <- checkpoint:::projectScanPackages(project = project_root)
    expect_equal(found$pkgs, sort(c(letters[1:8], "methods")))
    
    file.remove(codefile)
    
  })
  
  it("finds packages in Rmarkdown", {
    
    # Write dummy knitr code file to project
    knit <- sprintf("```{r}\n%s\n```", code)
    knitfile <- file.path(project_root, "knit.Rmd")
    cat(knit, file = knitfile)
    
    found <- checkpoint:::projectScanPackages(project = project_root, use.knitr = TRUE)
    expect_equal(found$pkgs, sort(c(letters[1:8], "methods")))
    file.remove(knitfile)
    
    unlink(project_root)
  })
  
  
})
