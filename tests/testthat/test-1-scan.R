library("testthat")
context("scan for library, require, :: and :::")

describe("scanRepoPackages finds dependencies", {
  
  
  collapse <- function(...) paste(..., sep="\n", collapse="\n")
  
  
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
    
    found <- projectScanPackages(project = project_root)
    expect_equal(found$pkgs, sort(c(letters[1:8], "methods")))
    
    file.remove(codefile)
    
  })
  
  it("finds packages in Rmarkdown", {
    if(!suppressWarnings(require("knitr", quietly = TRUE))){
      skip("knitr not available")
    }
    require("knitr", quietly = TRUE)
    
    # Write dummy knitr code file to project
    knit <- sprintf("```{r}\n%s\n```", code)
    knitfile <- file.path(project_root, "knit.Rmd")
    cat(knit, file = knitfile)
    
    found <- projectScanPackages(project = project_root, use.knitr = TRUE)
    expect_equal(found$pkgs, c(letters[1:8], "methods"))
    file.remove(knitfile)
    
    unlink(project_root)
  })
  
  it("auto-installs knitr and rmardown", {
    if(!suppressWarnings(require("knitr", quietly = TRUE))){
      skip("knitr not available")
    }
    require("knitr", quietly = TRUE)
    
    # Write dummy knitr code file to project
    knit <- sprintf("```{r}\n%s\n```", code)
    knitfile <- file.path(project_root, "knit.Rmd")
    cat(knit, file = knitfile)
    
    found <- projectScanPackages(project = project_root, use.knitr = TRUE, auto.install.knitr = TRUE)
    expect_equal(found$pkgs, c(letters[1:8], "methods", "knitr", "rmarkdown"))
    file.remove(knitfile)
    
    unlink(project_root)
  })
  
})
