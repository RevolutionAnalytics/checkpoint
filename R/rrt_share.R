#' Share an RRT repository
#'
#' @export
#' @param repo An RRT repository
#' @param to How to share. One of zip, tar, github, gist, bitbucket, or email.
#' @param output Output. This can be one of a file name, directory, or X.
#' @param ... Further args passed on to each internal method. See the examples and Details.
#'
#' @details
#'
#' Note that with \code{to='email'} we can't attach the zip file for you, but we print out where
#' the attachment is so you can easily find it and attach it to the email quickly.
#'
#' @examples \dontrun{
#' rrt_share(to='zip', output='~/myrepo.zip')
#' rrt_share(to='tar', output='~/myrepo.tar.gz')
#' rrt_share(to='gist')
#' rrt_share(to='github')
#' rrt_share(to='email')
#' }

rrt_share <- function(repo=getwd(), to="zip", output=NULL, address = NULL, ...)
{
  tt <- list.files(repo, full.names = TRUE)
  names(tt) <- list.files(repo)
  tt <- unname(tt[!names(tt) %in% 'rrt'])
  toinclude <- c(tt, file.path(repo, "rrt/rrt_manifest.yml"))
  switch(to,
         zip = do_zip(outfile=output, include=toinclude),
         tar = do_tar(outfile=output, include=toinclude),
         github = do_github(),
         gist = do_gist(toinclude, ...),
         email = do_email(address, repo, toinclude, ...))
}

do_zip <- function(outfile=NULL, include, ...){
  if(!syscheck("zip")) stop("You don't have ability to create a zip file")
  setwd(repo)
  if(is.null(outfile))
    outfile <- file.path(Sys.getenv("HOME"), sprintf("%s.zip", strsplit(repo, "/")[[1]][length(strsplit(repo, "/")[[1]])]))
  zip(zipfile = outfile, files = include, ...)
}

do_tar <- function(outfile, include, ...){
  if(!syscheck("tar")) stop("You don't have ability to create a tar file")
#   setwd(repo)
#   z <- gzfile(outfile)
#   tar(tarfile = z, files = include, ...)
  message("\nnot working yet")
}

do_gist <- function(include, ...){
  gist_create(files = include, ...)
}

do_github <- function(repo){
  if(!suppressWarnings(require("git2r", quietly = TRUE)))
    stop("You need to install git2r - You can always use git in your terminal :)", call. = FALSE)
  gg <- tryCatch(repository(repo), error = function(e) e)
  if("error" %in% class(gg)){
    message("\n  Please initialize git in your RRT repo first.  See ?rrt_git")
  }
  message("  function not done yet...")
#   add(gg)
}

####### Email functions
####### Nearly directly from devtools, but are only avail via :::, so included here
do_email <- function(address, repo, include, subject="Sharing my RRT repository", body="Hi!\n\nAttached is some code I've been working on. Just download the file, uncompress it, install the RRT package (devtools::install_github('RevolutionAnalytics/RRT')), and run the code.\n\nCheers,\n<your name here>", ...)
{
  if(is.null(address)) stop("You must supply an email address")
  filename <- file.path(Sys.getenv("HOME"), sprintf("%s.zip", strsplit(repo, "/")[[1]][length(strsplit(repo, "/")[[1]])]))
#   filename <- file.path(tmp, paste0(repo, ".zip"))
  file <- do_zip(include=include)
  url <- paste("mailto:", URLencode(address), "?subject=",
               URLencode(subject), "&body=", URLencode(body), sep = "")
  message(sprintf("Attach your zip file to the email from: %s", filename))
  tryCatch({
    browseURL(url, browser = email_browser())
  }, error = function(e) {
    message("Sending failed with error: ", e$message)
    cat("To: ", address, "\n", sep = "")
    cat("Subject: ", subject, "\n", sep = "")
    cat("\n")
    cat(body, "\n", sep = "")
  })
  invisible(TRUE)
}

email_browser <- function(){
  if (!identical(.Platform$GUI, "RStudio"))
    return(getOption("browser"))
  if (.Platform$OS.type == "windows")
    return(NULL)
  browser <- Sys.which(c("xdg-open", "open"))
  browser[nchar(browser) > 0][[1]]
}

syscheck <- function(x) if(nchar(Sys.which(x)[[1]]) > 0) TRUE else FALSE

####### gist functions
gist_create <- function(files, description = "", public = TRUE, verbose=TRUE,
  browse=TRUE, callopts=list())
{
  dat <- create_gist(files, description = description, public = public)
  credentials <- get_credentials()
  headers <- add_headers(`User-Agent` = "Dummy", `Accept` = 'application/vnd.github.v3+json')
  auth  <- authenticate(getOption("github.username"), getOption("github.password"), type = "basic")
  response <- POST(url = "https://api.github.com/gists", body = dat, config = c(auth, headers), callopts)
  warn_for_status(response)
  assert_that(response$headers$`content-type` == 'application/json; charset=utf-8')
  html_url <- content(response)$html_url
  gisturl <- paste("https://gist.github.com/", getOption("github.username"), "/",
                   basename(html_url), sep = "")
  message("Your gist has been published!")
  message("View gist at ", gisturl)
  browseURL(gisturl)
  return( gisturl )
}

create_gist <- function(filenames, description = "", public = TRUE) {
  files <- lapply(filenames, function(file) {
    x <- list(content = paste(readLines(file, warn = FALSE), collapse = "\n"))
  })
  names(files) <- basename(filenames)
  body <- list(description = description, public = public, files = files)
  RJSONIO::toJSON(body)
}

get_credentials <- function() {
  if (is.null(getOption("github.username"))) {
    username <- readline("Please enter your github username: ")
    if(nchar(username) == 0){
      stop("Authentication failed - you can't have a blank username")
    }
    options(github.username = username)
  }
  if (is.null(getOption("github.password"))) {
    password <- readline("Please enter your github password: ")
    if(nchar(password) == 0){
      stop("Authentication failed - you can't have a blank password")
    }
    options(github.password = password)
  }
}
