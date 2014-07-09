#' Create a Github repository
#'
#' @import httr RJSONIO
#' @export
#'
#' @param name (character) Required. The name of the repository
#' @param description	(character)	A short description of the repository
#' @param homepage (character)	A URL with more information about the repository
#' @param private	(boolean)	Either true to create a private repository, or false to create a public
#' one. Creating private repositories requires a paid GitHub account. Default: false
#' @param has_issues (boolean)	Either true to enable issues for this repository, false to disable
#' them. Default: true
#' @param has_wiki (boolean)	Either true to enable the wiki for this repository, false to disable
#' it. Default: true
#' @param has_downloads	(boolean)	Either true to enable downloads for this repository, false to
#' disable them. Default: true
#' @param team_id	(numberic)	The id of the team that will be granted access to this repository.
#' This is only valid when creating a repository in an organization.
#' @param auto_init	(boolean)	Pass true to create an initial commit with empty README. Default:
#' false
#' @param gitignore_template	(character)	Desired language or platform .gitignore template to
#' apply. Use the name of the template without the extension. For example, 'Haskell'. Ignored
#' if the auto_init parameter is not provided.
#' @param license_template	(character)	Desired LICENSE template to apply. Use the name of the
#' template without the extension. For example, 'mit' or 'mozilla'. Ignored if the auto_init
#' parameter is not provided.
#' @param browse (logical) If TRUE, at the end of the function the new repo is opened in your
#' default browser
#' @param ... Further curl options passed on to httr::POST
#'
#' @examples \dontrun{
#' rrt_github(name="yeppers_peppers", description='some comments')
#' }

rrt_github <- function(name, description = "", homepage = "", private = FALSE,
  has_issues=TRUE, has_wiki=TRUE, has_downloads=TRUE, team_id=NULL, auto_init=FALSE,
  gitignore_template = NULL, license_template = NULL, browse=TRUE, ...)
{
  get_credentials("github")
  headers <- add_headers(`User-Agent` = "Dummy", `Accept` = 'application/vnd.github.v3+json')
  auth  <- authenticate(Sys.getenv("GITHUB_USERNAME"), Sys.getenv("GITHUB_PASSWORD"), type = "basic")
  args <- rrt_compact(list(name=name, description=description, homepage=homepage, private=private,
                       has_issues=has_issues, has_wiki=has_wiki, has_downloads=has_downloads,
                       team_id=team_id, auto_init=auto_init, gitignore_template=gitignore_template,
                       license_template=license_template))
  response <- POST(url = "https://api.github.com/user/repos", body = RJSONIO::toJSON(args), config = c(auth, headers), ...)
  warn_for_status(response)
  if(response$headers$`content-type` == 'application/json; charset=utf-8') stop("Content-type does not equal 'application/json; charset=utf-8'")
  html_url <- content(response)$html_url
  githubrepourl <- paste("https://github.com/", Sys.getenv("GITHUB_USERNAME"), "/",
                   basename(html_url), sep = "")
  message("Your Github repo has been created")
  message("View repo at ", githubrepourl)
  if(browse) browseURL(githubrepourl)
}

#' Create Bitbucket repository
#'
#' @import httr RJSONIO
#' @export
#'
#' @param name Required. Name of the repository
#' @param description A string containing the repository's description.
#' @param private A boolean indicating if a repository is private or public.
#' @param fork_policy Control the rules for forking this repository. See Details
#' @param has_issues A boolean indicating a repository has an issue tracker.
#' @param has_wiki A boolean indicating if the repository has a wiki.
#' @param website Any website to go in the repository metadata
#' @param browse (logical) If TRUE, at the end of the function the new repo is opened in your
#' default browser
#' @param ... Further curl options passed on to httr::POST
#'
#' @details Available values for fork_policy parameter are:
#' \itemize{
#'  \item allow_forks: unrestricted forking
#'  \item no_public_forks: restrict forking to private forks (forks cannot be made public later)
#'  \item no_forks: deny all forking
#' }
#'
#' @examples \dontrun{
#' rrt_bitbucket(name="thingsandstuff", description="some comments")
#' }

rrt_bitbucket <- function(name, description = "", private = FALSE, fork_policy = "allow_forks",
  has_issues=TRUE, has_wiki=TRUE, website="", browse=TRUE, ...)
{
  get_credentials("bitbucket")
  auth  <- authenticate(Sys.getenv("BITBUCKET_USERNAME"), Sys.getenv("BITBUCKET_PASSWORD"), type = "basic")
  args <- rrt_compact(list(name=name, description=description, is_private=private, scm="git",
            fork_policy=fork_policy, has_issues=has_issues, has_wiki=has_wiki, website=website))
  url2 <- file.path("https://bitbucket.org/api/2.0/repositories", Sys.getenv("BITBUCKET_USERNAME"), name)
  response <- POST(url = url2, body = RJSONIO::toJSON(args), config = auth)
  if(response$status_code > 202){ stop(sprintf("%s - %s", response$status_code, content(response)$error$message), call. = FALSE) } else {
    if(response$headers$`content-type` == 'application/json; charset=utf-8') stop("Content-type does not equal 'application/json; charset=utf-8'")
    html_url <- file.path("https://bitbucket.org", Sys.getenv("BITBUCKET_USERNAME"), name)
    message("Your Github repo has been created")
    message("View repo at ", html_url)
    if(browse) browseURL(html_url)
  }
}

get_credentials <- function(what="github") {
  what <- match.arg(what, c("github","bitbucket"))
  if(what=="github"){
    if (is.null(Sys.getenv("GITHUB_USERNAME"))) {
      username <- readline("Please enter your github username: ")
      if(nchar(username) == 0){
        stop("Authentication failed - you can't have a blank github username")
      }
      Sys.setenv(GITHUB_USERNAME = username)
    }
    if (is.null(Sys.getenv("GITHUB_PASSWORD"))) {
      password <- readline("Please enter your github password: ")
      if(nchar(password) == 0){
        stop("Authentication failed - you can't have a blank github password")
      }
      Sys.setenv(GITHUB_PASSWORD = password)
    }
  } else {
    if (is.null(Sys.getenv("BITBUCKET_USERNAME"))) {
      username <- readline("Please enter your bitbucket username: ")
      if(nchar(username) == 0){
        stop("Authentication failed - you can't have a blank bitbucket username")
      }
      Sys.setenv(BITBUCKET_USERNAME = username)
    }
    if (is.null(Sys.getenv("BITBUCKET_PASSWORD"))) {
      password <- readline("Please enter your bitbucket password: ")
      if(nchar(password) == 0){
        stop("Authentication failed - you can't have a blank bitbucket password")
      }
      Sys.setenv(BITBUCKET_PASSWORD = password)
    }
  }
}
