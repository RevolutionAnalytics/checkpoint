#' Create a Github repo
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
#' apply. Use the name of the template without the extension. For example, “Haskell”. Ignored 
#' if the auto_init parameter is not provided.
#' @param license_template	(character)	Desired LICENSE template to apply. Use the name of the 
#' template without the extension. For example, “mit” or “mozilla”. Ignored if the auto_init 
#' parameter is not provided.
#' 
#' @examples \dontrun{
#' rrt_github(name="yeppers_peppers", description='some comments')
#' rrt_bitbucket(name="thingsandstuff", description="some comments")
#' }

rrt_github <- function(name, description = "", homepage = "", private = FALSE, 
  has_issues=TRUE, has_wiki=TRUE, has_downloads=TRUE, team_id=NULL, auto_init=FALSE, 
  gitignore_template = NULL, license_template = NULL, verbose=TRUE, browse=TRUE, ...)
{
  get_credentials("github")
  headers <- add_headers(`User-Agent` = "Dummy", `Accept` = 'application/vnd.github.v3+json')
  auth  <- authenticate(getOption("github.username"), getOption("github.password"), type = "basic")
  args <- compact(list(name=name, description=description, homepage=homepage, private=private,
                       has_issues=has_issues, has_wiki=has_wiki, has_downloads=has_downloads, 
                       team_id=team_id, auto_init=auto_init, gitignore_template=gitignore_template, 
                       license_template=license_template))
  response <- POST(url = "https://api.github.com/user/repos", body = RJSONIO::toJSON(args), config = c(auth, headers), ...)
  warn_for_status(response)
  assert_that(response$headers$`content-type` == 'application/json; charset=utf-8')
  html_url <- content(response)$html_url
  githubrepourl <- paste("https://github.com/", getOption("github.username"), "/", 
                   basename(html_url), sep = "")
  message("Your Github repo has been created")
  message("View repo at ", githubrepourl)
  browseURL(githubrepourl)
}

#' Create Bitbucket repository
#' 
#' @import httr RJSONIO
#' @export
#' 
#' @param scm The source control manager for the repository. This is either hg or git.
#' @param has_wiki A boolean indicating if the repository has a wiki.
#' @param description A string containing the repository's description.
#' @param links An array of related objects.
#' @param updated_on A date timestamp of the last update to this repository.
#' @param fork_policy Control the rules for forking this repository. See Details
#' @param created_on An ISO-8601 date timestamp of this repository's creation date.
#' @param owner The owner's account.
#' @param size The size of the repository in bytes.
#' @param parent The parent repository this repository was forked off (only present on forks). This is a repository object itself.
#' @param has_issues A boolean indicating a repository has an issue tracker.
#' @param is_private A boolean indicating if a repository is private or public.
#' @param full_name The unique key into the repository. This key has the format: {owner}/{repo_slug}
#' @param name The display name of the repository.
#' @param language The main (programming) language of the repository source files.
#' 
#' @details Available values for fork_policy parameter are:
#' \itemize{
#' \item allow_forks: unrestricted forking
#' \item no_public_forks: restrict forking to private forks (forks cannot be made public later)
#' \item no_forks: deny all forking
#' }
#' 
#' @rdname rrt_github
rrt_bitbucket <- function(name, description = "", private = FALSE, fork_policy = "allow_forks",
  has_issues=TRUE, has_wiki=TRUE, website="", verbose=TRUE, browse=TRUE, ...)
{
  get_credentials("bitbucket")
  auth  <- authenticate(getOption("bitbucket.username"), getOption("bitbucket.password"), type = "basic")
  args <- compact(list(name=name, description=description, is_private=private, scm="git",
            fork_policy=fork_policy, has_issues=has_issues, has_wiki=has_wiki, website=website))
  url2 <- file.path("https://bitbucket.org/api/2.0/repositories", getOption("bitbucket.username"), name)
  response <- POST(url = url2, body = RJSONIO::toJSON(args), config = auth)
  if(response$status_code > 202){ stop(sprintf("%s - %s", response$status_code, content(response)$error$message), call. = FALSE) } else {
    assert_that(response$headers$`content-type` == 'application/json; charset=utf-8')
    html_url <- file.path("https://bitbucket.org", getOption("bitbucket.username"), name)
    message("Your Github repo has been created")
    message("View repo at ", html_url)
   browseURL(html_url)
  }
}

get_credentials <- function(what="github") {
  what <- match.arg(what, c("github","bitbucket"))
  if(what=="github"){
    if (is.null(getOption("github.username"))) {
      username <- readline("Please enter your github username: ")
      if(nchar(username) == 0){
        stop("Authentication failed - you can't have a blank github username")
      }
      options(github.username = username)
    }
    if (is.null(getOption("github.password"))) {
      password <- readline("Please enter your github password: ")
      if(nchar(password) == 0){
        stop("Authentication failed - you can't have a blank github password")
      }
      options(github.password = password)
    }
  } else {
    if (is.null(getOption("bitbucket.username"))) {
      username <- readline("Please enter your bitbucket username: ")
      if(nchar(username) == 0){
        stop("Authentication failed - you can't have a blank bitbucket username")
      }
      options(bitbucket.username = username)
    }
    if (is.null(getOption("bitbucket.password"))) {
      password <- readline("Please enter your bitbucket password: ")
      if(nchar(password) == 0){
        stop("Authentication failed - you can't have a blank bitbucket password")
      }
      options(bitbucket.password = password)
    }
  }
}