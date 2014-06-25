#' Find R packages on Github
#' 
#' @import httr jsonlite
#' @param x
#' @param ... Curl options passed on to httr::GET
#' 
#' @return a vector of owner/repo character strings
#' 
#' @examples \dontrun{
#' github_r()
#' }

github_r <- function(sort=NULL, order=NULL, per_page=10, ...){
  url <- "https://api.github.com/search/repositories"
  args <- rrt_compact(list(q = "language:r", sort = sort, order = order, per_page = per_page))
  res <- GET(url, query=args, config=c(gh_auth(), ...))
  stop_for_status(res)
  tt <- content(res, as = "text")
  tmp <- jsonlite::fromJSON(tt, FALSE)
  vapply(tmp$items, "[[", "", "full_name")
}

github_is_r <- function(...){
  tmp <- github_r(...)
  out <- list()
  for(i in seq_along(tmp)){
    logs <- c(gh_has_desc(tmp[[i]]), gh_has_namespace(tmp[[i]]), gh_has_r_dir(tmp[[i]]))
    out[[i]] <- if(all(logs)) TRUE else FALSE
  }
  names(out) <- tmp
  out
}

gh_check_file <- function(x, file){
  url <- "https://api.github.com/repos/%s/%s/contents/%s"
  url <- makeurl(url, x, file)
  res <- GET(url, config=gh_auth())
  if(res$headers$statusmessage == "OK") TRUE else FALSE
}

gh_has_desc <- function(userrepo){
  gh_check_file(userrepo, "DESCRIPTION")
}

gh_has_namespace <- function(userrepo){
  gh_check_file(userrepo, "NAMESPACE")
}

gh_has_r_dir <- function(userrepo){
  gh_check_file(userrepo, "R")
}

makeurl <- function(url, x, file){
  ur <- strsplit(x, "/")[[1]]
  sprintf(url, ur[[1]], ur[[2]], file)
}

gh_auth <- function(){
  headers <- add_headers(`User-Agent` = "Dummy", `Accept` = 'application/vnd.github.v3+json')
  auth  <- authenticate(getOption("github.username"), getOption("github.password"), type = "basic")
  c(auth, headers)
}

rrt_compact <- function (l) Filter(Negate(is.null), l)