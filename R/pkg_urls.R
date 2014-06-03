#' Get package URIs given package name and version - for archived packages.
#'
#' @export
#' @param pkgs A data.frame with two columns, or a list or vector with structure \code{pkgname_ver}
#' @return A vector of urls of length equal to rows in the input data.frame
#' @examples
#' (dat <- data.frame(pkg=c('plyr','reshape2','shiny'), ver=c('1.6.0','1.2.1','0.8.0')))
#' pkgurls(pkgs=dat)
#' pkgurls(pkgs=c('plyr_1.6.0','reshape2_1.2.1','shiny_0.8.0'))

pkgurls <- function(pkgs)
{
  if(!any(is(pkgs) %in% c('data.frame','vector','list')))
    stop("input must be one of data.frame, vector, or lis")
  
  type <- class(pkgs)
  switch(type, 
         data.frame = geturlsdf(pkgs),
         character = geturlsdf(data.frame(do.call(rbind, strsplit(x, "_")))))
}

geturlsdf <- function(x){
  # force column names
  names(x) <- c('pkg','ver')
  make_url <- function(x){
    sprintf('http://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz', x['pkg'], x['pkg'], x['ver'])
  }
  vapply(apply(x, 1, as.list), make_url, "")
}

### NOTE: this will benefit from the metadata store of CRAN repos b/c e.g., the plyr url created in the 
### example is not correct, should be ...plyr_1.6.tar.gz, without the 0 at the end. With the metadata, we
### could regex this and fix them with a warning to user perhaps. 