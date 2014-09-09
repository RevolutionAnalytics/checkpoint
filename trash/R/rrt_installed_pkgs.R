#' Check what packages are installed.
#' 
#' @export
#' 
#' @param repo A repository path. This is the path to the root of your RRT repository. By default, we use the current working directory via \code{getwd()}.
#' @param simplify (logical) Whether to simplify to just package name and version or print entire
#' matrix of package information.
#' @family rrt
#' @examples \dontrun{
#' rrt_installed_pkgs()
#' rrt_installed_pkgs(simplify=FALSE)
#' }

rrt_installed_pkgs <- function(repo=getwd(), simplify=TRUE)
{
  if(is_rrt(repo, verbose = FALSE)){
    lib <- rrtPath(repo, "lib")
    tmp <- installed.packages(lib.loc = lib)
    if(simplify){
      if(!NROW(tmp) == 0){
        df <- tmp[,c("Package",'Version')]
        row.names(df) <- NULL
        data.frame(df, stringsAsFactors = FALSE)
      }
    }
    else { tmp }
  } else {
    message("This is not an RRT repository :(")
  }
}