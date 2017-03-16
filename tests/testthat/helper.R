is_online <- function(){
  u <- tryCatch(url("https://mran.microsoft.com"),
                error = function(e)e)
  if(inherits(u, "error")){
    u <- url("http://mran.microsoft.com")
  }
  on.exit(close(u))
  z <- tryCatch(suppressWarnings(readLines(u, n = 1, warn = FALSE)),
                error = function(e)e)
  !inherits(z, "error")
}

skip_if_offline <- function(){
  if(!is_online()) testthat::skip("Offline. Skipping test.")
}
