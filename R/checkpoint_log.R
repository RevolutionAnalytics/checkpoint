checkpoint_log <- function(log, snapshotDate, pkg, file = NULL){
  extract_bytes <- function(log){
    ptn <- "(Content type .* length )(\\d+).*"
    gsub(ptn, "\\2", log[grep(ptn, log)])
  }
  
  extract_pkgs <- function(log){
    ptn <- "(also installing the dependencies )(.*).*"
    z <- gsub(ptn, "\\2", log[grep(ptn, log)])
    if(length(z) == 0) return(z)
    z <- gsub("[‘’'\"]*", "", z, useBytes = TRUE)
    strsplit(z, ", ")[[1]]
  }
  
  z <- data.frame(
    timestamp = Sys.time(),
    snapshotDate = snapshotDate,
    pkg = c(extract_pkgs(log), pkg),
    bytes = extract_bytes(log),
    stringsAsFactors = FALSE
  )
  if(is.null(file)){
    z
  } else {
    if(!file.exists(file)){
      suppressWarnings(
        write.table(z, file = file, 
                    append = FALSE, 
                    sep = ",", 
                    row.names = FALSE, 
                    col.names = TRUE) 
      )
    } else {
      suppressWarnings(
        write.table(z, file = file, 
                    append = TRUE, 
                    sep = ",", 
                    row.names = FALSE, 
                    col.names = FALSE) 
      )
    }
  }
}  