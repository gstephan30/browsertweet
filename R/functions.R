#' Setup the API key with Renvrion file in base or R folder
#' 
#' @param secret Sting of secret to export.
#' 
#' @return Outputs secret token from environment file
#' @examples 
#' setup_keys()
setup_keys <- function(secret){
  if(file.exists(".Renviron")){
    message("Keys found in current folder")
    readRenviron(".Renviron")
  } else if (file.exists("../.Renviron")) {
    message(print("Keys found in base folder"))
    readRenviron("../.Renviron")
  } else {
    stop("Need to setup api secret tokens")
  }
  
  if (nchar(Sys.getenv(secret)) > 0) {
    output <- Sys.getenv(secret)
  } else {
    stop("Missing credentials")
  }
  
  return(output)
}

#' Check if a local data base file exists
#' if not, create a new one
#' 
#' @examples 
#' db <- check_db()
check_db <- function() {
  if (!file.exists("data/db.rds")) {
    print("no data found")
    db <- tibble()
    saveRDS(db, file = "data/db.rds")
    db <- readRDS("data/db.rds")
  } else {
    print("loading data")
    db <- readRDS("data/db.rds")
  }
}

