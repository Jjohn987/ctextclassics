
#' @export FALSE
ctext_api <- function(path) {
  url <- httr::modify_url("https://api.ctext.org/", path = path)
  ok <- FALSE
  counter <- 0

  while(ok == FALSE & counter <= 5) {
    resp <- httr::GET(url)
    counter <- counter + 1

    if(httr::http_type(resp) != "application/json" | httr::http_error(resp) == TRUE) {
      stop("API did not return json", call. = FALSE)
      parsed <- list(NA, NA)
      Sys.sleep(2)
    } else {
      ok <- TRUE
      parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE, flatten = TRUE)
    }
  }
  return(parsed)
}


