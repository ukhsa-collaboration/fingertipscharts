#' Check if the given ONS json is available
#' @param endpoint string, the json to check
#' @return \code{TRUE} if the API is available, otherwise \code{stop()} is called.
ensure_ons_api_available <- function(ons_api) {
        code <- FALSE
        try({
                code <- httr::status_code(httr::GET(ons_api))
        }, silent = TRUE)

        if (code == 200) return(TRUE)

        errtext <- paste('The ons_api provided is currently unavailable:')
        if (code != FALSE) errtext <- paste(errtext, 'HTTP code', code)
        stop(paste(errtext, collapse='\n  '), call. = FALSE)
}
