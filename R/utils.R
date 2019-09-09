#' Check if the given ONS json is available
#' @inheritParams map
#' @return \code{TRUE} if the API is available, otherwise \code{stop()} is called.
ensure_ons_api_available <- function(ons_api) {
        code <- FALSE
        try({
                code <- httr::status_code(httr::GET(ons_api))
        }, silent = TRUE)

        if (code == 200) return(TRUE)

        errtext <- paste('The ons_api provided is currently unavailable')
        stop(errtext, call. = FALSE)
}
