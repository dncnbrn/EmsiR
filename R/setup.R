#' Set login details for the Episteme API.
#'
#' @description  Sets login details for Emsi Episteme as environment variables which will be used to authenticate all queries to the API.
#'
#' @param username is the episteme-username variable for Episteme.
#' @param password is the episteme-secret1 variable for Episteme.
#' @return The variables are placed within the environment for use by other functions.
#' @export
EpistemeLogin <- function(username, password) {
    Sys.setenv(EMSIUN = username, EMSIPW = password)
}

#' Background function which is used to take Emsi Episteme login details set by \code{\link{EpistemeLogin}} and prepares the header for all queries to the API.
EpistemeSettings <- function() {
    c(`episteme-username` = Sys.getenv("EMSIUN"), `episteme-secret1` = Sys.getenv("EMSIPW"), `episteme-pub` = "512", `content-type` = "application/json")
}
