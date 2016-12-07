#' Query available dataset on the Episteme API
#'
#' @description Queries the Episteme API regarding available datasets and returns them in an organised list.
#'
#' @return A full listing of datasets with description by Country, Content and Release, to allow for filtering.
#' @export
dataset_list <- function() {
    datasets <- httr::POST("http://episteme.economicmodeling.com/meta", body = NULL, encode = "json", httr::add_headers(EpistemeSettings()))
    rawdata <- jsonlite::fromJSON(httr::content(datasets, "text"), simplifyVector = FALSE)
    datasets <- rawdata[[1]] %>% purrr::transpose()
    name <- unlist(datasets[[1]])
    versions <- as.numeric(purrr::map_chr(datasets[[2]], length))
    datasets <- data.frame(dataset = c(rep(name, versions)), version = c(unlist(datasets[[2]])))
    final <- datasets %>% dplyr::tbl_df() %>%
      dplyr::mutate(Dataset = as.character(dataset), Version = as.character(version), Country = substr(dataset,
        6, 7), Content = substr(dataset, 9, nchar(Dataset)), Identifier = paste(dataset, "/", version, sep = "")) %>%
      dplyr::arrange(Country) %>%
      dplyr::select(-dataset, -version, -Dataset, Country, Content, Version, Identifier) %>%
      dplyr::group_by(Country, Content)
    return(final)
}

#' Query concepts used on the Episteme API
#'
#' @description Queries the Episteme API regarding concepts used to structure data and returns them in an organised list.
#'
#' @return A full listing of concepts with description by Country and Concept.
#' @export
concept_list <- function() {
    datasets <- httr::POST("http://episteme.economicmodeling.com/meta", body = NULL, encode = "json", httr::add_headers(EpistemeSettings()))
    rawdata <- jsonlite::fromJSON(httr::content(datasets, "text"), simplifyVector = FALSE)
    concepts <- rawdata[[2]] %>% purrr::transpose()
    concepts <- unlist(concepts) %>% dplyr::tbl_df
    concepts %>% dplyr::tbl_df() %>% purrr::transmute(Identifier = value) %>% dplyr::mutate(Country = substr(Identifier, 6, 7),
        Content = substr(Identifier, 9, nchar(Identifier))) %>% dplyr::select(Country, Content, Identifier)
}

#' Query the details of an Episteme API dataset
#'
#' @description Identify the available metrics and required dimensions for a dataset held on Emsi Episteme.
#'
#' @param country The two-digit country identifier for the dataset.
#' @param content The keyword identifier for the content of the dataset (e.g. \code{"Occupation"}, \code{"Industry"}).
#' @param release The release or version identifier for the dataset (e.g. \code{"2016.1"}).
#' @return A list with two elements: \code{Metrics} lists the Metrics available within the dataset and \code{Dimensions}
#' lists the dimensions requiring constraint when making a request for data. Note that \code{Metrics}
#'  does not include Location Quotients and Shift-Share, which are derived from the metrics here.
#' @examples
#' dataset_detail("UK","Occupation","2016.1")
#' @export
dataset_detail <- function(country, content, release) {
    dataset <- paste("EMSI", country, content, sep = ".")
    URI <- paste("http://episteme.economicmodeling.com/meta/dataset", dataset, release, sep = "/")
    parameters <- httr::POST(URI, body = NULL, encode = "json", httr::add_headers(EpistemeSettings()))
    parameters <- jsonlite::fromJSON(httr::content(parameters, "text"), simplifyVector = FALSE)
    dimensions <- parameters[["dimensions"]] %>% unlist
    list(Metrics = as.list(parameters[["metrics"]] %>% unlist %>% dplyr::tbl_df() %>% dplyr::arrange(value)), Dimensions = as.list(dimensions[names(dimensions) %in%
        "name"] %>% dplyr::tbl_df()))
}

#' Query the details of an Episteme API concept
#'
#' @description Identify the parameters of concepts used to organise data within Emsi Episteme.
#'
#' @param country The two-digit country identifier for the concept.
#' @param content The keyword identifier for the content of the concept (e.g. \code{"Occupation"}, \code{"Industry"}).
#' @return A list of parameters (typically concept levels) associated with the concept.
#' @examples
#' concept_detail("UK", "SOC.2010")
#' @export
concept_detail <- function(country, concept) {
    concept <- paste("EMSI", country, concept, sep = ".")
    URI <- paste("http://episteme.economicmodeling.com/meta/concept", concept, sep = "/")
    parameters <- httr::POST(URI, body = NULL, encode = "json", httr::add_headers(EpistemeSettings()))
    parameters <- jsonlite::fromJSON(httr::content(parameters, "text"), simplifyVector = FALSE)
    conceptlevels <- unlist(parameters$levels)
    names(conceptlevels) <- NULL
    return(conceptlevels)
}
