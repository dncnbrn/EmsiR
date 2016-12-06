dataset_list <- function() {
    datasets <- httr:POST("http://episteme.economicmodeling.com/meta", body = NULL, encode = "json",
       httr::add_headers(EpistemeSettings()))
    rawdata <- jsonlite::fromJSON(httr::content(datasets, "text"), simplifyVector = FALSE)
    datasets <- rawdata[[1]] %>% purrr::transpose()
    name <- unlist(datasets[[1]])
    versions <- as.numeric(purrr::map_chr(datasets[[2]], length))
    datasets <- data.frame(dataset = c(rep(name, versions)), version = c(unlist(datasets[[2]])))
    datasets <- dplyr::tbl_df() %>% dplyr::mutate(Dataset = as.character(dataset), Version = as.character(version),
                              Country = substr(dataset, 6, 7), Content = substr(dataset, 9, nchar(Dataset)), Identifier = paste(dataset, "/", version, sep = "")) %>%
      dplyr::arrange(Country) %>%
      dplyr::select(-dataset, -version, -Dataset,
                              Country, Content, Version, Identifier) %>% dplyr::group_by(Country, Content)
    return(final)
}

concept_list <- function() {
    datasets <- httr::POST("http://episteme.economicmodeling.com/meta", body = NULL, encode = "json",
       httr::add_headers(EpistemeSettings()))
    rawdata <- jsonlite::fromJSON(httr::content(datasets, "text"), simplifyVector = FALSE)
    concepts <- rawdata[[2]] %>% purrr::transpose()
    concepts <- unlist(concepts) %>% dplyr::tbl_df
    concepts %>% dplyr::tbl_df() %>% purrr::transmute(Identifier = value) %>% dplyr::mutate(Country = substr(Identifier,
        6, 7), Content = substr(Identifier, 9, nchar(Identifier))) %>% dplyr::select(Country, Content,
        Identifier)
}

dataset_detail <- function(country, content, release) {
    dataset <- paste("EMSI", country, content, sep = ".")
    URI <- paste("http://episteme.economicmodeling.com/meta/dataset", dataset, release, sep = "/")
    parameters <- httr::POST(URI, body = NULL, encode = "json",httr::add_headers(EpistemeSettings()))
    parameters <- jsonlite::fromJSON(httr::content(parameters, "text"), simplifyVector = FALSE)
    dimensions <- parameters[["dimensions"]] %>% unlist
    list(Metrics = as.list(parameters[["metrics"]] %>% unlist %>% dplyr::tbl_df() %>% dplyr::arrange(value)),
        Dimensions = as.list(dimensions[names(dimensions) %in% "name"] %>% dplyr::tbl_df()))
}

concept_detail <- function(country, concept) {
    concept <- paste("EMSI", country, concept, sep = ".")
    URI <- paste("http://episteme.economicmodeling.com/meta/concept", concept, sep = "/")
    parameters <- httr::POST(URI, body = NULL, encode = "json",httr::add_headers(EpistemeSettings()))
    parameters <- jsonlite::fromJSON(httr::content(parameters, "text"), simplifyVector = FALSE)
    conceptlevels <- unlist(parameters$levels)
    names(conceptlevels) <- NULL
    return(conceptlevels)
}

