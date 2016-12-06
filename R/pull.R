pullquery <- function(country, content, release, constraints, metrics) {
    emsi_URL <- paste("http://episteme.economicmodeling.com/EMSI.", country, ".", content, "/",
        release, sep = "")
    body <- list(metrics = metrics, constraints = constraints)
    r <- httr::POST(emsi_URL, body = body, encode = "json", httr::add_headers(EpistemeSettings()))
    r2 <- jsonlite::fromJSON(httr::content(r, "text"), simplifyDataFrame = TRUE)
    outputdata <- as.data.frame(r2$data$rows) %>% dplyr::tbl_df()
    colnames(outputdata) <- r2$data$name
    rm(r)
    rm(r2)
    outputdata[, colnames(outputdata) %in% names(constraints)] <- apply(outputdata[, colnames(outputdata) %in%
        names(constraints)], 2, function(x) factor(x))
    outputdata[, colnames(outputdata) %in% metrics$as] <- apply(outputdata[, colnames(outputdata) %in%
        metrics$as], 2, function(x) as.numeric(x))
    return(outputdata)
}

datapull <- function(country, content, release, constraints, metrics) {
    if (ncol(metrics) == 2) {
        outputdata <- pullquery(country, content, release, constraints, metrics)
    }
    if (ncol(metrics) == 3) {
        if (nrow(metrics[is.na(metrics$operation), ]) == 0) {
            outputdata <- pullquery(country, content, release, constraints, metrics)
        }
        if (nrow(metrics[is.na(metrics$operation), ]) > 0) {
            metrics1 <- metrics %>% dplyr::filter(is.na(operation)) %>% dplyr::select(-operation)
            metrics2 <- metrics %>% dplyr::filter(!is.na(operation))
            outputdata1 <- pullquery(country, content, release, constraints, metrics1)
            outputdata2 <- pullquery(country, content, release, constraints, metrics2)
            outputdata <- outputdata1 %>% dplyr::left_join(outputdata2)
            rm(outputdata1, outputdata2)
        }
    }
    return(outputdata)
}
