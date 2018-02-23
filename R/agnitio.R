#' @importFrom magrittr "%>%"
#' @export
agnitio_login <- function(username, password) {
  Sys.setenv(EMSIUN = username, EMSISECRET = password)
}

#' @importFrom magrittr "%>%"
#' @export
agnitio_token <- function() {
  raw_token <- httr::POST("https://auth.emsicloud.com/connect/token",
                          body=list('client_id'=Sys.getenv("EMSIUN"),
                                    'client_secret'=Sys.getenv("EMSISECRET"),
                                    'grant_type'="client_credentials",
                                    'scope'="emsiauth"),
                          httr::add_headers(`Content-Type`="application/x-www-form-urlencoded"),
                          encode="form"#,
                          # httr::verbose(data_out=TRUE, data_in = TRUE, info = TRUE, ssl=TRUE)
                          )
  proc_token <- jsonlite::fromJSON(httr::content(raw_token, "text",encoding="UTF-8"),
                                   simplifyVector = TRUE)
  Sys.setenv(EMSITOKEN=proc_token$access_token)
  Sys.setenv(EMSIEXPIRY=Sys.time()+proc_token$expires_in-120)
}

#' @importFrom magrittr "%>%"
#' @export
agnitio_settings <- function() {
  if(Sys.getenv("EMSITOKEN")=="") {
    agnitio_token()
    }
  if(as.numeric(Sys.getenv("EMSIEXPIRY"))<Sys.time()) {
    agnitio_token()
    }
  return(httr::add_headers(`Authorization`=paste("bearer",Sys.getenv("EMSITOKEN"),sep=" ")))
}

pullquery <- function(country, content, release, constraints, metrics) {
  emsi_URL <- paste("http://agnitio.emsicloud.com/emsi.", country, ".", content, "/", release, sep = "")
  body <- list(metrics = metrics, constraints = constraints)
  r <- httr::POST(emsi_URL, body = body, encode = "json", agnitio_settings())
  r2 <- jsonlite::fromJSON(httr::content(r, "text", encoding="UTF-8"), simplifyDataFrame = TRUE)
  outputdata <- as.data.frame(r2$data$rows) %>% dplyr::tbl_df()
  colnames(outputdata) <- r2$data$name
  rm(r)
  rm(r2)
  outputdata[, colnames(outputdata) %in% names(constraints)] <- apply(outputdata[, colnames(outputdata) %in% names(constraints)],
                                                                      2, function(x) factor(x))
  if(ncol(outputdata[, colnames(outputdata) %in% metrics$as])>1) {
    outputdata[, colnames(outputdata) %in% metrics$as] <- apply(outputdata[, colnames(outputdata) %in% metrics$as], 2, function(x) as.numeric(x))
  }
  return(outputdata)
}

datapull <- function(country, content, release, constraints, metrics) {
  if (ncol(metrics) == 2) {
    outputdata <- pullquery(country, content, release, constraints, metrics)
  }
  if (ncol(metrics) > 2) {
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

dataset_list <- function() {
  datasets <- httr::GET("http://agnitio.emsicloud.com/meta", body = NULL, encode = "json", agnitio_settings())
  rawdata <- jsonlite::fromJSON(httr::content(datasets, "text", encoding="UTF-8"), simplifyVector = FALSE)
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

dataset_detail <- function(country, content, release) {
  dataset <- paste("emsi", tolower(country), tolower(content), sep = ".")
  URI <- paste("http://agnitio.emsicloud.com/meta/dataset", dataset, release, sep = "/")
  parameters <- httr::GET(URI, body = NULL, encode = "json", agnitio_settings())
  parameters <- jsonlite::fromJSON(httr::content(parameters, "text", encoding="UTF-8"), simplifyDataFrame = TRUE)
  parameters[c("dimensions","metrics")]
}

metseq <- function(varname, from, to, separator) {
  gap <- to-from
  paste(rep(varname, gap), seq(from, to, by=1), sep=separator)
}

#' Specify metrics for an Episteme API query
#'
#' @description Takes a data frame of required metrics and necessary supporting criteria and specifies them ready for an Emsi Episteme data pull.
#'
#' @param metricdf at minimum, a data frame with two columns: \code{name} sets out the desired names for the metrics and
#' \code{as} sets out the titles of the codes on Emsi Episteme. Where using derivative metrics (Openings, Location Quotients and
#'  Shift-Share), additional columns are required in the form of \code{metrics} to specify if they are \emph{"OP"}, \emph{"LQ"} or
#'  \emph{"SS"} and, for Openings and Shift-Share, a \code{base} column identifies the comparison metric for the year.
#' @param geoparent is required for derivative metrics, and is a geographical code identifing the parent geographical unit for analysis.
#' @param along is required for derivative metrics, and reflects the intended domain for analysis (e.g. "Industry" or "Occupation").
#' @return A prepared data frame which will be ready for inclusion in a data pull query.
#' @examples
#' met1 <- data.frame(names=c("Jobs.2016","Jobs.2022"), as=c("Jobs.2016","Jobs.2022"))
#' metricmaker(met1)
#' met2 <- data.frame(name=c("Jobs.2016","Jobs.2016","Jobs.2016"),as=c("Jobs16","LQ16","SS16"),metrics=c(NA,"LQ","SS"),base=c(NA,NA,"Jobs.2003"))
#' metricmaker(met2, "GB", "Occupation")
#' @importFrom magrittr "%>%"
#' @export
metricmaker <- function(metricdf, geoparent, along) {
  if (("as" %in% colnames(metricdf))==FALSE) {
    metricdf$as <- metricdf$name
  }
  if (ncol(metricdf) == 2) {
    metricdf$metrics <- c(rep(NA, nrow(metricdf)))
  }
  if (ncol(metricdf) == 3 & nrow(metricdf[!is.na(metricdf$metrics), ]) == 0) {
    metrics <- metricdf
    metrics$metrics <- NULL
  }
  if (ncol(metricdf) >= 3 & nrow(metricdf[is.na(metricdf$metrics), ]) == 0) {
    metrics <- metricdf %>%
      dplyr::mutate(geoparent = geoparent, along = along) %>%
      dplyr::group_by(name, as) %>%
      dplyr::do(operation = metricalc(.$metrics, .$name,
                                      .$base, .$geoparent, .$along))
  }
  if (ncol(metricdf) >= 3 & nrow(metricdf[!is.na(metricdf$metrics), ]) > 0 & nrow(metricdf[is.na(metricdf$metrics), ]) > 0) {
    a <- metricdf %>% dplyr::filter(is.na(metrics)) %>% dplyr::select(name, as)
    a$operation <- list(NA)
    b <- metricdf %>% dplyr::filter(!is.na(metrics))
    b <- b %>%
      dplyr::mutate(geoparent = geoparent, along = along) %>%
      dplyr::group_by(name, as) %>%
      dplyr::do(operation = metricalc(.$metrics, .$name,
                                      .$base, .$geoparent, .$along))
    metrics <- dplyr::bind_rows(a, b)
    rm(a, b)
  }
  return(metrics)
}

dimmaker <- function(dimension, mapping) {
  if (is.atomic(mapping)) {
    if (mapping == "asIdentity") {
      list(dimensionName = dimension, asIdentity = TRUE)
    } else {
      message("No mapping or asIdentity proposed.")
    }
  } else {
    if (("name" %in% colnames(mapping))==FALSE) {
      mapping$name <- mapping$code
    }
    list(dimensionName = dimension, map = mapper(mapping))
  }
}

# want this available, plus region-by-region LAD and a national LEP one
# London one already available, maybe leave other GORs until a need?
mygrid <- data.frame(
  code = c("Scotland", "North East", "Yorkshire and the Humber",
           "North West", "East of England", "East Midlands",
           "Wales", "West Midlands", "South East",
           "South West", "London"),
  name = c("UKM", "UKC", "UKE",
           "UKD", "UKH", "UKF",
           "UKL", "UKG", "UKJ",
           "UKK", "UKI"),
  row = c(1, 2, 3,
          3, 4, 4,
          4, 4, 5,
          5, 5),
  col = c(2, 2.5, 3, 2, 4, 3, 1, 2, 3, 2, 4),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid)
