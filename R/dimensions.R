#' @export
mapper <- function(mapping) {
    map1 <- mapping %>% dplyr::group_by(name) %>% tidyr::nest()
    map2 <- purrr::map(map1$data, "code") %>% purrr::map(as.list)
    names(map2) <- map1$name
    return(map2)
}

#' Specify dimension for an Episteme API query
#'
#' @description  Takes a data frame of names and codes along a dimension and specifies them ready for an Emsi Episteme data pull.
#'
#' @param dimension A named dimension required by the dataset being used for analysis (e.g. "Occupation").
#' @param mapping This may take the form of the atomic "asIdentity" if all possible individual items are required with
#' no relabelling, or else a data frame with two columns: \code{name} sets labels for \code{code} which refers to the
#' geographic, industry, occupation or other codes used to categorise data within Episteme. Where one label is used for
#' multiple codes, \code{dimmaker} will merge them to form a hybrid category.
#' @return A prepared list identifying the dimension and supporting mapping, ready to organise a data pull query.
#' @examples
#' mgrs <- data.frame(names=c("CEOs",
#'                            rep("Group of managers A",3),
#'                            rep("Group of managers B",4)),
#'                    codes=c("1115","1116","1121","1122","1123","1131","1132","1133"))
#' occs <- dimmaker("Occupation", mgrs)
#' @export
dimmaker <- function(dimension, mapping) {
    if (is.atomic(mapping)) {
        if (mapping == "asIdentity") {
            list(dimensionName = dimension, asIdentity = TRUE)
        } else {
            print("No mapping or asIdentity proposed.")
        }
    } else {
        list(dimensionName = dimension, map = mapper(mapping))
    }
}

#' A short-hand function to map ClassOfWorker dimensions around one of four options
#'
#' @param option One of "E" (for Employee), "P" (for "Proprietor"), "A" (for "All" combined) or "S" (for
#' Employee and Proprietor separately.)
#' @return The necessary mapping for ClassOfWorker for inclusion in a data pull query.
#' @examples
#' CoW("E")
#' @export
CoW <- function(option) {
    if (option == "E") {
        mapClassCode <- list("1")
        mapClass <- list(Employees = mapClassCode)
        ClassOfWorker <- list(dimensionName = "ClassOfWorker", map = mapClass)
    }
    if (option == "P") {
        mapClassCode <- list("2")
        mapClass <- list(Proprietors = mapClassCode)
        ClassOfWorker <- list(dimensionName = "ClassOfWorker", map = mapClass)
    }
    if (option == "A") {
        mapClassCode <- list("1", "2")
        mapClass <- list(All = mapClassCode)
        ClassOfWorker <- list(dimensionName = "ClassOfWorker", map = mapClass)
    }
    if (option == "S") {
        mapClassCode1 <- list("1")
        mapClassCode2 <- list("2")
        mapClass <- list(Employees = mapClassCode1, Proprietors = mapClassCode2)
        ClassOfWorker <- list(dimensionName = "ClassOfWorker", map = mapClass)
    }
    return(ClassOfWorker)
}
