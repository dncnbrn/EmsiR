mapper <- function(mapping) {
  map1 <- mapping %>%
    dplyr::group_by(names) %>%
    tidyr::nest()
  map2 <- purrr::map(map1$data, "codes") %>% purrr::map(as.list)
  names(map2) <- map1$names
  return(map2)
}

dimmaker <- function(dimension, mapping) {
  if(is.atomic(mapping)) {
    if(mapping == "asIdentity") {
      list(dimensionName = dimension, asIdentity = TRUE)
    } else {
      print("No mapping or asIdentity proposed.")
    }
  } else
  {
    list(dimensionName = dimension, map = mapper(mapping))
  }
}

# ClassOfWorker short-hand with E for Employees, P for Employers/Proprietors A for All employed
# and S for separates

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
