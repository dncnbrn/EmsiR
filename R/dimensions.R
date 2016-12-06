mapper <- function(mapnames,mapcodes) {
  map <- list()
  for (i in 1:length(mapnames)) {
    map[[i]] <- as.list(mapcodes[[i]])
  }
  names(map) <- mapnames
  return(map)
}

# ClassOfWorker short-hand with E for Employees, P for Employers/Proprietors
# A for All employed and S for separates

CoW <- function(option) {
  if(option=="E") {
    mapClassCode <- list("1")
    mapClass <- list(Employees = mapClassCode)
    ClassOfWorker <- list(dimensionName = "ClassOfWorker",
                          map = mapClass)
  }
  if(option=="P") {
    mapClassCode <- list("2")
    mapClass <- list(Proprietors = mapClassCode)
    ClassOfWorker <- list(dimensionName = "ClassOfWorker",
                          map = mapClass)
  }
  if(option=="A") {
    mapClassCode <- list("1","2")
    mapClass <- list(All = mapClassCode)
    ClassOfWorker <- list(dimensionName = "ClassOfWorker",
                          map = mapClass)
  }
  if(option=="S") {
    mapClassCode1 <- list("1")
    mapClassCode2 <- list("2")
    mapClass <- list(Employees = mapClassCode1,
                     Proprietors = mapClassCode2)
    ClassOfWorker <- list(dimensionName = "ClassOfWorker",
                          map = mapClass)
  }
  return(ClassOfWorker)
}
