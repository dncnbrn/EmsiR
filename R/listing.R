dataset_list <- function() {
  datasets <- POST("http://episteme.economicmodeling.com/meta",
               body = NULL,
               encode = "json",
               add_headers(EpistemeSettings()))
  rawdata <- jsonlite::fromJSON(content(datasets, "text"), simplifyVector = FALSE)
  datasets <- rawdata[[1]] %>% transpose()
  name <- unlist(datasets[[1]])
  versions <-  as.numeric(map_chr(datasets[[2]],length))
  datasets  <-  data.frame(dataset = c(rep(name,versions)),
                           version = c(unlist(datasets[[2]])))
  final <- datasets %>%
    tbl_df() %>%
    mutate(Dataset=as.character(dataset),
           Version=as.character(version),
           Country=substr(dataset,6,7),
           Content=substr(dataset,9,nchar(Dataset)),
           Identifier=paste(dataset,"/",version,sep="")) %>%
    arrange(Country) %>%
    select(-dataset,-version,-Dataset,
           Country, Content, Version, Identifier) %>%
    group_by(Country,Content)
  return(final)
}

concept_list <- function() {
  datasets <- POST("http://episteme.economicmodeling.com/meta",
               body = NULL,
               encode = "json",
               add_headers(EpistemeSettings()))
  rawdata <- jsonlite::fromJSON(content(datasets, "text"), simplifyVector = FALSE)
  concepts <- rawdata[[2]] %>% transpose()
  concepts <- unlist(concepts) %>% tbl_df
  concepts %>%
    tbl_df() %>%
    transmute(Identifier=value) %>%
    mutate(Country=substr(Identifier,6,7),
           Content=substr(Identifier,9,nchar(Identifier))) %>%
    select(Country, Content, Identifier)
}

dataset_detail <- function(country,content,release) {
  dataset <- paste("EMSI",country,content,sep=".")
  URI <- paste("http://episteme.economicmodeling.com/meta/dataset",dataset,release,sep="/")
  parameters <- POST(URI,
                     body = NULL,
                     encode = "json",
                     add_headers(EpistemeSettings()))
  parameters <- jsonlite::fromJSON(content(parameters, "text"), simplifyVector = FALSE)
  dimensions <- parameters[["dimensions"]] %>% unlist
  list(Metrics=as.list(parameters[["metrics"]] %>% unlist %>% tbl_df() %>% arrange(value)),
       Dimensions=as.list(dimensions[names(dimensions) %in% "name"] %>% tbl_df()))
}

concept_detail <- function(country,concept) {
  concept <- paste("EMSI",country,concept,sep=".")
  URI <- paste("http://episteme.economicmodeling.com/meta/concept",concept,sep="/")
  parameters <- POST(URI,
                     body = NULL,
                     encode = "json",
                     add_headers(EpistemeSettings()))
  parameters <- jsonlite::fromJSON(content(parameters, "text"), simplifyVector = FALSE)
  conceptlevels <- unlist(parameters$levels)
  names(conceptlevels) <- NULL
  return(conceptlevels)
}

