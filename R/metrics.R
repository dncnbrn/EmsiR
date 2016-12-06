metricalc <- function(metrics,base,geoparent,along) {
  if(metrics=="LQ"){
    metout <- list(name="LocationQuotient",
                   geoparent=geoparent,
                   along=along)
  }
  if(metrics=="SS"){
    metout <- list(name="ShiftShare",
                   geoparent=geoparent,
                   along=along,
                   base=base)
  }
  return(metout)
}

metricmaker <- function(metricdf, geoparent, along){
  if(ncol(metricdf)==2) {
    metricdf$metrics <- c(rep(NA,nrow(metricdf)))
  }
  if(ncol(metricdf)==3 & nrow(metricdf[!is.na(metricdf$metrics),])==0) {
    metrics <- metricdf
    metrics$metrics <- NULL
  }
  if(ncol(metricdf)>=3 & nrow(metricdf[is.na(metricdf$metrics),])==0) {
    metrics <- metricdf %>%
      mutate(geoparent=geoparent,
             along=along) %>%
      group_by(name,as) %>%
      do(operation=metricalc(.$metrics,.$base,.$geoparent,.$along))
  }
  if(ncol(metricdf)>=3 & nrow(metricdf[!is.na(metricdf$metrics),])>0 &
     nrow(metricdf[is.na(metricdf$metrics),])>0) {
    a <- metricdf %>%
      filter(is.na(metrics)) %>%
      select(name,as)
    a$operation <- list(NA)
    b <- metricdf %>%
      filter(!is.na(metrics))
    b <- b %>%
      mutate(geoparent=geoparent,
             along=along) %>%
      group_by(name,as) %>%
      do(operation=metricalc(.$metrics,.$base,.$geoparent,.$along))
    metrics <- bind_rows(a,b)
    rm(a,b)}
  return(metrics)
}
