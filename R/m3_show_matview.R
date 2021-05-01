#' Return materialized view names
#'
#' @param conn connection of mimic3 database
#' @param schema schema name, default is mimic3
#' @importFrom magrittr %>%
#' @return characters
#' @export
#'
matview_3 <- function(conn,schema='mimic3'){
    if (missing(conn)) conn = get('connect_MIMIC',envir = .GlobalEnv)
    sql=sprintf("select matviewname from pg_matviews where schemaname='%s'",
                schema)
    df<-dplyr::tbl(conn,
                   dbplyr::sql(sql)) %>%
        as.data.frame()
    if (nrow(df)==0) return('no matviews found')
    df[,1]
}
