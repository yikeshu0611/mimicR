#' unique rows and numbers
#' unique rows and numbers for tbl_PostgreSQLConnection
#' @param x table of tbl_PostgreSQLConnection
#' @param ... one or more columns, if missing, all column names will be used
#'
#' @return lazy table
#' @export
Unique <- function(x,...){

    sqlf = 'SELECT %s, COUNT(*)
            FROM (%s) "q%s"
            group by %s'
    colnames=c(...)
    if (is.null(colnames)) colnames=colnames(x)
    s1 <- paste0(colnames,collapse = ', ')
    s2 <- as.character(dbplyr::sql_render(x))
    q <-  sql_postgre_q(x) + 1
    sql <- dplyr::sql(sprintf(sqlf,s1,s2,q,s1))
    dplyr::tbl(x$src$con,sql)
}
