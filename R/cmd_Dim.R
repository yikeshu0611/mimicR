#' dimension of tbl_PostgreSQLConnection
#'
#' @param x tbl_PostgreSQLConnection
#'
#' @return number of row and column
#' @export
Dim <- function(x) UseMethod('Dim')


#' @export
#' @method Dim tbl_PostgreSQLConnection
Dim.tbl_PostgreSQLConnection <- function(x){
    sql0 <- 'select count(*) as rown from (%s) q%s'
    s1 <- as.character(dbplyr::sql_render(x))
    s2 <- sql_postgre_q(x)+1
    sql <- dbplyr::sql(sprintf(sql0,s1,s2))
    nrow <- as.data.frame(dplyr::tbl(x$src$con,sql))[1,1]
    ncol <- dim(x)[2]
    c(nrow,ncol)
}

#' @export
#' @method Dim data.frame
Dim.data.frame <- function(x){
    dim(x)
}
