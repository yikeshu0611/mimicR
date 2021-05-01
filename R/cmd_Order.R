#' order for PostgreSQL data
#' order for data with tbl_PostgreSQLConnection
#' @param x data with tbl_PostgreSQLConnection
#' @param ... one or more colnames start with minus or not
#'
#' @return data with tbl_PostgreSQLConnection
#' @export
#' @examples
#' \donttest{
#' library(mimicR)
#' connect_MIMIC <- connect_MIMIC()
#' tbl3_caregivers() %>%
#'     Order('label','-cgid')
#' }
Order <- function(x,...) UseMethod('Order')

#' @method Order data.frame
#' @export
Order.data.frame <- function(x,...){
    odb <- c(...)
    odb[do::left(odb,1) != '-']=paste0(as.character(substitute(x)),'$',odb[do::left(odb,1) != '-'])
    odb[do::left(odb,1) == '-']=paste0('-',as.character(substitute(x)),'$',do::knife_left(odb[do::left(odb,1) == '-'],1))
    p <- paste0(odb,collapse = ', ')
    p0 <- '%s[order(%s),]'
    txt <- sprintf(p0,as.character(substitute(x)),p)
    eval(parse(text = txt))
}

#' @method Order tbl_PostgreSQLConnection
#' @export
Order.tbl_PostgreSQLConnection <- function(x,...){
    sql = 'select * from (%s) q%s order by %s'
    org <- as.character(dbplyr::sql_render(x))
    q <- sql_postgre_q(x) + 1
    odb <- c(...)
    odb[do::left(odb,1) != '-']=paste0(odb[do::left(odb,1) != '-'],' asc')
    odb[do::left(odb,1) == '-']=paste0(do::knife_left(odb[do::left(odb,1) == '-'],1),' desc')
    s3 <- paste0(odb,collapse = ', ')
    sql2 <- sprintf(sql,org,q,s3)
    y=dplyr::tbl(x$src$con,dplyr::sql(sql2))
    class(y)=class(x)
    y
}
