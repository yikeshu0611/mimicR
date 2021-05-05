#' Extract table from mimic database
#'
#' @param mimic mimic database
#' @param tbl one table name or table number
#' @param cols one or more colnames or column numbers
#' @param schema schama name, default is mimiciii
#' @return all table names if tbl is missing, or tible
#' @export
#'
#' @examples
#' \donttest{
#' library(mimicR)
#' connect_MIMIC <- connect_MIMIC()
#'
#'
#' # extract all table names
#' tbl4()
#'
#' # extract services table with row_id and subject_id columns
#' tbl4('services','row_id','subject_id')
#'
#' }
tbl4 <- function(tbl,...,conn){
    if (missing(conn)) conn=get('connect_MIMIC',envir = .GlobalEnv)
    all_table <- dplyr::src_tbls(conn)
    all_table <- all_table[!grepl('chartevents_|zzz',all_table)]
    all_table <- all_table[order(all_table)]
    if (missing(tbl)){
        all_table
    }else{
        cols <- c(...)
        if (is.null(cols)) cols ='*'
        cols=unique(cols)
        cols <- paste0(cols,collapse = ',')
        expr <- "SELECT %s FROM %s.%s"
        expr <- sprintf(expr,cols,'mimic4',tbl)
        dplyr::tbl(conn,dplyr::sql(expr))
    }
}
