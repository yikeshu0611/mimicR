#' Connect to MIMIC3 database in PostgreSQL
#'
#' @param user user name
#' @param password password
#' @param dbname database name, default is mimic3.
#' @param ... other parameter.
#'
#' @return connection to MIMIC3 database in PostgreSQL.
#' @export
#'
#' @examples
#' \donttest{
#' connect_MIMIC <- connect3()
#' }
connect_MIMIC <- function(user='postgres',password = 'pg',
                       dbname='mimic',...){
    con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          user = user,
                          password = password,
                          dbname=dbname,
                          ...)
    dbplyr::src_dbi(con, auto_disconnect = TRUE)
}
