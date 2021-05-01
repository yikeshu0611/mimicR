#' Drop mimic database
#'
#' @param dbname mimic
#' @param user postgres
#' @param password pg
#'
#' @return drop database in PostgreSQL
#' @export
#'
#' @examples
#' \donttest{
#' delete_MIMIC()
#' }
delete_MIMIC <- function(dbname='mimic',user='postgres',password = 'pg'){
    conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          user = user,
                          password = password)
    # revoke
    DBI::dbGetQuery(conn = conn,
                    statement = 'REVOKE CONNECT ON DATABASE mimic FROM public;')
    statement <- "SELECT pg_terminate_backend(pg_stat_activity.pid)
                FROM pg_stat_activity
                WHERE pg_stat_activity.datname = 'mimic'"
    DBI::dbGetQuery(conn = conn,
                          statement = statement)

    # delete
    df <- DBI::dbGetQuery(conn = conn,
                          statement = 'DROP DATABASE IF EXISTS mimic')

    invisible('ok')
}
