#' delete materialized view from mimic3
#'
#' @param matview matview names
#' @param conn connection
#'
#' @export
#'
#' @examples
#' \donttest{
#' delete_matview_3()
#' }
delete_matview_3 <- function(matview,conn){
    if (missing(conn)) conn = get('connect_MIMIC',envir = .GlobalEnv)
    if (missing(matview)){
        # delete all
        j=0
        while (all(j <= 20)) {
            j=j+1
            matview=matview_3(conn,schema='mimic3')
            for (i in 1:length(matview)) {
                if (matview[i] == 'no matviews found') next(i)
                if (i==1) echeck=c()
                x=tryCatch(suppressWarnings(suppressMessages(DBI::dbGetQuery(conn = conn$con,
                                                                             statement = sprintf("DROP MATERIALIZED VIEW mimic3.%s;",
                                                                                                 matview[i])))),
                           error=function(e) 'e')
                if (is.character(x)){
                    echeck=c(echeck,matview[i])
                }else{
                    cat('delete',matview[i],'\n')
                }

            }
        }

    }else{
        echeck=matview
        while (length(echeck) >0) {
            matview=echeck
            for (i in 1:length(matview)) {
                if (i==1) echeck=c()
                x=tryCatch(suppressWarnings(suppressMessages(DBI::dbGetQuery(conn = conn$con,
                                                                             statement = sprintf("DROP MATERIALIZED VIEW mimic3.%s;",
                                                                                                 matview[i])))),
                           error=function(e) 'e')
                if (is.character(x)){
                    echeck=c(echeck,matview[i])
                }else{
                    cat('delete',matview[i],'\n')
                }

            }
        }
    }
    message('=== all matviews were deleted ===')
}
