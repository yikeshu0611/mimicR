#' Title
#'
#' @param x dataframe or tbl_PostgreSQLConnection object
#' @param column one column name
#' @param newname new column name
#'
#' @return dataframe or tbl_PostgreSQLConnection with seqence
#' @export
#'
SeqN <- function(x,column,newname='SeqN')  UseMethod('SeqN')

#' @export
#' @method SeqN data.frame
SeqN.data.frame <- function(x,column,newname='SeqN'){
    u=unique(x[,column])
    u=u[!is.na(u)]
    x[,newname]=NA
    for (i in u) {
        jdg <-  x[,column] == i
        jdg[is.na(jdg)] = FALSE
        x[jdg,newname] <- 1:sum(jdg)
    }
    x
}


#' @export
#' @method SeqN tbl_PostgreSQLConnection
SeqN.tbl_PostgreSQLConnection <- function(x,column,newname='SeqN'){
    sql = 'select *,ROW_NUMBER() over (PARTITION BY %s) as %s
          from (\n%s\n) q%s'
    sql2 <- sprintf(sql,column,newname,
                    as.character(dbplyr::sql_render(x)),
                    sql_postgre_q(x)+1)
    dplyr::tbl(x$src$con,dplyr::sql(sql2))
}
