#' Return q number of sql_render()
#'
#' @param x sql object
#'
#' @return one number
#' @export
#'
sql_postgre_q <- function(x){
    sql0 = as.character(dbplyr::sql_render(x))
    q <- as.numeric(gsub('\"|q','',stringr::str_extract_all(sql0,'\"q[0-9]*\"')[[1]]))
    ifelse(length(q)==0,0,q)
}
