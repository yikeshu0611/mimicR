
#' Extract Parts of tbl_PostgreSQLConnection
#'
#' @param x an object of tbl_PostgreSQLConnection
#' @param i row select
#' @param j column names
#' @param view logical, if TRUE, view data
#' @param df logical, if TRUE, convert the results to dataframe
#' @param head one number to extract the head data
#' @return still tbl_PostgreSQLConnection object
#' @export
#'
`[.tbl_PostgreSQLConnection` <- function(x,i,j='*',
                                         view=FALSE,
                                         df=FALSE,
                                         head){
    frame="select %s from ( %s ) q%s"

    j <- do::Replace0(j,c('\"','\'','c\\(','\\(','\\)'))
    j=paste0(j,collapse = ', ')
    select0 <- as.character(x[["ops"]][["x"]])
    q = 1+sql_postgre_q(x)
    sqlch<-sprintf(frame,j,select0,q)
    if (!missing(i)){
        where0 <- deparse(substitute(i))
        where0 <- ii.any(ii=where0)
        where <- do::Replace(where0,
                             pattern = c(' ~ ~: ~* ',
                                         "\":\'",
                                         '/~: !~* ',
                                         ' & : AND ',
                                         ' == : = ',
                                         ' \\| : or '))
        sqlch <- paste0(sqlch,' where ',where)

    }
    r <- dplyr::tbl(x$src$con,dplyr::sql(sqlch))
    if (df) r <- as.data.frame(r)
    if (!missing(head)) r = as.data.frame(head(r,head))
    if (class(r)[1] == 'data.frame'){
        class(r)=c('data.frame',class(x)[-1])
    }else{
        class(r)=class(x)
    }
    if (view){
        View(as.data.frame(r))
        invisible(r)
    }else{
        return(r)
    }


}

ii.any <- function(ii){
    while (grepl('~ ~any\\(',ii)){
        anyloc <- as.numeric(gregexpr('~ ~any\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('~ ~any\\(', '\\)', '"'))
        s <- gsub(' {0,}, {0,}','|',s1)
        pt2 <- paste0("~* '(",s,")'")
        ii <- paste0(substr(ii,0,st-1),
                     pt2,
                     substr(ii,end+1,nchar(ii)))

    }
    while (grepl('/~any\\(',ii)){
        anyloc <- as.numeric(gregexpr('/~any\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('/~any\\(', '\\)', '"'))
        s <- gsub(' {0,}, {0,}','|',s1)
        pt2 <- paste0("!~* '(",s,")'")
        ii <- paste0(substr(ii,0,st-1),
                     pt2,
                     substr(ii,end+1,nchar(ii)))

    }
    while (grepl(' == any\\(',ii)){
        anyloc <- as.numeric(gregexpr('== any\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('== any\\(', '\\)', '"'))
        pt <- sprintf("= any(array[%s])",s1)
        ii <- paste0(substr(ii,0,st-1),
                     pt,
                     substr(ii,end+1,nchar(ii)))

    }
    while (grepl(' not.null\\(',ii)){
        anyloc <- as.numeric(gregexpr('not.null\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('not.null\\(', '\\)', '"'))
        pt <- sprintf("%s is not null",s1)
        ii <- paste0(substr(ii,0,st-1),
                     pt,
                     substr(ii,end+1,nchar(ii)))

    }
    ii
}
