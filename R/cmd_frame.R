
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
    cla <- class(x)
    # choose column
    frame="select %s from ( %s ) q%s"
    j <- do::Replace0(j,c('\"','\'','c\\(','\\(','\\)'))
    j=paste0(j,collapse = ', ')
    select0 <- dbplyr::sql_render(x)
    q = 1+sql_postgre_q(x)
    sqlr<-sprintf(frame,j,select0,q)
    x <- dplyr::tbl(x$src$con,dbplyr::sql(sqlr))


    # filter row
    if (!missing(i)){
        where00 <<- substitute(i)
        ck <- tryCatch(eval(where00),error=function(e) 'e')
        if (is.numeric(ck)){
            # choose by row number
            rownumber <- paste0(eval(where00),collapse = ',')
            sqlc <- rownumber.n(x,rownumber)
        }else{
            # formula
            where0 <- deparse(where00)
            where <- ii.any(ii=where0)
            sqlc <- paste0(dbplyr::sql_render(x),' where ',where)
        }
        x <- dplyr::tbl(x$src$con,dplyr::sql(sqlc))
    }

    if (df) x <- as.data.frame(x)
    if (!missing(head)) x = as.data.frame(head(x,head))
    if (class(x)[1] == 'data.frame'){
        class(x)=c('data.frame',cla[-1])
    }else{
        class(x)=cla
    }
    if (view){
        View(as.data.frame(x))
        invisible(x)
    }else{
        return(x)
    }


}
rownumber.n <- function(x,rownumber){
    sql0 <- 'with
            a as(
            %s
            ),
            b as(
            	select row_number() over() as roiiiiidrrrow_id,*
            	from a
            ),
            c as(
            	select * from b
            	where roiiiiidrrrow_id = any(array[%s])
            )
            select %s from c'
    sql <- sprintf(sql0,
                   dbplyr::sql_render(x),
                   paste0(rownumber),
                   paste0(colnames(x),collapse = ','))
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
    while (grepl('~ ~c\\(',ii)){
        anyloc <- as.numeric(gregexpr('~ ~c\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('~ ~c\\(', '\\)', '"'))
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
    while (grepl('/~c\\(',ii)){
        anyloc <- as.numeric(gregexpr('/~c\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('/~c\\(', '\\)', '"'))
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
    while (grepl(' == c\\(',ii)){
        anyloc <- as.numeric(gregexpr('== c\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('== c\\(', '\\)', '"'))
        pt <- sprintf("= any(array[%s])",s1)
        ii <- paste0(substr(ii,0,st-1),
                     pt,
                     substr(ii,end+1,nchar(ii)))

    }
    while (grepl(' {0,}not.NULL\\(',ii)){
        anyloc <- as.numeric(gregexpr('not.NULL\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('not.NULL\\(', '\\)', '"'))
        pt <- sprintf("%s is not null",s1)
        ii <- paste0(substr(ii,0,st-1),
                     pt,
                     substr(ii,end+1,nchar(ii)))

    }
    while (grepl(' {0,}is.NULL\\(',ii)){
        anyloc <- as.numeric(gregexpr('is.NULL\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('is.NULL\\(', '\\)', '"'))
        pt <- sprintf("%s is not null",s1)
        ii <- paste0(substr(ii,0,st-1),
                     pt,
                     substr(ii,end+1,nchar(ii)))

    }
    while (grepl(' {0,}not.NA\\(',ii)){
        anyloc <- as.numeric(gregexpr('not.NA\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('not.NA\\(', '\\)', '"'))
        pt <- sprintf("%s is not null",s1)
        ii <- paste0(substr(ii,0,st-1),
                     pt,
                     substr(ii,end+1,nchar(ii)))

    }
    while (grepl(' {0,}is.NA\\(',ii)){
        anyloc <- as.numeric(gregexpr('is.NA\\(',ii)[[1]])
        rq <- as.numeric(gregexpr('\\)',ii)[[1]])
        st=anyloc[1]
        end=min(rq[rq > st])
        simi <- substr(ii,st,end)
        s1 <- do::Replace0(simi,c('is.NA\\(', '\\)', '"'))
        pt <- sprintf("%s is null",s1)
        ii <- paste0(substr(ii,0,st-1),
                     pt,
                     substr(ii,end+1,nchar(ii)))

    }
    do::Replace(ii,
                pattern = c(' ~ ~: ~* ',
                            "\":\'",
                            '/~: !~* ',
                            ' & : AND ',
                            ' == : = ',
                            ' \\| : or '))

}
