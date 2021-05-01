#' put sql file lines to one command for r
#' sql files must be simplified.
#' @param sql sql file names
#'
#' @export
#'
sql_to_command <- function(sql){
    if (missing(sql)) sql=list.files(pattern = '.sql')
    for (i in 1:length(sql)) {
        append=TRUE
        if (i==1) append=FALSE
        cat('write', sql[i],'\n')
        write.table(paste0(do::Replace0(sql[i],'.sql'),' <- "',
                           paste0(readLines(sql[i]),collapse = 'onlyforclapse'),'"\n\n'),
                    'sql_to_command.R',
                    append = append,col.names = F,row.names = F,quote = F)

    }
    f <- 'd <- c(%s)'
    f2 <- sprintf(f,do::Replace0(paste0(sql,collapse = ', '),'.sql'))
    write.table(f2,'sql_to_command.R',
                append = TRUE,col.names = F,row.names = F,quote = F)
    f.name <- 'd.name <- c(\"%s\")'
    f.name2 <- sprintf(f.name,do::Replace0(paste0(sql,collapse = '", "'),'.sql'))
    write.table(f.name2,'sql_to_command.R',
                append = TRUE,col.names = F,row.names = F,quote = F)

    loop = "\nfor (i in 1:length(d)) {
    if (i==1) message(sprintf('push %s This data',length(d)))
    cat(paste0(i,'.'),d.names[i],'onlyforclapse')
    DBI::dbGetQuery(conn = mimic$con,statement =d[i])
}"
    write.table(loop,'sql_to_command.R',
                append = TRUE,col.names = F,row.names = F,quote = F)

    cat('\nall sqls were written to sql_to_command.R\n')
    message('ATTENTION: replace onlyforclapse to \\n')
}
