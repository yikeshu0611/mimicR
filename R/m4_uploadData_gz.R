#' upload mimic iv data to PostgreSQL
#'
#' @param data_dir data directory
#' @param tbl table names for some one
#' @param conn connection, if missing, it will be get 'connectiontopsql' from global environment.
#'
#' @return add mimic iv data to PostgreSQL
#' @export
#'
#' @examples
#' \donttest{
#' uploadData_4gz('f:/data')
#' }
uploadData_4gz <- function(data_dir,
                           tbl=c('admissions', 'chartevents', 'd_hcpcs', 'd_icd_diagnoses', 'd_icd_procedures',
                                 'd_items', 'd_labitems', 'datetimeevents', 'diagnoses_icd', 'drgcodes', 'emar',
                                 'emar_detail', 'hcpcsevents', 'icustays', 'inputevents', 'labevents',
                                 'microbiologyevents', 'outputevents', 'patients', 'pharmacy', 'poe',
                                 'poe_detail', 'prescriptions', 'procedureevents', 'procedures_icd',
                                 'services', 'transfers'),
                           conn){
    if (missing(conn)) conn = get('connect_MIMIC',envir = .GlobalEnv)
    tbl_full <- list.files(data_dir,pattern = '.csv.gz',recursive = TRUE,full.names = TRUE)
    tbl2 <- do::Replace0(tbl_full,c('.*/','.csv.gz'))
    jdg <- tolower(tbl2) %in% tolower(tbl)
    tbl2 <- tbl2[jdg]
    tbl_full <- tbl_full[jdg]
    #                                        tbl name                  gz full path name
    cmd <- "SET client_min_messages TO WARNING;\nset search_path to mimic4;\ncopy %s FROM PROGRAM 'gzip -dc %s' DELIMITER ',' CSV HEADER NULL ''"

    message('upload data')
    for (i in 1:length(tbl_full)){
        if (i==1) res=0
        cat("     data",paste0(i,'. ',tbl2[i]))
        state <- sprintf(cmd,tbl2[i],tbl_full[i])
        consume <- system.time(DBI::dbGetQuery(conn = conn$con,
                                               statement = state))
        cat('  ',time_segment(consume[3]),'\n')
        res=res+consume[3]
        if (i==length(x)) cat('  ',time_segment(res),'\n')
    }
}
