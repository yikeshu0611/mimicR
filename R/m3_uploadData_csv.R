#' upload data to PostgreSQL
#'
#' @param data_dir data directory
#' @param tbl table names for some one
#' @param conn connection, if missing, it will be get 'connectiontopsql' from global environment.
#'
#' @return add 26 data to PostgreSQL
#' @export
#'
#' @examples
#' \donttest{
#' uploadData_3csv('f:/data')
#' }
uploadData_3csv <- function(data_dir,
                             tbl=c("admissions", "callout", "caregivers", "chartevents", "cptevents", "d_cpt", "d_icd_diagnoses", "d_icd_procedures", "d_items", "d_labitems", "datetimeevents", "diagnoses_icd", "drgcodes", "icustays", "inputevents_cv", "inputevents_mv", "labevents", "microbiologyevents", "noteevents", "outputevents", "patients", "prescriptions", "procedureevents_mv", "procedures_icd", "services", "transfers"),
                             conn){
    if (missing(conn)) conn = get('connect_MIMIC',envir = .GlobalEnv)
    if (do::right(data_dir,1) != '/') data_dir=paste0(data_dir,'/')
    ADMISSIONS <- "set search_path to mimic3;\ncopy ADMISSIONS FROM '%sADMISSIONS.csv.gz' DELIMITER ',' CSV HEADER NULL ''"
    CALLOUT <- "set search_path to mimic3;\ncopy CALLOUT from '%sCALLOUT.csv.gz' delimiter ',' csv header NULL ''"
    CAREGIVERS <- "set search_path to mimic3;\ncopy CAREGIVERS from '%sCAREGIVERS.csv.gz' delimiter ',' csv header NULL ''"
    CHARTEVENTS <- "set search_path to mimic3;\ncopy CHARTEVENTS from '%sCHARTEVENTS.csv.gz' delimiter ',' csv header NULL ''"
    CPTEVENTS <- "set search_path to mimic3;\ncopy CPTEVENTS from '%sCPTEVENTS.csv.gz' delimiter ',' csv header NULL ''"
    DATETIMEEVENTS <- "set search_path to mimic3;\ncopy DATETIMEEVENTS from '%sDATETIMEEVENTS.csv.gz' delimiter ',' csv header NULL ''"
    DIAGNOSES_ICD <- "set search_path to mimic3;\ncopy DIAGNOSES_ICD from '%sDIAGNOSES_ICD.csv.gz' delimiter ',' csv header NULL ''"
    DRGCODES <- "set search_path to mimic3;\ncopy DRGCODES from '%sDRGCODES.csv.gz' delimiter ',' csv header NULL ''"
    D_CPT <- "set search_path to mimic3;\ncopy D_CPT from '%sD_CPT.csv.gz' delimiter ',' csv header NULL ''"
    D_ICD_DIAGNOSES <- "set search_path to mimic3;\ncopy D_ICD_DIAGNOSES from '%sD_ICD_DIAGNOSES.csv.gz' delimiter ',' csv header NULL ''"
    D_ICD_PROCEDURES <- "set search_path to mimic3;\ncopy D_ICD_PROCEDURES from '%sD_ICD_PROCEDURES.csv.gz' delimiter ',' csv header NULL ''"
    D_ITEMS <- "set search_path to mimic3;\ncopy D_ITEMS from '%sD_ITEMS.csv.gz' delimiter ',' csv header NULL ''"
    D_LABITEMS <- "set search_path to mimic3;\ncopy D_LABITEMS from '%sD_LABITEMS.csv.gz' delimiter ',' csv header NULL ''"
    ICUSTAYS <- "set search_path to mimic3;\ncopy ICUSTAYS from '%sICUSTAYS.csv.gz' delimiter ',' csv header NULL ''"
    INPUTEVENTS_CV <- "set search_path to mimic3;\ncopy INPUTEVENTS_CV from '%sINPUTEVENTS_CV.csv.gz' delimiter ',' csv header NULL ''"
    INPUTEVENTS_MV <- "set search_path to mimic3;\ncopy INPUTEVENTS_MV from '%sINPUTEVENTS_MV.csv.gz' delimiter ',' csv header NULL ''"
    LABEVENTS <- "set search_path to mimic3;\ncopy LABEVENTS from '%sLABEVENTS.csv.gz' delimiter ',' csv header NULL ''"
    MICROBIOLOGYEVENTS <- "set search_path to mimic3;\ncopy MICROBIOLOGYEVENTS from '%sMICROBIOLOGYEVENTS.csv.gz' delimiter ',' csv header NULL ''"
    NOTEEVENTS <- "set search_path to mimic3;\ncopy NOTEEVENTS from '%sNOTEEVENTS.csv.gz' delimiter ',' csv header NULL ''"
    OUTPUTEVENTS <- "set search_path to mimic3;\ncopy OUTPUTEVENTS from '%sOUTPUTEVENTS.csv.gz' delimiter ',' csv header NULL ''"
    PATIENTS <- "set search_path to mimic3;\ncopy PATIENTS from '%sPATIENTS.csv.gz' delimiter ',' csv header NULL ''"
    PRESCRIPTIONS <- "set search_path to mimic3;\ncopy PRESCRIPTIONS from '%sPRESCRIPTIONS.csv.gz' delimiter ',' csv header NULL ''"
    PROCEDUREEVENTS_MV <- "set search_path to mimic3;\ncopy PROCEDUREEVENTS_MV from '%sPROCEDUREEVENTS_MV.csv.gz' delimiter ',' csv header NULL ''"
    PROCEDURES_ICD <- "set search_path to mimic3;\ncopy PROCEDURES_ICD from '%sPROCEDURES_ICD.csv.gz' delimiter ',' csv header NULL ''"
    SERVICES <- "set search_path to mimic3;\ncopy SERVICES from '%sSERVICES.csv.gz' delimiter ',' csv header NULL ''"
    TRANSFERS <- "set search_path to mimic3;\ncopy TRANSFERS from '%sTRANSFERS.csv.gz' delimiter ',' csv header NULL ''"



    x = c(ADMISSIONS, CALLOUT, CAREGIVERS, CHARTEVENTS,
          CPTEVENTS, D_CPT, D_ICD_DIAGNOSES, D_ICD_PROCEDURES,
          D_ITEMS, D_LABITEMS, DATETIMEEVENTS, DIAGNOSES_ICD,
          DRGCODES, ICUSTAYS, INPUTEVENTS_CV, INPUTEVENTS_MV,
          LABEVENTS, MICROBIOLOGYEVENTS, NOTEEVENTS, OUTPUTEVENTS,
          PATIENTS, PRESCRIPTIONS, PROCEDUREEVENTS_MV,
          PROCEDURES_ICD, SERVICES, TRANSFERS)


    x.names=c("admissions", "callout", "caregivers", "chartevents", "cptevents", "d_cpt", "d_icd_diagnoses", "d_icd_procedures", "d_items", "d_labitems", "datetimeevents", "diagnoses_icd", "drgcodes", "icustays", "inputevents_cv", "inputevents_mv", "labevents", "microbiologyevents", "noteevents", "outputevents", "patients", "prescriptions", "procedureevents_mv", "procedures_icd", "services", "transfers")
    if (!missing(tbl)){
        tbl = match.arg(tbl)
        x=x[x.names %in% tbl]
        x.names=x.names[x.names %in% tbl]
    }
    message('upload data')
    for (i in 1:length(x)){
        if (i==1) res=0
        cat("     data",paste0(i,'. ',x.names[i]))
        consume <- system.time(DBI::dbGetQuery(conn = conn$con,
                                               statement = sprintf(x[i],data_dir)))
        cat('  ',time_segment(consume[3]),'\n')
        res=res+consume[3]
        if (i==length(x)) cat('  ',time_segment(res),'\n')
    }
}
