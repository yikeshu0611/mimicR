#' conn tables
#' @param ... one or more column names
#' @param conn connection
#' @name table
#' @export
tbl4_admissions <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id')
        cols = unique(c(key,cols))
    }
    tbl = 'admissions'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-admissions;patients;subject_id;subject_id',
               'link-admissions;callout;hadm_id;hadm_id',
               'link-admissions;chartevents;hadm_id;hadm_id',
               'link-admissions;cptevents;hadm_id;hadm_id',
               'link-admissions;datetimeevents;hadm_id;hadm_id',
               'link-admissions;diagnoses_icd;hadm_id;hadm_id',
               'link-admissions;drgcodes;hadm_id;hadm_id',
               'link-admissions;icustays;hadm_id;hadm_id',
               'link-admissions;inputevents_cv;hadm_id;hadm_id',
               'link-admissions;inputevents_mv;hadm_id;hadm_id',
               'link-admissions;labevents;hadm_id;hadm_id',
               'link-admissions;microbiologyevents;hadm_id;hadm_id',
               'link-admissions;noteevents;hadm_id;hadm_id',
               'link-admissions;outputevents;hadm_id;hadm_id',
               'link-admissions;prescriptions;hadm_id;hadm_id',
               'link-admissions;procedureevents_mv;hadm_id;hadm_id',
               'link-admissions;procedures_icd;hadm_id;hadm_id',
               'link-admissions;services;hadm_id;hadm_id',
               'link-admissions;transfers;hadm_id;hadm_id')

    x
}

#' @rdname table
#' @export
tbl4_callout <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id')
        cols = unique(c(key,cols))
    }
    tbl = 'callout'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-callout;patients;subject_id;subject_id',
               'link-admissions;callout;hadm_id;hadm_id')

    x
}

#' @rdname table
#' @export
tbl4_caregivers <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = 'cgid'
        cols = unique(c(key,cols))
    }
    tbl = 'caregivers'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-caregivers;chartevents;cgid;cgid',
               'link-caregivers;datetimeevents;cgid;cgid',
               'link-caregivers;inputevents_cv;cgid;cgid',
               'link-caregivers;inputevents_mv;cgid;cgid',
               'link-caregivers;noteevents;cgid;cgid',
               'link-caregivers;outputevents;cgid;cgid')

    x
}

#' @rdname table
#' @export
tbl4_chartevents <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id','cgid','itemid')
        cols = unique(c(key,cols))
    }
    tbl = 'chartevents'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-chartevents;patients;subject_id;subject_id',
               'link-admissions;chartevents;hadm_id;hadm_id',
               'link-chartevents;icustays;icustay_id;icustay_id',
               'link-chartevents;d_items;itemid;itemid',
               'link-caregivers;chartevents;cgid;cgid')

    x
}

#' @rdname table
#' @export
tbl4_cptevents <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','cpt_cd')
        cols = unique(c(key,cols))
    }
    tbl = 'cptevents'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-cptevents;patients;subject_id;subject_id',
               'link-admissions;cptevents;hadm_id;hadm_id',
               'link-cptevents;d_cpt;cpt_cd;mincodeinsubsection and maxcodeinsubsection')

    x
}

#' @rdname table
#' @export
tbl4_datetimeevents <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id','itemid','cgid')
        cols = unique(c(key,cols))
    }
    tbl = 'datetimeevents'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-datetimeevents;patients;subject_id;subject_id',
               'link-admissions;datetimeevents;hadm_id;hadm_id',
               'link-datetimeevents;icustays;icustay_id;icustay_id',
               'link-d_items;datetimeevents;itemid;itemid',
               'link-caregivers;datetimeevents;cgid;cgid')

    x
}

#' @rdname table
#' @export
tbl4_diagnoses_icd <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icd9_code')
        cols = unique(c(key,cols))
    }
    tbl = 'diagnoses_icd'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-diagnoses_icd;patients;subject_id;subject_id',
               'link-admissions;diagnoses_icd;hadm_id;hadm_id',
               'link-d_icd_diagnoses;diagnoses_icd;icd9_code;icd9_code',
               'link-d_icd_procedures;diagnoses_icd;icd9_code;icd9_code',
               'link-diagnoses_icd;procedures_icd;icd9_code;icd9_code')

    x
}

#' @rdname table
#' @export
tbl4_drgcodes <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id')
        cols = unique(c(key,cols))
    }
    tbl = 'drgcodes'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-drgcodes;patients;subject_id;subject_id',
               'link-admissions;drgcodes;hadm_id;hadm_id')

    x
}

#' @rdname table
#' @export
tbl4_d_cpt <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('mincodeinsubsection','maxcodeinsubsection ')
        cols = unique(c(key,cols))
    }
    tbl = 'd_cpt'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-cptevents;d_cpt;cpt_cd;mincodeinsubsection and maxcodeinsubsection')

    x
}

#' @rdname table
#' @export
tbl4_d_icd_diagnoses <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = 'icd9_code'
        cols = unique(c(key,cols))
    }
    tbl = 'd_icd_diagnoses'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-d_icd_diagnoses;d_icd_procedures;icd9_code;icd9_code',
               'link-d_icd_diagnoses;diagnoses_icd;icd9_code;icd9_code',
               'link-d_icd_diagnoses;procedures_icd;icd9_code;icd9_code')

    x
}
#' @rdname table
#' @export
tbl4_procedures_icd <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icd9_code')
        cols = unique(c(key,cols))
    }
    tbl = 'procedures_icd'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-patients;procedures_icd;subject_id;subject_id',
               'link-admissions;procedures_icd;hadm_id;hadm_id',
               'link-d_icd_diagnoses;procedures_icd;icd9_code;icd9_code',
               'link-d_icd_procedures;procedures_icd;icd9_code;icd9_code',
               'link-diagnoses_icd;procedures_icd;icd9_code;icd9_code')

    x
}
#' @rdname table
#' @export
tbl4_d_icd_procedures <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = 'icd9_code'
        cols = unique(c(key,cols))
    }
    tbl = 'd_icd_procedures'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-d_icd_diagnoses;d_icd_procedures;icd9_code;icd9_code',
               'link-d_icd_procedures;diagnoses_icd;icd9_code;icd9_code',
               'link-d_icd_procedures;procedures_icd;icd9_code;icd9_code')

    x
}

#' @rdname table
#' @export
tbl4_d_items <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('itemid','label','unitname','category','linksto','param_type')
        cols = unique(c(key,cols))
    }
    tbl = 'd_items'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-chartevents;d_items;itemid;itemid',
               'link-d_items;datetimeevents;itemid;itemid',
               'link-d_items;inputevents_cv;itemid;itemid',
               'link-d_items;inputevents_mv;itemid;itemid',
               'link-d_items;microbiologyevents;itemid;spec_itemid',
               'link-d_items;microbiologyevents;itemid;org_itemid',
               'link-d_items;microbiologyevents;itemid;ab_itemid',
               'link-d_items;outputevents;itemid;itemid',
               'link-d_items;procedureevents_mv;itemid;itemid')

    x
}

#' @rdname table
#' @export
tbl4_d_labitems <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = 'itemid'
        cols = unique(c(key,cols))
    }
    tbl = 'd_labitems'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-d_labitems;labevents;itemid;itemid')

    x
}

#' @rdname table
#' @export
tbl4_icustays <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id')
        cols = unique(c(key,cols))
    }
    tbl = 'icustays'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-icustays;patients;subject_id;subject_id',
               'link-admissions;icustays;hadm_id;hadm_id',
               'link-chartevents;icustays;icustay_id;icustay_id',
               'link-datetimeevents;icustays;icustay_id;icustay_id',
               'link-icustays;inputevents_cv;icustay_id;icustay_id',
               'link-icustays;inputevents_mv;icustay_id;icustay_id',
               'link-icustays;outputevents;icustay_id;icustay_id',
               'link-icustays;prescriptions;icustay_id;icustay_id',
               'link-icustays;procedureevents_mv;icustay_id;icustay_id',
               'link-icustays;transfers;icustay_id;icustay_id')

    x
}

#' @rdname table
#' @export
tbl4_inputevents_cv <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id','itemid','cgid')
        cols = unique(c(key,cols))
    }
    tbl = 'inputevents_cv'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-inputevents_cv;patients;subject_id;subject_id',
               'link-admissions;inputevents_cv;hadm_id;hadm_id',
               'link-icustays;inputevents_cv;icustay_id;icustay_id',
               'link-d_items;inputevents_cv;itemid;itemid',
               'link-caregivers;inputevents_cv;cgid;cgid')

    x
}

#' @rdname table
#' @export
tbl4_inputevents_mv <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id','cgid','itemid')
        cols = unique(c(key,cols))
    }
    tbl = 'inputevents_mv'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-inputevents_mv;patients;subject_id;subject_id',
               'link-admissions;inputevents_mv;hadm_id;hadm_id',
               'link-icustays;inputevents_mv;icustay_id;icustay_id',
               'link-d_items;inputevents_mv;itemid;itemid',
               'link-caregivers;inputevents_mv;cgid;cgid')

    x
}

#' @rdname table
#' @export
tbl4_labevents <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','itemid')
        cols = unique(c(key,cols))
    }
    tbl = 'labevents'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-labevents;patients;subject_id;subject_id',
               'link-admissions;labevents;hadm_id;hadm_id',
               'link-d_labitems;labevents;itemid;itemid')

    x
}

#' @rdname table
#' @export
tbl4_microbiologyevents <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','ab_itemid','org_itemid','spec_itemid')
        cols = unique(c(key,cols))
    }
    tbl = 'microbiologyevents'
    x <- tbl4(tbl=tbl,cols,conn = conn)

    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-microbiologyevents;patients;subject_id;subject_id',
               'link-admissions;microbiologyevents;hadm_id;hadm_id',
               'link-d_items;microbiologyevents;itemid;spec_itemid',
               'link-d_items;microbiologyevents;itemid;org_itemid',
               'link-d_items;microbiologyevents;itemid;ab_itemid')

    x
}

#' @rdname table
#' @export
tbl4_noteevents <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','cgid')
        cols = unique(c(key,cols))
    }
    tbl = 'noteevents'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-noteevents;patients;subject_id;subject_id',
               'link-admissions;noteevents;hadm_id;hadm_id',
               'link-caregivers;noteevents;cgid;cgid')

    x
}

#' @rdname table
#' @export
tbl4_outputevents <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id','itemid','cgid')
        cols = unique(c(key,cols))
    }
    tbl = 'outputevents'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-outputevents;patients;subject_id;subject_id',
               'link-admissions;outputevents;hadm_id;hadm_id',
               'link-icustays;outputevents;icustay_id;icustay_id',
               'link-d_items;outputevents;itemid;itemid',
               'link-caregivers;outputevents;cgid;cgid')

    x
}

#' @rdname table
#' @export
tbl4_patients <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = 'subject_id'
        cols = unique(c(key,cols))
    }
    tbl = 'patients'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-admissions;patients;subject_id;subject_id',
               'link-callout;patients;subject_id;subject_id',
               'link-chartevents;patients;subject_id;subject_id',
               'link-cptevents;patients;subject_id;subject_id',
               'link-datetimeevents;patients;subject_id;subject_id',
               'link-diagnoses_icd;patients;subject_id;subject_id',
               'link-drgcodes;patients;subject_id;subject_id',
               'link-icustays;patients;subject_id;subject_id',
               'link-inputevents_cv;patients;subject_id;subject_id',
               'link-inputevents_mv;patients;subject_id;subject_id',
               'link-labevents;patients;subject_id;subject_id',
               'link-microbiologyevents;patients;subject_id;subject_id',
               'link-noteevents;patients;subject_id;subject_id',
               'link-outputevents;patients;subject_id;subject_id',
               'link-patients;prescriptions;subject_id;subject_id',
               'link-patients;procedureevents_mv;subject_id;subject_id',
               'link-patients;procedures_icd;subject_id;subject_id',
               'link-patients;services;subject_id;subject_id',
               'link-patients;transfers;subject_id;subject_id')

    x
}

#' @rdname table
#' @export
tbl4_prescriptions <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id')
        cols = unique(c(key,cols))
    }
    tbl = 'prescriptions'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-patients;prescriptions;subject_id;subject_id',
               'link-admissions;prescriptions;hadm_id;hadm_id',
               'link-icustays;prescriptions;icustay_id;icustay_id')

    x
}

#' @rdname table
#' @export
tbl4_procedureevents_mv <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id','itemid')
        cols = unique(c(key,cols))
    }
    tbl = 'procedureevents_mv'
    x <- tbl4(tbl=tbl,cols,conn = conn)
    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-patients;procedureevents_mv;subject_id;subject_id',
               'link-admissions;procedureevents_mv;hadm_id;hadm_id',
               'link-icustays;procedureevents_mv;icustay_id;icustay_id',
               'link-d_items;procedureevents_mv;itemid;itemid')

    x
}



#' @rdname table
#' @export
tbl4_services <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    tbl = 'services'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id')
        cols = unique(c(key,cols))
    }
    x <- tbl4(tbl=tbl,cols,conn = conn)

    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-patients;services;subject_id;subject_id',
               'link-admissions;services;hadm_id;hadm_id')

    x
}

#' @rdname table
#' @export
tbl4_transfers <- function(...,conn){
    cols <- c(...)
    if (is.null(cols)) cols = '*'
    tbl = 'transfers'
    if (all(cols != '*')){
        key = c('subject_id','hadm_id','icustay_id')
        cols = unique(c(key,cols))
    }
    x <- tbl4(tbl=tbl,cols,conn = conn)

    class(x)=c(class(x),paste0('tablename--',tbl),
               'link-patients;transfers;subject_id;subject_id',
               'link-admissions;transfers;hadm_id;hadm_id',
               'link-icustays;transfers;icustay_id;icustay_id')

    x
}
