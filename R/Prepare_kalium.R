
prepareKalium <- function() {

  writeLogHeader("Prepare Kalium360",
      "This part loads basic data")

  writeLog("Check data files and columns")
  checkConfig()

  writeLog("Load and check unit conversion file")
  loadUnitConversionFile()

  writeLog("Loading and joining potassium and patient data")
  createBasePotassiumTable()

  writeLog("Check for repeated measurements (same effectivedatetime)")
  writeLogData("If possible remove the first data point")
  removeRepeatMeasurements("potassium")

  writeLog("Check for and remove implausible values")
  removeExtremValues("potassium")
}

createBasePotassiumTable <- function() {

  # create Patient Table
  path <- paste0(data_dir, "/", name_of_patient_csv)

  # FHIR allows missing birthdate as well as only year or year and month
  # If only the year is there: set 01.07 as birthdate. If only a month is
  # there take the 15. day.

  query <- glue_sql("
        CREATE TEMP TABLE patients AS
        SELECT
          {`pat_id`} AS pat_id,
          {`pat_gebdat`} AS gebdat,
          {`pat_gender`} AS gender,
          TRY_CAST(
            CASE
              WHEN {`pat_gebdat`} IS NULL
                THEN NULL
              WHEN LENGTH(TRIM(CAST({`pat_gebdat`} AS VARCHAR))) = 4
                THEN TRIM(CAST({`pat_gebdat`} AS VARCHAR)) || '-07-01'
              WHEN LENGTH(TRIM(CAST({`pat_gebdat`} AS VARCHAR))) = 7
                THEN TRIM(CAST({`pat_gebdat`} AS VARCHAR)) || '-15'
              ELSE TRIM(CAST({`pat_gebdat`} AS VARCHAR))
            END
          AS DATE) AS gebdat_normalized
        FROM read_csv_auto({path})
      ", .con = con)

  dbExecute(con, query)

  checkForDuplicates("patients", "pat_id")

  # birthdate is a key-value for further analysis. Check if there are problems.
  # Unparseable and missing birthdates will result in age = NULL and later on
  # in silent exclusion from further age-based analysis.

  query <- glue_sql("
      SELECT
        COUNT(*) FILTER (WHERE gebdat IS NULL) AS missing,
        COUNT(*) FILTER (WHERE gebdat IS NOT NULL
                           AND gebdat_normalized IS NULL) AS unparseable,
        COUNT(*) FILTER (WHERE LENGTH(TRIM(CAST(gebdat
                           AS VARCHAR))) = 4) AS year_only,
        COUNT(*) FILTER (WHERE LENGTH(TRIM(CAST(gebdat
                           AS VARCHAR))) = 7) AS year_month_only
      FROM patients
    ", .con = con)

  gebdat_quality <- dbGetQuery(con, query)

  writeLogData(message = "check birthdate: ",
               data = data.frame(metric = names(gebdat_quality),
                                 value = unlist(gebdat_quality)))

  # create base view with patient join.
  # This table is reused in all three parts
  path <- paste0(data_dir, "/", name_of_lab_csv)

  # it could happen that issued is missing in .csv. Check and make the
  # query save in case it happens

  issued_expr <- getColumnExpr(name_of_lab_csv, obs_issued, "TIMESTAMP")

  basedon_expr <- getColumnExpr(name_of_lab_csv, obs_basedon, "VARCHAR")

  types_clause <- getVarcharTypeClause(c(obs_value_code, obs_method_code))


  # core table with potassium patient join
  query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE potassium AS
        SELECT DISTINCT
          pot.{`obs_id`} AS obs_id,
          {`obs_loinc`} AS loinc,
          {`obs_value`} AS value,
          {`obs_value_unit`} AS unit,
          {`obs_value_code_system`} AS value_code_system,
          {`obs_value_code`} AS value_code,
          {`obs_reference_low`} AS ref_low,
          {`obs_reference_high`} AS ref_high,
          {`obs_reference_high_unit`} AS ref_high_unit,
          {`obs_reference_low_unit`} AS ref_low_unit,
          {`obs_value_comp`} AS comparator,
          {`obs_patient`} AS patient,
          {`obs_time`} AS time,
          {issued_expr} AS issued,
          {`obs_encounter`} AS encounter,
          {`obs_specimen`} AS specimen_ref,
          {basedon_expr} AS basedon,
          {`obs_method_code`} AS method_code,
          {`obs_method_system`} AS method_system,
          pat_id,
          gender,
          date_diff('year', pat.gebdat_normalized,
                    ({`obs_time`}::TIMESTAMP)::DATE) AS age
        FROM read_csv_auto({path}, {types_clause}) pot
        LEFT JOIN patients pat
        ON pot.{`obs_patient`} = concat('Patient/', pat.pat_id)
        WHERE ({`obs_status`} IS NULL
          OR NOT {`obs_status`} IN ('cancelled', 'entered-in-error'))
        AND {`obs_loinc_system`} = 'http://loinc.org'
        AND {`obs_loinc`} IN ({LOINCs_Kalium*})
        AND {`obs_time`} >= {global_min_time}
        AND {`obs_time`} <= {global_max_time}
      ", .con = con)

  count <- dbExecute(con, query)
  checkReference("potassium", "pat_id")
  checkForDuplicates("potassium", "obs_id", "loinc")

  writeLogData("Count raw base table: ", count)

  # drop patient table
  query <- glue_sql("
      DROP TABLE IF EXISTS patients;
    ", .con = con)
  dbExecute(con, query)
}

removeRepeatMeasurements <- function(table) {

  # get the count of measurements with more than 2 measurements at the same time
  query <- glue_sql("
      SELECT count(Distinct obs_id) as count
      FROM {table} p1
      WHERE (patient, time) IN (
        SELECT patient, time
        FROM {table}
        GROUP BY patient, time
        HAVING COUNT (DISTINCT obs_id) >= 2
      )
    ", .con = con)
  shared_timestamp <- dbGetQuery(con, query)$count

  shared_timestamp_loinc <- 0

  if(shared_timestamp > 0) {
    writeLogData("Measurements(ids) with same patient and timestamp: ",
                 shared_timestamp)

    query <- glue_sql("
      SELECT count(Distinct obs_id) as count
      FROM {table}  p1
      WHERE (patient, time) IN (
        SELECT patient, time
        FROM {table}
        GROUP BY patient, time, loinc
        HAVING COUNT (DISTINCT obs_id) >= 2
      )
    ", .con = con)
    shared_timestamp_loinc <- dbGetQuery(con, query)$count
    writeLogData("Measurements with same patient and timestamp and loinc: ",
                 shared_timestamp_loinc)

    query <- glue_sql("
      SELECT count(Distinct obs_id) as count
      FROM {table}  p1
      WHERE (patient, time) IN (
        SELECT patient, time
        FROM {table}
        GROUP BY patient, time, loinc
        HAVING COUNT (DISTINCT obs_id) >= 3
      )
    ", .con = con)
    shared_timestamp_loinc_3 <- dbGetQuery(con, query)$count
    writeLogData("More than two measurements with same patient and timestamp and loinc: ",
                 shared_timestamp_loinc_3)

    query <- glue_sql("
        SELECT
          COUNT(DISTINCT CASE WHEN issued IS NULL THEN obs_id END) AS issued_na,
          COUNT(DISTINCT CASE WHEN issued = time  THEN obs_id END) AS issued_identical,
          COUNT(DISTINCT CASE WHEN issued < time  THEN obs_id END) AS issued_before,
          COUNT(DISTINCT CASE WHEN issued > time  THEN obs_id END) AS issued_after
       FROM (
          SELECT *
          FROM {table}
          QUALIFY COUNT(*) OVER (PARTITION BY patient, time, loinc) >= 2
        ) sub
          ", .con = con)

    issued_comparison_problem <- dbGetQuery(con, query)
    writeLogData("Issued NA/same/before/after effectivedatetime (problematic rows): "
                 , issued_comparison_problem)

    # check if there are useful issued information. Detailed issued for
    # log is in part1
    query <- glue_sql("
      SELECT COUNT(DISTINCT obs_id) AS issued_after
      FROM potassium
      WHERE issued > time
    ", .con = con)

    issued_after <- dbGetQuery(con, query)$issued_after

    # if we have issued information try to clean data
    if(issued_after > 0) {

      query_lower_issued <- glue_sql("
        WITH duplicates AS (
          SELECT *,
                 MAX(issued) OVER (PARTITION BY patient, time, loinc) AS max_issued,
                 COUNT(*) OVER (PARTITION BY patient, time, loinc) AS n_dup
          FROM {table}
        )
        SELECT count(*)
        FROM duplicates
        WHERE n_dup >= 2
          AND issued < max_issued
      ", .con = con)

      lower_issued <- dbGetQuery(con, query_lower_issued)
      writeLogData("Count of removed data: ", lower_issued)

      query_lower_issued <- glue_sql("
        WITH duplicates AS (
          SELECT *,
                 MAX(issued) OVER (PARTITION BY patient, time, loinc) AS max_issued,
                 COUNT(*) OVER (PARTITION BY patient, time, loinc) AS n_dup
          FROM {table}
        )
        SELECT count(*)
        FROM duplicates
        WHERE n_dup >= 2
          AND issued >= max_issued
      ", .con = con)

      unclear_issued <- dbGetQuery(con, query_lower_issued)
      writeLogData("Count of remaining unclear data: ", unclear_issued)

      # create a new potassium table without the problematic measurements

      query_replace <- glue_sql("
        CREATE OR REPLACE TEMP TABLE {table} AS
        SELECT *
        FROM {table}
        QUALIFY NOT (
          COUNT(*) OVER (PARTITION BY patient, time, loinc) >= 2
          AND issued < MAX(issued) OVER (PARTITION BY patient, time, loinc)
        )
      ", .con = con)

      count <- dbExecute(con, query_replace)

      writeLogData("Count base table: ", count)
    }

  } else writeLogData("No measurements with shared timestamps")

}

removeExtremValues <- function(table) {
  # The idea is to remove only technical impossible values. Normal outliners
  # should be retained.
  # This calculates the MAD (median absolute deviation). A modified
  # z-score threshold of 10 (vs. the conventional 3.5 for general outlier detection)
  # is used to flag only extreme, implausible values.
  # The scaling factor 1.4826 makes MAD consistent with standard deviation.

  cutoff <- 10

  query <- glue_sql("
    WITH step1 AS (
        SELECT
          loinc, unit, value,
          MEDIAN(value) OVER (PARTITION BY loinc, unit) AS med_val
          FROM {table}
          ),
      step2 AS (
        SELECT
            loinc, unit, value,
            med_val,
            MEDIAN(ABS(value - med_val)) OVER (PARTITION BY loinc, unit) * 1.4826 AS mad_scaled
        FROM step1
      ),
      step3 AS (
          SELECT
              loinc, unit, value,
              med_val,mad_scaled,
              CASE
                  WHEN value IS NULL THEN FALSE
                  WHEN mad_scaled = 0 THEN FALSE
                  WHEN ABS(value - med_val) / mad_scaled > {cutoff} THEN TRUE
                  ELSE FALSE
              END AS is_implausible
          FROM step2
      )
      SELECT
          loinc,
          unit,
          COUNT(*) FILTER (WHERE is_implausible) AS n_implausible,
          MIN(value) FILTER (WHERE is_implausible) AS min_implausible,
          MAX(value) FILTER (WHERE is_implausible) AS max_implausible,
          MEDIAN(value) FILTER (WHERE is_implausible) AS median_implausible
      FROM step3
      GROUP BY loinc, unit;
      ", .con = con)

  implausible_values <- dbGetQuery(con, query)

  if (any(implausible_values$n_implausible > 0)) {
    implausible_rows <- implausible_values[implausible_values$n_implausible > 0, ]
    writeLogData("Found implausible values (loinc, unit, count, min, max, median): ")
    for (i in seq_len(nrow(implausible_rows))) {
      writeLogData(implausible_rows[i, ], kanonymity = FALSE)
    }

    # remove problematic data
    # mad_scaled NULL indicates that every value is NULL
    # mad_scaled = 0 indicates that too many values are the same as the median
    # resulting in a not useful test

    query_replace <- glue_sql("
      CREATE OR REPLACE TEMP TABLE {table} AS
        WITH step1 AS (
            SELECT
                *,
                MEDIAN(value) OVER (PARTITION BY loinc, unit) AS med_val
            FROM {table}
        ),
        step2 AS (
            SELECT
                *,
                MEDIAN(ABS(value - med_val)) OVER (PARTITION BY loinc, unit) * 1.4826 AS mad_scaled
            FROM step1
        )
        SELECT
            * EXCLUDE (med_val, mad_scaled)
        FROM step2
        WHERE value IS NULL
           OR mad_scaled IS NULL
           OR mad_scaled = 0
           OR ABS((value - med_val) / mad_scaled) <= {cutoff};
      ", .con = con)

    count <- dbExecute(con, query_replace)

    writeLogData("Count of final filtered table: ", count)

  } else {
    writeLogData("No implausible values found")
  }
}


loadUnitConversionFile <- function() {
  # load unit_conversion.csv
  unit_conversion <- read.csv("unit_conversion.csv", stringsAsFactors = FALSE)

  # get all rows with same label and source_unit
  dup_check <- unit_conversion[duplicated(
    unit_conversion[, c("label", "source_unit")]), c("label", "source_unit")]

  # if there are multiple rows per label and source_unit stop the skript
  if (nrow(dup_check) > 0) {

    writeLogData("ERROR: There are multiple rows for the same combination of label
                 and source_unit. Please fix unit_conversion.csv and try again.")
    writeLogData(paste0("Affected entries: ",
                        paste(sprintf("%s %s", dup_check$label, dup_check$source_unit),
                        collapse = ", ")))

    stop("Multiple rows for the same combination of label and source_unit.")
  }

  # stop is there are non-numeric factors (excluding true NA, which is a missing factor)
  non_numeric_factor <- unit_conversion[
    !is.na(unit_conversion$factor) &
    unit_conversion$target_unit != "invalid" &
    is.na(suppressWarnings(as.numeric(unit_conversion$factor))),
  ]
  if (nrow(non_numeric_factor) > 0) {
    writeLogData("ERROR: There are entries where factor is not numeric.
                 Please fix unit_conversion.csv and try again.")
    writeLogData(paste0("Affected entries: ",
                        paste(sprintf("%s %s", non_numeric_factor$label,
                                      non_numeric_factor$source_unit), collapse = ", ")))
    stop("Non-numeric factor.")
  }

  # stop if there are missing factors
  na_factor_check <- unit_conversion[
    is.na(unit_conversion$factor) & unit_conversion$target_unit != "invalid",
  ]

  if (nrow(na_factor_check) > 0) {

    writeLogData("ERROR: There are entries without a factor.
                 Please fix unit_conversion.csv and try again.")
    writeLogData(paste0("Affected entries: ",
                        paste(sprintf("%s %s", na_factor_check$label,
                          na_factor_check$source_unit), collapse = ", ")))

    stop("Missing factor.")
  }

  # check if there is only one target_unit for each label (except invalid)
  # get all rows without "invalid" as target_unit
  target_check <- unit_conversion[unit_conversion$target_unit != "invalid", ]
  # count distinct target_units per label
  target_per_label <- aggregate(
    target_unit ~ label, data = target_check,
    FUN = function(x) length(unique(x))
  )

  # labels with more than one distinct target_unit
  inconsistent_labels <- target_per_label$label[target_per_label$target_unit > 1]

  if (length(inconsistent_labels) > 0) {
    writeLogData("ERROR: target_unit is not unique per label (excluding 'invalid' entries).
               Please fix unit_conversion.csv and try again.")
    writeLogData(paste0("Affected labels: ", paste(inconsistent_labels, collapse = ", ")))
    stop("Inconsistent target_unit per label.")
  }

  # create table unit_conversion
  dbWriteTable(con, "unit_conversion", unit_conversion, overwrite = TRUE)

  writeLogData("File format is fine. Check units in data")

  checkAvailableUnits(unit_conversion)
}

checkAvailableUnits <- function(unit_conversion) {

  # get all relevant LOINCs
  all_loincs <- c(LOINCs_Kalium, LOINCs_bicarbonat, LOINCs_crea, LOINCs_glucose)

  path <- paste0(data_dir, "/", name_of_lab_csv)

  query <- glue_sql("
    SELECT DISTINCT unit, label FROM (
      SELECT DISTINCT
        {`obs_value_unit`} AS unit,
        CASE
          WHEN {`obs_loinc`} IN ({LOINCs_Kalium*})     THEN 'kalium'
          WHEN {`obs_loinc`} IN ({LOINCs_bicarbonat*}) THEN 'bicarbonat'
          WHEN {`obs_loinc`} IN ({LOINCs_crea*})       THEN 'crea'
          WHEN {`obs_loinc`} IN ({LOINCs_glucose*})    THEN 'glucose'
          ELSE NULL
        END AS label
      FROM read_csv_auto({path}) obs
      WHERE ({`obs_status`} IS NULL
         OR NOT {`obs_status`} IN ('cancelled', 'entered-in-error'))
      AND {`obs_loinc_system`} = 'http://loinc.org'
      AND {`obs_loinc`} IN ({all_loincs*})
      AND {`obs_time`} >= {global_min_time}
      AND {`obs_time`} <= {global_max_time}
      AND label IS NOT NULL
      AND unit IS NOT NULL
    ) t
    WHERE label IS NOT NULL AND unit IS NOT NULL
  ", .con = con)

  lab_units <- dbGetQuery(con, query)

  # compare if found units are in unit_conversion
  check <- lab_units %>%
    dplyr::left_join(
      unit_conversion,
      by = c("label" = "label", "unit" = "source_unit")
    )

  # stop if there are units which are not in unit_conversion.csv
  missing <- check %>%
    dplyr::filter(is.na(target_unit))

  if (nrow(missing) > 0) {
      writeLogData("ERROR: There are missing units in unit_conversion.csv")
      writeLogData(paste0("Affected entries: ",
                          paste(sprintf("%s %s", missing$label, missing$unit),
                                collapse = ", ")))
      writeLogData("Please add the missing units in unit_conversion.csv and
                   try again.")
      stop("Missing units in unit_conversion.csv")
  }

  # if there are units (in the data) marked as invalid write a note to log
  invalid <- check %>%
    dplyr::filter(target_unit == "invalid")

  if (nrow(invalid) > 0) {
    writeLogData(
      paste0("Note: Units marked as invalid: ",
      paste(sprintf("%s %s", invalid$label, invalid$unit), collapse = ", "))
    )
  }

  writeLogData("All units are defined")
}

checkConfig <- function() {

  pathLab <- paste0(data_dir, "/", name_of_lab_csv)
  pathPatient <- paste0(data_dir, "/", name_of_patient_csv)

  # patient and lab file are mandatory. Check if they are there.
  if (!file.exists(pathLab) || !file.exists(pathPatient)) {
    if(!file.exists(pathPatient)) {
      writeLogData(paste0("File ", name_of_patient_csv, " not found."))
      writeLogData("Please check your Input-files and contact us if you need help.")
    }
    if(!file.exists(pathLab)) {
      writeLogData(paste0("File ", name_of_lab_csv, " not found."))
      writeLogData("Please check your Input-files and contact us if you need help.")
    }
    stop("Mandatory files missing")
  }

  # helper function to do the check
  checkCsv <- function(filename, required_cols) {

    path <- paste0(data_dir, "/", filename)

    if (!file.exists(path)) {
      return(invisible(list(file = filename, exists = FALSE, missing = character(0))))
    }

    # read the colnames
    actual_cols <- colnames(dbGetQuery(con, glue_sql(
      "SELECT * FROM read_csv_auto({path}, ALL_VARCHAR = TRUE) LIMIT 0",
      .con = con
    )))

    missing_cols <- setdiff(required_cols, actual_cols)

    return(invisible(list(file = filename, exists = TRUE, missing = missing_cols)))
  }

  # define all needed columns

  lab_cols <- c(obs_loinc, obs_loinc_system, obs_value, obs_value_unit, obs_value_comp,
                obs_id, obs_reference_high, obs_reference_low, obs_reference_high_unit,
                obs_reference_low_unit, obs_time, obs_patient, obs_status,
                obs_interpretation, obs_value_code, obs_value_code_system, obs_specimen,
                obs_encounter, obs_method_code, obs_method_system)

  patient_cols <- c(pat_id, pat_gebdat, pat_gender)

  specimen_cols <- c(spec_id, spec_patient, spec_cond, spec_cond_system)

  encounter_cols <- c(enc_id, enc_patient, enc_class, enc_period_end, enc_period_start,
                      enc_type, enc_status, enc_type_system)

  procedure_cols <- c(pro_id, pro_patient, pro_status, pro_ops_code, pro_sno_code,
                      pro_period_start, pro_period_end, pro_performed, pro_encounter)

  procedure_icu_cols <- c(pro_id, pro_patient, pro_status, pro_ops_code, pro_sno_code,
                          pro_period_start, pro_period_end, pro_encounter)

  dialyse_cols <- c(obs_id, obs_time, obs_patient, obs_status,
                    obs_start, obs_end, obs_encounter)

  condition_cols <- c(con_id, con_patient, con_status, con_veri, con_icd, con_recorded, con_encounter)

  adm_base_cols <- c(adm_id, adm_patient, adm_status, adm_period_end, adm_period_start,
                     adm_effective, adm_enc)

  medadmCode_cols    <- c(adm_base_cols, adm_med_atc)
  medadmMedCode_cols <- c(adm_base_cols, adm_med_ref)
  medadmComplex_cols <- c(adm_base_cols, adm_med_ref)

  medication_cols <- c(med_id, med_atc, med_med_ref)


  # do the checks
  checks <- list(
    list(file = name_of_lab_csv,               cols = lab_cols),
    list(file = name_of_patient_csv,           cols = patient_cols),
    list(file = name_of_specimen_csv,          cols = specimen_cols),
    list(file = name_of_encounter_csv,         cols = encounter_cols),
    list(file = name_of_procedure_csv,         cols = procedure_cols),
    list(file = name_of_procedure_icu_csv,     cols = procedure_icu_cols),
    list(file = name_of_dauer_dialyse_csv,     cols = dialyse_cols),
    list(file = name_of_condition_csv,         cols = condition_cols),
    list(file = name_of_medadmCode_csv,        cols = medadmCode_cols),
    list(file = name_of_medadmMedCode_csv,     cols = medadmMedCode_cols),
    list(file = name_of_medadmComplex_csv,     cols = medadmComplex_cols),
    list(file = name_of_medication_csv,        cols = medication_cols)
  )

  results <- lapply(checks, function(x) {checkCsv(x$file, x$cols)})

  # make a list with all existing files
  existing <- Filter(function(x) x$exists, results)
  existing_files <- vapply(existing, function(x) x$file, character(1))

  writeLogData("Available input files:")
  for (x in existing_files) {
    writeLogData(x)
    }

  # Stop if there are missing columns in input files
  incomplete <- Filter(function(x) length(x$missing) > 0, existing)

  if (length(incomplete) > 0) {
    detail <- vapply(incomplete, function(x) {
      sprintf("%s: %s", x$file, paste(x$missing, collapse = ", "))
    }, character(1))

    writeLogData("Missing columns: ")
    for (x in incomplete) {
      writeLogData(sprintf("%s: %s", x$file, paste(x$missing, collapse = ", ")))
    }
    writeLogData("Please check if you are using the correct version of the
                 flatteningLookup.json in your DUP-Pipeline.")
    stop("Missing columns")
  } else {
    writeLogData("All nescesssary columns are available")
  }
}
