loadPotassiumData <- function() {

  # check if reference_units match with value units.
  query_unit_check <- glue_sql("
  SELECT
      unit,
      ref_low_unit,
      ref_high_unit,
      COUNT(*) AS n
    FROM potassium
    WHERE (unit IS NOT NULL AND ref_low_unit IS NOT NULL AND unit <> ref_low_unit)
       OR (unit IS NOT NULL AND ref_high_unit IS NOT NULL AND unit <> ref_high_unit)
    GROUP BY unit, ref_low_unit, ref_high_unit
    ORDER BY n DESC
  ", .con = con)

  unit_mismatches <- dbGetQuery(con, query_unit_check)

  if (nrow(unit_mismatches) > 0) {
    writeLogData("Warning: There are reference units that do not match with value unit"
                 , unit_mismatches)
  }

  # count values without unit
  query <- glue_sql("
    SELECT
      COUNT(*) AS n
      FROM potassium
      WHERE value is not null
      and unit is null
  ", .con = con)

  values_without_unit <- dbGetQuery(con, query)$n

  if (values_without_unit > 0) {
    writeLogData("Measurements excluded because of missing unit (but have a value): "
                 , values_without_unit)
  }

  # if there is strange stuff in units there is the possibility to mark
  # the unit as invalid. Filter those values out.
  query <- glue::glue_sql("
      SELECT source_unit
      FROM unit_conversion
      WHERE label = 'kalium'
        AND target_unit = 'invalid'
  ", .con = con)

  invalid_units <- DBI::dbGetQuery(con, query)$source_unit

  invalid_filter_sql <- if (length(invalid_units) > 0) {
    glue_sql("p.unit NOT IN ({invalid_units*})", .con = con)
  } else {
    glue_sql("1 = 1", .con = con)
  }

  # create raw table with value_norm and interpretation of result (L/N/H)
  query <- glue_sql("
      CREATE OR REPLACE TEMP TABLE potassium_result_raw AS
      WITH p AS (
        SELECT *,
        CASE WHEN TRIM(comparator) IN ('>', '>=', '<', '<=')
        THEN TRIM(comparator) ELSE NULL END AS comparator_norm
        FROM potassium
      )
      SELECT
          p.loinc, p.obs_id, p.patient, p.time, p.age, p.gender,
          p.value, p.unit,
          p.value * uc.factor AS value_norm,
        CASE
          WHEN p.comparator_norm IN ('<', '<=') THEN
            CASE
             WHEN (ref_low IS NOT NULL AND p.value <= ref_low)
              OR (ref_low IS NULL AND value_norm <= {kalium_ref$low}) THEN 'L'
             ELSE 'N'
            END
          WHEN p.comparator_norm IN ('>', '>=') THEN
            CASE
              WHEN (ref_high IS NOT NULL AND p.value >= ref_high)
               OR (ref_high IS NULL AND value_norm >= {kalium_ref$high}) THEN 'H'
              ELSE 'N'
            END
          WHEN (ref_low IS NOT NULL AND p.value < ref_low)
            OR (ref_low IS NULL AND value_norm < {kalium_ref$low}) THEN 'L'
          WHEN (ref_high IS NOT NULL AND p.value > ref_high)
            OR (ref_high IS NULL AND value_norm > {kalium_ref$high}) THEN 'H'
          ELSE 'N'
        END AS result
        FROM p
        LEFT JOIN unit_conversion uc
        ON uc.label = 'kalium' AND uc.source_unit = p.unit
        WHERE p.value IS NOT NULL
        AND p.unit IS NOT NULL
        AND {invalid_filter_sql}
        AND NOT (
          p.age IS NOT NULL AND p.age < 18
          AND (p.ref_low IS NULL OR p.ref_high IS NULL)
      )
    ", .con = con)
  dbExecute(con, query)

  # get all values that are not inside the plausibility range
  # Remember that in the beginning there was already a filter step based on MAD.

  query <- glue_sql("
    SELECT
      COUNT(*) AS n_implausible,
      MIN(value_norm) AS min_implausible,
      MAX(value_norm) AS max_implausible,
      MEDIAN(value_norm) AS median_implausible
    FROM potassium_result_raw
    WHERE value_norm >= {kalium_ref$high_ext}
    OR value_norm <= {kalium_ref$low_ext}
  ", .con = con)

  implausible_values <- dbGetQuery(con, query)

  if (implausible_values$n_implausible > 0) {
    writeLogData("Filtered implausible values (n, min, max, median): ")
    writeLogData(implausible_values, kanonymity = FALSE)
    writeLogData("Note that for potassium there was already a filter step at the beginning")
  }


  # base table for main analysis without loinc
  query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE potassium_result AS
      SELECT DISTINCT
        p.obs_id,
        p.patient,
        p.time,
        p.age,
        p.gender,
        p.value,
        p.unit,
        p.value_norm,
        p.result
      FROM potassium_result_raw p
      WHERE value_norm <= {kalium_ref$high_ext}
       AND value_norm >= {kalium_ref$low_ext}
    ", .con = con)

  count_result <- dbExecute(con, query)
  writeLogData("Total count of potassium values: ", count_result)

  # base table for blood/serum analysis
  query <- glue_sql("
    CREATE OR REPLACE TABLE potassium_result_loinc AS
      SELECT DISTINCT
        p.loinc,
        p.obs_id,
        p.patient,
        p.time,
        p.value_norm,
        p.result,
        CASE
          WHEN p.loinc IN ({LOINCs_kalium_serum*}) THEN 'serum'
          WHEN p.loinc IN ({LOINCs_kalium_blood*})  THEN 'blood'
          ELSE NULL
        END AS sample_type
      FROM potassium_result_raw p
      WHERE value_norm <= {kalium_ref$high_ext}
        AND value_norm >= {kalium_ref$low_ext}
    ", .con = con)
  dbExecute(con, query)

  query <- glue_sql("
    SELECT sample_type, count(*)
    FROM potassium_result_loinc
    GROUP BY sample_type
  ", .con = con)
  sample_type <- dbGetQuery(con, query)
  writeLogData("Number of potassium values per sample type: ", sample_type)

  # get counts of excluded values
  query <- glue_sql("
    SELECT 'without_value' AS kategorie, COUNT(*) AS n
    FROM potassium
    WHERE value IS NULL
    UNION ALL
    SELECT 'underage_without_reference', COUNT(*)
    FROM potassium
    WHERE age IS NOT NULL
      AND age < 18
      AND (ref_low IS NULL OR ref_high IS NULL)
  ", .con = con)

  exclusion_summary <- dbGetQuery(con, query)
  writeLogData("Values excluded from further evaluation: "
               , exclusion_summary)

  # get counts of measurements that have two different results at the same time
  query <- glue_sql("
     SELECT count(*)
      FROM (
       SELECT patient, time
       FROM potassium_result
       GROUP BY patient, time
       HAVING COUNT(*) > 1
      ) sub
    ", .con = con)

  multi_results <- dbGetQuery(con, query)
  writeLogData("Measurements with more than one result at the same time: ",
               multi_results[[1]])

  # get summary of potassium values
  query <- glue_sql("
     SELECT result, count(*) as n
     FROM potassium_result
     GROUP BY result
    ", .con = con)

  results <- dbGetQuery(con, query)
  writeLogData("potassium_measurements by result: ", results)

  query <- glue_sql("
      DROP TABLE IF EXISTS potassium_result_raw;
    ", .con = con)
  dbExecute(con, query)

  # if there are no lines in the resulting table, return FALSE
  if (count_result > 0) {
    return(TRUE)
  } else return(FALSE)
}

loadEncounter <- function() {

  writeLog("Loading encounter data if available")

  encounter_available <- file.exists(paste0(data_dir, "/", name_of_encounter_csv))

  # if there is no encounter file return
  if(!encounter_available) {
    writeLogData("Note: no encounters available")
    return (FALSE)
  }

  path <- paste0(data_dir, "/", name_of_encounter_csv)

  # get all encounters. Filter for enc_type_system if available
  query <- glue_sql("
       CREATE OR REPLACE TEMP TABLE encounter_all AS
            SELECT DISTINCT
            enc.{`enc_id`} AS enc_id,
            enc.{`enc_patient`} AS patient,
            enc.{`enc_class`} AS class,
            enc.{`enc_period_start`} AS period_start,
            enc.{`enc_period_end`} AS period_end,
            enc.{`enc_type`} AS type
      FROM read_csv_auto({path}) enc
      WHERE (enc.{`enc_status`} IS NULL
        OR NOT enc.{`enc_status`} IN ('planned', 'cancelled', 'entered-in-error'))
      AND (enc.{`enc_type_system`} IS NULL OR {`enc_type_system`} = 'http://fhir.de/CodeSystem/Kontaktebene')
      ", .con = con)
  dbExecute(con, query)

  checkForDuplicates("encounter_all","enc_id")

  # get the available types with counts:
  query <- glue_sql("
      SELECT
      type, class, count(*) as count
      FROM encounter_all
      GROUP BY type, class
      ORDER BY type, class
      ", .con = con)

  types <- dbGetQuery(con, query)

  writeLogData("Available encounter types: ", types)

  writeLogData("Note: encounters without type are considered to be einrichtungskontakte")

  # create a view for einrichtungskontakt encounter. Also add a time column
  # that contains period_start (for timeline)
  dbExecute(con, "
    CREATE OR REPLACE TEMP VIEW encounter_main AS
    SELECT *,
    period_start AS time
    FROM encounter_all
    WHERE type IS NULL OR type = 'einrichtungskontakt';
  ")

  checkForDuplicates("encounter_main", "enc_id")

  writeLogData("Encounter loaded successfully")

  return(TRUE)
}

loadMedicationData <- function() {

  writeLog("Loading medication data if available")

  # get availability of medication files

  # medications that are one-hop referenced by an Administration
  medication_available <- file.exists(paste0(data_dir, "/", name_of_medication_csv))
  # administrations with ATC in on-hop referenced medications
  medadm_med_available <- file.exists(paste0(data_dir, "/", name_of_medadmMedCode_csv))
  # administrations with ATC in codeableconcept
  medadm_code_available <- file.exists(paste0(data_dir, "/", name_of_medadmCode_csv))
  # administrations with ATC in two-hop referenced medications
  medadm_complex_available <- file.exists(paste0(data_dir, "/", name_of_medadmComplex_csv))


  # check if any medication data is there
  if (!medication_available  && !medadm_med_available &&
      !medadm_code_available && !medadm_complex_available) {
    writeLogData("Note: no medication ressources available")
    return(FALSE)
  }

  # check if there is only medication but no administrations
  if (medication_available &&
      !medadm_med_available && !medadm_code_available && !medadm_complex_available) {
    writeLogData("Note: medication available, but no medication administrations")
    return(FALSE)
  }

  table_medadm_med_exists <- FALSE
  table_medadm_code_exists <- FALSE
  table_madadm_complex_exists <- FALSE

  if (medication_available) {

    writeLog("Loading medication")

    path_med <- paste0(data_dir, "/", name_of_medication_csv)

    query <- glue_sql("
      CREATE OR REPLACE TEMP TABLE medication AS
            SELECT DISTINCT
            med.{`med_id`} AS med_id,
            med.{`med_atc`} AS atc,
            med.{`med_med_ref`} AS med_med_ref,
            concat('Medication/', med.{`med_id`}) AS med_ref_full
      FROM read_csv_auto({path_med}) med;
      ", .con = con)
    count <- dbExecute(con, query)
    writeLogData("Total medication rows: ", count)

    query <- glue_sql("
      SELECT count(distinct med_id)
      FROM medication
      ", .con = con)

    count_medications <- dbGetQuery(con, query)
    writeLogData("Total medication-ids: ", count_medications)

    query <- glue_sql("
      SELECT count(distinct med_id)
      FROM medication
      WHERE ATC IS NOT NULL
      ", .con = con)

    count_medatc <- dbGetQuery(con, query)
    writeLogData("Total medications with any atc: ", count_medatc)

    checkForDuplicates("medication", "med_id", "atc")
  }

  # Administrations with ATC in referenced medications
  if(medadm_med_available) {

    if(medication_available) {
      writeLog("Loading administrations with one-hop referenced medications")

      path_adm <- paste0(data_dir, "/", name_of_medadmMedCode_csv)

      query <- glue_sql("
         CREATE OR REPLACE TEMP TABLE adm_join AS
              SELECT DISTINCT
              adm.{`adm_id`} AS adm_id,
              adm.{`adm_patient`} AS patient,
              adm.{`adm_period_start`} AS period_start,
              adm.{`adm_period_end`} AS period_end,
              adm.{`adm_effective`} AS effective,
              adm.{`adm_enc`} AS encounter,
              adm.{`adm_med_ref`} AS med_ref
        FROM read_csv_auto({path_adm}) adm
        WHERE adm.{`adm_status`} IS NULL
          OR NOT adm.{`adm_status`} IN ('not-done', 'on-hold', 'entered-in-error', 'stopped');
        ", .con = con)
      dbExecute(con, query)

      query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE medadm_med AS
            SELECT
                adm.adm_id,
                adm.patient,
                adm.period_start,
                adm.period_end,
                adm.effective,
                adm.encounter,
                med.med_id,
                CAST(NULL AS VARCHAR) AS med_id2,
                med.atc,
                'medadm_med' AS source
            FROM adm_join adm
            LEFT JOIN medication med
            ON adm.med_ref = med.med_ref_full;
        ", .con = con)
      count <- dbExecute(con, query)

      writeLogData("Total one-hop administrations: ", count)

      checkForDuplicates("medadm_med", "adm_id", "atc")
      checkReference("medadm_med", "med_id")

      table_medadm_med_exists <- TRUE
      # Drop initial tables
      query <- glue_sql("
        DROP TABLE IF EXISTS adm_join;
      ", .con = con)
      dbExecute(con, query)

    } else {
      writeLogData ("Note: There is a file for MedicationAdministrations
                    referencing medications. But there is no .csv containing
                    those medications.")
    }
  }

  # Administrations with ATC in Codeableconcept
  if(medadm_code_available) {

    writeLog("Loading administrations with ATC in codeableconcept")

    path_adm <- paste0(data_dir, "/", name_of_medadmCode_csv)

    query <- glue_sql("
          CREATE OR REPLACE TEMP TABLE medadm_code AS
          SELECT DISTINCT
          adm.{`adm_id`} AS adm_id,
          adm.{`adm_patient`} AS patient,
          adm.{`adm_period_start`} As period_start,
          adm.{`adm_period_end`} As period_end,
          adm.{`adm_effective`} AS effective,
          adm.{`adm_enc`} AS encounter,
          CAST(NULL AS VARCHAR) AS med_id,
          CAST(NULL AS VARCHAR) AS med_id2,
          adm.{`adm_med_atc`} AS atc,
          'medadm_code' AS source
          FROM read_csv_auto({path_adm}) adm
          WHERE adm.{`adm_status`} IS NULL
            OR NOT adm.{`adm_status`} IN ('not-done', 'on-hold', 'entered-in-error', 'stopped')
        ", .con = con)
    count <- dbExecute(con, query)
    writeLogData("Total codeableconcept administrations: ", count)

    checkForDuplicates("medadm_code", "adm_id", "atc")

    table_medadm_code_exists <- TRUE
  }

  # Administrations with ATC in two hop medications
  # if there is an ATC code in the middle section that one is also
  # considered
  if(medadm_complex_available) {

    if(medication_available){

      writeLog("Loading administrations with two-hop referenced medications")

      path_adm <- paste0(data_dir, "/", name_of_medadmComplex_csv)

      query <- glue_sql("
         CREATE OR REPLACE TEMP TABLE adm_join AS
              SELECT DISTINCT
              adm.{`adm_id`} AS adm_id,
              adm.{`adm_patient`} AS patient,
              adm.{`adm_period_start`} AS period_start,
              adm.{`adm_period_end`} AS period_end,
              adm.{`adm_effective`} AS effective,
              adm.{`adm_enc`} AS encounter,
              adm.{`adm_med_ref`} AS med_ref
        FROM read_csv_auto({path_adm}) adm
        WHERE adm.{`adm_status`} IS NULL
         OR NOT adm.{`adm_status`} IN ('not-done', 'on-hold', 'entered-in-error', 'stopped');
        ", .con = con)
      dbExecute(con, query)

      # get the first hop
      query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE adm_med1 AS
            SELECT
                adm.adm_id,
                adm.patient,
                adm.period_start,
                adm.period_end,
                adm.effective,
                adm.encounter,
                med1.med_id AS med_id,
                med1.atc AS atc1,
                med1.med_med_ref AS med_med_ref
            FROM adm_join adm
            LEFT JOIN medication med1
            ON adm.med_ref = med1.med_ref_full;
        ", .con = con)
      dbExecute(con, query)

      # get the atc from first hop (if it exists) and get the medication
      # from the second hop
      query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE medadm_complex AS
            SELECT
                adm_id, patient, period_start, period_end, effective, encounter,
                med_id, CAST(NULL AS VARCHAR) AS med_id2, atc1 AS atc,
                'medadm_complex' AS source
            FROM adm_med1
            WHERE atc1 IS NOT NULL

            UNION ALL BY NAME

            SELECT
                adm_med1.adm_id, adm_med1.patient, adm_med1.period_start,
                adm_med1.period_end, adm_med1.effective, adm_med1.encounter,
                adm_med1.med_id, med2.med_id AS med_id2, med2.atc AS atc,
                'medadm_complex' AS source
            FROM adm_med1
            LEFT JOIN medication med2
            ON adm_med1.med_med_ref = med2.med_ref_full
            WHERE adm_med1.med_med_ref IS NOT NULL;
        ", .con = con)
      count <- dbExecute(con, query)

      writeLogData("Total two-hop administrations: ", count)

      checkReference("medadm_complex", "med_id")
      checkReference("medadm_complex", "med_id2")

      table_madadm_complex_exists <- TRUE

      # Drop initial tables
      query <- glue_sql("
        DROP TABLE IF EXISTS adm_join;
        DROP TABLE IF EXISTS adm_med1;
      ", .con = con)
      dbExecute(con, query)
    } else {
      writeLogData ("Note: There is a file for administrations referencing two
                    hops medications. But there is no .csv containing those
                    medications.")
    }
  }

  # get a list with all existing tables
  writeLog("Joining all medication data in one table")

  union_parts <- character()

  # create a select for every existing table and store it in union_parts
  if (table_medadm_med_exists) {
    union_parts <- c(union_parts, "SELECT * FROM medadm_med")
  }

  if (table_medadm_code_exists) {
    union_parts <- c(union_parts, "SELECT * FROM medadm_code")
  }

  if (table_madadm_complex_exists) {
    union_parts <- c(union_parts, "SELECT * FROM medadm_complex")
  }

  if (length(union_parts) > 0) {

    # combine all existing tables in one medadm_all table
    query <- glue::glue_sql(
      "CREATE OR REPLACE TEMP TABLE medadm_all AS {SQL(paste({union_parts},
          collapse = ' UNION ALL BY NAME '))}",
      .con = con
    )

    total_count <- dbExecute(con, query)

    checkForDuplicates("medadm_all", "adm_id", "atc")

    #delete the initial tables
    query <- glue_sql("
      DROP TABLE IF EXISTS medadm_med;
      DROP TABLE IF EXISTS medadm_code;
      DROP TABLE IF EXISTS medadm_complex;
    ", .con = con)
    dbExecute(con, query)

    # create a view that gives as time effective, if not available than
    # period_end, if that is also null than period_start.
    # CAST is needed because if one of those fields is always NULL it will
    # be of type VARCHAR.

    dbExecute(con, "
      CREATE OR REPLACE TEMP VIEW medadm_all_end AS
      SELECT *,
         COALESCE(
           CAST(effective    AS TIMESTAMPTZ),
           CAST(period_end   AS TIMESTAMPTZ),
           CAST(period_start AS TIMESTAMPTZ)
         ) AS time
      FROM medadm_all
    ")

    # and view that gives effective or period_start
    dbExecute(con, "
      CREATE OR REPLACE TEMP VIEW medadm_all_start AS
      SELECT *,
         COALESCE(
           CAST(effective AS TIMESTAMPTZ),
           CAST(period_start AS TIMESTAMPTZ)
         ) AS time
      FROM medadm_all
    ")

    writeLogData("Medication loaded successfully. Total count: ", total_count)
    return (TRUE)

  } else {
    writeLogData("Note: no medication data available")
    return (FALSE)
  }
}

loadLabMain <- function() {

  writeLog("Loading further labresults")

  # load all lab-values as separate tables
  loadLab("glucose", LOINCs_glucose)
  loadLab("bicarbonat", LOINCs_bicarbonat)
  loadLab("pH", LOINCs_pH, care_about_unit = FALSE)
  loadLab("crea", LOINCs_crea)
  loadLab("GFR", LOINCs_GFR, care_about_unit = FALSE)

  writeLog("Joining all Lab-values in one table")

  # build one big table
  query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE lab AS
        SELECT * FROM glucose
        UNION ALL
        SELECT * FROM bicarbonat
        UNION ALL
        SELECT * FROM pH
        UNION ALL
        SELECT * FROM crea
        UNION ALL
        SELECT * from GFR
      ", .con = con)
  count <- dbExecute(con, query)

  # drop single tables
  query <- glue_sql("
      DROP TABLE IF EXISTS glucose;
      DROP TABLE IF EXISTS bicarbonat;
      DROP TABLE IF EXISTS pH;
      DROP TABLE IF EXISTS crea;
      DROP TABLE IF EXISTS GFR;
    ", .con = con)
  dbExecute(con, query)

  if(count > 0) {
    writeLogData("Labresults loaded successfully. Total count: ", count)
    return(TRUE)
  } else {
    writeLogData("No further lab results available")
    return (FALSE)
    }
}

loadLab <- function(name, loincs, care_about_unit = TRUE) {

  writeLog(paste0("Loading ", name))

  path <- paste0(data_dir, "/", name_of_lab_csv)

  name_raw <- paste0(name, "_raw")

  issued_expr <- getColumnExpr(name_of_lab_csv, obs_issued, "TIMESTAMP")

  types_clause <- getVarcharTypeClause(c(obs_value_code))

  query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE {name_raw} AS
        SELECT DISTINCT
          o.{`obs_id`} AS obs_id,
          o.{`obs_loinc`} AS loinc,
          o.{`obs_value`} AS value,
          o.{`obs_value_unit`} AS unit,
          o.{`obs_reference_low`} AS ref_low,
          o.{`obs_reference_high`} AS ref_high,
          o.{`obs_reference_high_unit`} AS ref_high_unit,
          o.{`obs_reference_low_unit`} AS ref_low_unit,
          o.{`obs_value_comp`} AS comparator,
          o.{`obs_patient`} AS patient,
          o.{`obs_value_code`} AS codeableconcept,
          o.{`obs_time`} AS time,
          o.{issued_expr} AS issued,
          o.{`obs_encounter`} AS encounter,
          {name} AS label
        FROM read_csv_auto({path}, {types_clause}) o
        WHERE (o.{`obs_status`} IS NULL
          OR NOT o.{`obs_status`} IN ('cancelled', 'entered-in-error'))
        AND o.{`obs_loinc_system`} = 'http://loinc.org'
        AND o.{`obs_loinc`} IN ({loincs*})
        AND o.{`obs_time`} >= {global_min_time}
        AND o.{`obs_time`} <= {global_max_time}
      ", .con = con)
  count <- dbExecute(con, query)

  writeLogData(paste0("Number of rows in ", name_raw, ": "), count)

  removeRepeatMeasurements(name_raw)

  if (count > 0) {
    checkForDuplicates(name_raw, "obs_id", "loinc")
  }

  # check if reference_units match with value units.
  query_unit_check <- glue_sql("
  SELECT
      unit,
      ref_low_unit,
      ref_high_unit,
      COUNT(*) AS n
    FROM {name_raw}
    WHERE (unit IS NOT NULL AND ref_low_unit IS NOT NULL AND unit <> ref_low_unit)
       OR (unit IS NOT NULL AND ref_high_unit IS NOT NULL AND unit <> ref_high_unit)
    GROUP BY unit, ref_low_unit, ref_high_unit
    ORDER BY n DESC
  ", .con = con)

  unit_mismatches <- dbGetQuery(con, query_unit_check)

  if (nrow(unit_mismatches) > 0) {
    writeLogData(paste0("Warning: In ", name,
                        " are reference units that do not match with value unit")
                 , unit_mismatches)
  }

  # count values without unit
  query <- glue_sql("
    SELECT
      COUNT(*) AS n
      FROM {name_raw}
      WHERE value is not null
      and unit is null
  ", .con = con)

  values_without_unit <- dbGetQuery(con, query)

  if (values_without_unit[[1]] > 0) {
    writeLogData("Measurements with value but missing unit: "
                 , values_without_unit)
  }

  # get some information
  query <- glue_sql("
    SELECT loinc, unit, COUNT(*) AS n,
     COUNT(*) FILTER (WHERE ref_high IS NOT NULL) AS n_ref_high,
     COUNT(*) FILTER (WHERE ref_low IS NOT NULL) AS n_ref_low
      FROM {name_raw}
      GROUP BY loinc, unit
  ", .con = con)

  loincs_summary <- dbGetQuery(con, query)

  writeLogData(paste0("Found LOINCs and units in ", name_raw, ": "))
  writeLogData("LOINC/unit/count/count_ref_high/count_ref_low ", loincs_summary)

  query <- glue_sql("
    SELECT loinc, unit, COUNT(*) AS n
      FROM {name_raw}
      WHERE comparator is not NULL
      GROUP BY loinc, unit
  ", .con = con)
  comparator_summary <- dbGetQuery(con, query)

  if (nrow(comparator_summary) > 0) {
   writeLogData(paste0("Measurements with comparator in ", name, ": "), comparator_summary)
  }

  # get the default reference_values
  default <- get(paste0(name, "_ref"))

  # get the count of measurement without value
  query <- glue_sql("
    SELECT COUNT(*) AS n
      FROM {name_raw}
      WHERE value IS NULL
  ", .con = con)
  no_value <- dbGetQuery(con, query)

  if(no_value > 0) {
    writeLogData("Measurements excluded because of missing value: "
                 , no_value)
  }

  # get counts of measurements that have two different results at the same time
  query <- glue_sql("
     SELECT count(*) as n
      FROM (
       SELECT patient, time
       FROM {name_raw}
       GROUP BY patient, time
       HAVING COUNT(*) > 1
      ) sub
    ", .con = con)

  multi_results <- dbGetQuery(con, query)$n
  if(multi_results > 0) {
    writeLogData("Measurements with more than one result at the same time (final table): ",
                 multi_results)
  }


  # get all invalid rows. This is done for all label (also the ones that
  # dont care about units)
  query <- glue_sql("
      SELECT source_unit
      FROM unit_conversion
      WHERE label = {name}
        AND target_unit = 'invalid'
  ", .con = con)

  invalid_units <- DBI::dbGetQuery(con, query)$source_unit

  invalid_filter_sql <- if (length(invalid_units) > 0) {
    glue_sql("o.unit NOT IN ({invalid_units*})", .con = con)
  } else {
    glue_sql("1 = 1", .con = con)
  }

  # get the number of rows with invalid units. Those are excluded in the
  # next step
  query <- glue_sql("
      SELECT count(*) as n
      FROM {name_raw}
      WHERE unit in ({invalid_units*})
  ", .con = con)

  count_invalid <- DBI::dbGetQuery(con, query)$n

  if(count_invalid > 0) {
    writeLogData("Excluded measurements because of invalid unit: ",
                 count_invalid)
  }

  # assign L/N/H.
  # If there are no ref-ranges at all the measurement will be discarded.
  # Only one ref range (high or low) is fine.
  unit_filter_sql <- if (care_about_unit) {
    glue_sql("o.unit IS NOT NULL", .con = con)
  } else {
    glue_sql("1 = 1", .con = con)
  }

  query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE {name} AS
    WITH o AS (
      SELECT *,
      CASE WHEN TRIM(comparator) IN ('>', '>=', '<', '<=')
      THEN TRIM(comparator) ELSE NULL END AS comparator_norm
      FROM {name_raw}
    )
    SELECT DISTINCT
      o.patient,
      o.time,
      o.value,
      o.encounter,
      CASE WHEN {care_about_unit}
            THEN o.value * uc.factor
            ELSE o.value
      END AS value_norm,
      CASE
        WHEN o.comparator_norm IN ('<', '<=') THEN
          CASE
            WHEN (o.ref_low IS NOT NULL AND o.value <= ref_low)
              OR (o.ref_low IS NULL AND value_norm <= {default$low}) THEN 'L'
            ELSE 'N'
          END
        WHEN o.comparator_norm IN ('>', '>=') THEN
          CASE
            WHEN (o.ref_high IS NOT NULL AND o.value >= ref_high)
              OR (o.ref_high IS NULL AND value_norm >= {default$high}) THEN 'H'
            ELSE 'N'
          END
        WHEN (o.ref_low IS NOT NULL AND o.value < ref_low)
          OR (o.ref_low IS NULL AND value_norm < {default$low}) THEN 'L'
        WHEN (o.ref_high IS NOT NULL AND o.value > ref_high)
          OR (o.ref_high IS NULL AND value_norm > {default$high}) THEN 'H'
        ELSE 'N'
      END AS result,
      o.label
    FROM o
    LEFT JOIN unit_conversion uc
      ON uc.label = o.label
      AND uc.source_unit = o.unit
    WHERE value IS NOT NULL
    AND {unit_filter_sql}
    AND {invalid_filter_sql}
    AND (
        COALESCE(o.ref_low, {default$low}) IS NOT NULL
        OR COALESCE(o.ref_high, {default$high}) IS NOT NULL
    )
  ", .con = con)
  count <- dbExecute(con, query)


  # get values that are not inside the plausibility range
  query <- glue_sql("
    SELECT
      COUNT(*) AS n_implausible,
      MIN(value_norm) AS min_implausible,
      MAX(value_norm) AS max_implausible,
      MEDIAN(value_norm) AS median_implausible
    FROM {name}
    WHERE value_norm >= {default$high_ext}
    OR value_norm <= {default$low_ext}
  ", .con = con)

  implausible_values <- dbGetQuery(con, query)

  if (implausible_values$n_implausible > 0) {
    writeLogData("Filtered implausible values (n, min, max, median): ")
    writeLogData(implausible_values, kanonymity = FALSE)

    query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE {name} AS
      SELECT *
      FROM {name}
      WHERE value_norm <= {default$high_ext}
       AND value_norm >= {default$low_ext}
    ", .con = con)

    count <- dbExecute(con, query)
  }


  writeLogData(paste0("Number of final ", name, " values: "), count)

  query <- glue_sql("
      DROP TABLE IF EXISTS {`name_raw`};
    ", .con = con)
  dbExecute(con, query)
}

loadProcedures <- function() {

    writeLog("Loading procedures")

    procedures_available <- file.exists(paste0(data_dir, "/", name_of_procedure_csv))
    procedures_icu_available <- file.exists(paste0(data_dir, "/", name_of_procedure_icu_csv))
    procedures_icu_dauer_available <- file.exists(paste0(data_dir, "/", name_of_dauer_dialyse_csv))

    if(!procedures_available && !procedures_icu_available && !procedures_icu_dauer_available) {
      writeLogData("No procedures available")
      return(FALSE)
    }

    createProcedureTable <-function(name, path, icu = FALSE) {

      # the given OPS-codes are just the first patterns, so we need a OR list with
      # starts_with().
      ops_conditions <- DBI::SQL(glue_sql_collapse(
        glue_sql("starts_with({`pro_ops_code`}, {OPS_codes})", .con = con),
        sep = " OR "
      ))

      # in icu profile there is no performed datetime
      performed_sql <- if (icu) {
        glue_sql("NULL::TIMESTAMP", .con = con)
      } else {
        glue_sql("{`pro_performed`}", .con = con)
      }

      types_clause <- getVarcharTypeClause(c(pro_sno_code, pro_ops_code))

      query <- glue_sql("
            CREATE OR REPLACE TEMP TABLE {name} AS
            SELECT DISTINCT
              {`pro_id`} AS pro_id,
              {`pro_patient`} AS patient,
              {`pro_period_start`} AS period_start,
              {`pro_period_end`} AS period_end,
              {performed_sql} AS performed,
              {`pro_encounter`} AS encounter,
              CASE
                  WHEN {`pro_ops_code`} IS NOT NULL AND {`pro_sno_code`} IS NOT NULL THEN 'both'
                  WHEN {`pro_ops_code`} IS NOT NULL THEN 'ops'
                  WHEN {`pro_sno_code`} IS NOT NULL THEN 'snomed'
              END AS code_type
            FROM read_csv_auto({path},{types_clause}) pro
            WHERE ( {`pro_status`} IS NULL
              OR NOT {`pro_status`} IN ('stopped', 'entered-in-error', 'on-hold', 'preparation', 'not-done'))
            AND (
              ({ops_conditions}) OR {`pro_sno_code`} IN ({dialyse_snomed*})
            )
          ", .con = con)

      count <- dbExecute(con, query)
      writeLogData(paste0("Total count of ", name, " table: "), count)
    }

    union_parts <- character()

    if(procedures_available) {
      path <- paste0(data_dir, "/", name_of_procedure_csv)
      createProcedureTable("procedure_raw", path, icu = FALSE )
      union_parts <- c(union_parts, "SELECT * FROM procedure_raw")
    }

    if(procedures_icu_available) {
      path <- paste0(data_dir, "/", name_of_procedure_icu_csv)
      createProcedureTable("procedure_icu_raw", path, icu = TRUE)
      union_parts <- c(union_parts, "SELECT * FROM procedure_icu_raw")
    }

    if(procedures_icu_dauer_available) {
      path <- paste0(data_dir, "/", name_of_dauer_dialyse_csv)

      # we want to union with other procedures, so we need to name it like
      # a procedure
      query <- glue_sql("
            CREATE OR REPLACE TEMP TABLE dauer_dialyse AS
            SELECT DISTINCT
              {`obs_id`} AS pro_id,
              {`obs_patient`} AS patient,
              {`obs_start`} AS period_start,
              {`obs_end`} AS period_end,
              {`obs_time`} AS performed,
              {`obs_encounter`} AS encounter,
              'icu_dauer' AS code_type
            FROM read_csv_auto({path}) pro
            WHERE ({`obs_status`} IS NULL
              OR NOT {`obs_status`} IN ('registered', 'preliminary', 'cancelled','entered-in-error'))
          ", .con = con)

      count <- dbExecute(con, query)
      writeLogData("Total count of dauer_dialyse table: ", count)
      union_parts <- c(union_parts, "SELECT * FROM dauer_dialyse")
    }

    # at this point this should always be > 0
    if (length(union_parts) > 0) {

      # combine all existing tables in one table
      query <- glue::glue_sql(
        "CREATE OR REPLACE TEMP TABLE procedures AS {SQL(paste({union_parts},
          collapse = ' UNION ALL BY NAME '))}",
        .con = con
      )
      total_count <- dbExecute(con, query)
      writeLogData("Total count: ", total_count)

      checkForDuplicates("procedures", "pro_id")

      query <- glue_sql("
      SELECT code_type, count(*)
        FROM procedures
        GROUP BY code_type
      ", .con = con)
      count_per_type <- dbGetQuery(con, query)

      writeLogData("Counts per code_type: ", count_per_type)

      #delete the initial tables
      query <- glue_sql("
      DROP TABLE IF EXISTS procedure_raw;
      DROP TABLE IF EXISTS procedure_icu_raw;
      ", .con = con)
      dbExecute(con, query)

      # create a view with time
      dbExecute(con, "
      CREATE OR REPLACE TEMP VIEW procedures_start AS
      SELECT *,
         COALESCE(
           CAST(performed    AS TIMESTAMPTZ),
           CAST(period_start  AS TIMESTAMPTZ)
         ) AS time
      FROM procedures
      ")

      return(TRUE)
    } else {
      writeLogData("No procedures available")
      return(FALSE)
    }
}

loadConditions <- function() {
  writeLog("Loading conditions")
  conditions_available <- file.exists(paste0(data_dir, "/", name_of_condition_csv))

  if(!conditions_available) {
    writeLogData("No conditions available")
    return(FALSE)
  }

  path <- paste0(data_dir, "/", name_of_condition_csv)

  # status and verifaction NULL is possible because it is not required.
  query <- glue_sql("
            CREATE OR REPLACE TEMP TABLE conditions AS
            SELECT DISTINCT
              {`con_id`} AS con_id,
              {`con_patient`} AS patient,
              {`con_recorded`} AS time,
              {`con_encounter`} AS encounter,
              {`con_icd`} AS icd
            FROM read_csv_auto({path}) c
            WHERE ({`con_status`} IS NULL
            OR NOT {`con_status`} IN ('inactive', 'remission', 'resolved'))
            AND   ({`con_veri`} IS NULL
            OR NOT {`con_veri`} IN ('unconfirmed','provisional','differential',
                                                  'refuted','entered-in-error'))
          ", .con = con)

  count <- dbExecute(con, query)

  checkForDuplicates("conditions", "con_id", "icd")

  writeLogData(paste0("Total count of condition table: "), count)
  return(TRUE)
}
