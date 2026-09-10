
evaluateSampleQuality <- function() {

  writeLogHeader("Part2: Sample-Quality analysis.",
                  "This part evaluates if sample-quality information
                  is present and if it can be linked to potassium data. Besides
                  potassium observations this part relies on, if available,
                  referenced specimen and observations with quality results")

  # variable to store the information if we have specimen
  specimen_available <- FALSE

  writeLog("Create tables for sample-quality analysis")
  specimen_available <- createTablesForSampleQuality()

  writeLog("Evaluate quality-LOINCs")
  quality_loincs_available <- evaluateQualityLOINCs()

  writeLog("Evaluate non-numeric results")
  analyseCodeResults()

  writeLog("Evaluate notes")
  free_text <- evaluateNotes(quality_loincs_available)

  if(specimen_available) {
    writeLog("Analyse specimen referenced from observation")
    analyseSpecimen()
  }

  writeLog("Evaluate methods")
  evaluateMethods()

  cleanUpSampleQuality()
  return(free_text)
}

createTablesForSampleQuality <- function() {

  path <- paste0(data_dir, "/", name_of_lab_csv)

  basedOn_expr <- getColumnExpr(name_of_lab_csv, obs_basedon, "VARCHAR")

  types_clause <- DBI::SQL(sprintf("types={'%s': 'VARCHAR'}", obs_value_code))

  # create a table with all available quality loincs
  query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE quality_data AS
        SELECT DISTINCT
          {`obs_loinc`} AS loinc,
          {`obs_value`} AS value,
          {`obs_value_unit`} AS unit,
          {`obs_value_code`} AS value_code,
          {`obs_value_code_system`} AS value_code_system,
          {`obs_id`} AS obs_id,
          {`obs_time`} AS time,
          {`obs_patient`} AS patient,
          {`obs_interpretation`} AS interpretation,
          {basedOn_expr} AS basedon
        FROM read_csv_auto({path},{types_clause})
        WHERE {`obs_loinc_system`} = 'http://loinc.org'
        AND {`obs_loinc`} IN ({LOINCs_Quality*})
        AND {`obs_time`} >= {global_min_time}
        AND {`obs_time`} <= {global_max_time}
        AND ({`obs_status`} IS NULL
          OR NOT {`obs_status`} IN ('cancelled', 'entered-in-error'))
      ", .con = con)
  dbExecute(con, query)

  checkForDuplicates("quality_data", "obs_id", "loinc")


  # if there are available specimens create a table for them. If not
  # write a note to log
  path <- paste0(data_dir, "/", name_of_specimen_csv)
  available_specimen <- file.exists(path)


  if(available_specimen) {

    type_expr <- getColumnExpr(name_of_specimen_csv, spec_type, "VARCHAR")

    query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE specimen_data AS
        SELECT DISTINCT
        {`spec_id`} AS spec_id,
        {type_expr} AS type,
        {`spec_cond`} As condition,
        {`spec_cond_system`} As condition_system
        FROM read_csv_auto({path})
      ", .con = con)

    dbExecute(con, query)

    checkForDuplicates("specimen_data", "spec_id")

  } else {
    writeLogData("Note: There are no Specimens linked from Potassium-Observations")
  }
  return(available_specimen)
}

evaluateQualityLOINCs <- function() {

  # which quality-loincs are available?
  query <- glue_sql("
      SELECT distinct loinc
      FROM quality_data
    ", .con = con)
  available_quality_loinc <- dbGetQuery(con, query)


  # if the result is empty write a note to log, else try to match them with
  # potassium values

  if (nrow(available_quality_loinc) == 0) {
    writeLogData("There are no quality LOINCs available")
    return(FALSE)
  } else {
    writeLogData("Available quality LOINCs: ", available_quality_loinc)

    query <- glue_sql("
      SELECT count(*)
      FROM quality_data
    ", .con = con)
    total_count_quality_loinc <- dbGetQuery(con, query)
    writeLogData("Total count: ", total_count_quality_loinc)

    writeLog("Trying to match quality results to Potassium values by timestamp")

    # test if there are potassium-measurements that share the same
    # timestamp

    query <- glue_sql("
      SELECT count(Distinct obs_id) as count
      FROM potassium p1
      WHERE (patient, time) IN (
        SELECT patient, time
        FROM potassium
        GROUP BY patient, time
        HAVING COUNT (DISTINCT obs_id) >= 2
      )
    ", .con = con)
    shared_timestamp <- dbGetQuery(con, query)$count

    if(shared_timestamp > 0) {
      writeLogData("Note: There are Potassium Measurements with same patient and timestamp: ",
                   shared_timestamp)
    }

    # join potassium measurements with quality loincs
    # assumption: same timestamp = same sample

    query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE potassium_quality_join AS
    SELECT DISTINCT
      p.obs_id          AS potassium_obs_id,
      p.loinc           AS potassium_loinc,
      p.time            AS time,
      p.patient         AS patient,
      q.obs_id          AS quality_obs_id,
      q.loinc           AS quality_loinc,
      q.value           AS quality_value,
      q.unit            AS quality_unit,
      q.value_code      AS quality_value_code,
      q.value_code_system AS quality_value_system,
      q.interpretation AS quality_interpretation
    FROM potassium p
    INNER JOIN quality_data q
      ON p.patient = q.patient
      AND p.time   = q.time
     ", .con = con)
    count_timestamp <- dbExecute(con, query)

    # no check for duplicates. The multiple matched timestamp issue is already
    # taken care of.

    if(shared_timestamp > 0 && count_timestamp > 0) {
      # if there are  potassium values with shared timestamp get
      # detailed information how those duplicates match
      writeLogData("Analysing potassium measurements with shared timestamp: ")
      query <- glue_sql("
        WITH group_stats AS (
          SELECT
            patient,
            time,
            COUNT(DISTINCT potassium_obs_id) AS n_potassium,
            COUNT(DISTINCT quality_loinc)    AS n_quality_loincs,
            COUNT(DISTINCT quality_obs_id)   AS n_quality_obs
          FROM potassium_quality_join
          GROUP BY patient, time
          HAVING COUNT(DISTINCT potassium_obs_id) >= 2
        )
        SELECT
          COUNT(*) AS n
        FROM group_stats
        WHERE n_quality_obs < n_potassium * n_quality_loincs
      ", .con = con)
      incomplete_quality_groups <- dbGetQuery(con, query)$n

      writeLogData("Number of groups (same timestamp) where not all measurements have a quality result: ",
                   incomplete_quality_groups)
    }

    if(count_timestamp == 0) {
      writeLogData("No matches found by timestamp.")
    } else {
      writeLog("Analyse results from matching by timestamp")
      analyseQualityMatch("potassium_quality_join")
    }

    # check if basedon column is present

    writeLog("Trying to match quality data by basedOn reference")
    path <- paste0(data_dir, "/", name_of_lab_csv)

    csv_cols <- dbGetQuery(con, glue_sql(
      "DESCRIBE SELECT * FROM read_csv_auto({path}) LIMIT 0",
      .con = con
    ))$column_name

    if (obs_basedon %in% csv_cols) {

      # check if basedOn is always NULL
      # TODO: nein hier ausgeben wie oft basedOn gefüllt ist!
      query <- glue_sql("
        SELECT count(1) n
        FROM potassium
        WHERE basedOn IS NOT NULL
      ", .con = con)
      basedon <- dbGetQuery(con, query)$n

      if(basedon == 0) {
        writeLogData("BasedOn is always NULL. Skip matching.")
      } else {
        writeLogData("Rows with existing basedOn reference: ", basedon)

        # get the referecetype of basedon (like Servicerequest)
        query <- glue_sql("
          SELECT
              split_part(basedon, '/', 1) AS ref_type,
                count(*) AS count
              FROM potassium
              WHERE basedon IS NOT NULL
              GROUP BY ref_type
          ", .con = con)
        basedon_prefix <- dbGetQuery(con, query)
        writeLogData("BasedOn reference type: ", basedon_prefix)

        query <- glue_sql("
          CREATE OR REPLACE TEMP TABLE potassium_quality_join_ref AS
          SELECT DISTINCT
            p.obs_id          AS potassium_obs_id,
            p.loinc           AS potassium_loinc,
            p.time            AS time,
            p.patient         AS patient,
            q.obs_id          AS quality_obs_id,
            q.loinc           AS quality_loinc,
            q.value           AS quality_value,
            q.unit            AS quality_unit,
            q.value_code      AS quality_value_code,
            q.value_code_system AS quality_value_system,
            q.interpretation AS quality_interpretation
          FROM potassium p
          INNER JOIN quality_data q
            ON p.basedon = q.basedon
          ", .con = con)
        count_ref <- dbExecute(con, query)

        checkForDuplicates("potassium_quality_join_ref", "potassium_obs_id")

        if(count_ref == 0) {
          writeLogData("No matches found by basedOn reference.")
        } else {
          writeLogData("Total matches by reference: ", count_ref)
          writeLog("Analyse results from matching by reference")
          analyseQualityMatch("potassium_quality_join_ref")
        }
      }

    } else {
      writeLogData("No basedOn column in ", name_of_lab_csv)
    }
    return(TRUE)
  }
}


analyseQualityMatch <- function(join_table) {

  n_potassium   <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM potassium")$n
  n_pot_matched <- dbGetQuery(con, glue_sql("
      SELECT COUNT(DISTINCT potassium_obs_id) AS n
      FROM {join_table}
    ", .con = con))$n

  n_pairs <- dbGetQuery(con, glue_sql(
    "SELECT COUNT(DISTINCT (potassium_obs_id, quality_obs_id)) AS n
    FROM {join_table}", .con = con))$n

  if (n_pairs == 0) {
    writeLogData(glue("No matches found. Skip further analysis"))
    return()
  }

  writeLogData("Total Potassium Values: ", n_potassium)
  writeLogData("At least one Quality match: ", n_pot_matched)
  writeLogData("Total Quality-pairs: ", n_pairs)

  # group the matched rows by potassium loinc
  quality_loincs_per_potassium <- dbGetQuery(con, glue_sql("
    SELECT
      potassium_loinc,
      quality_loinc,
      COUNT(*) AS n_pairs
    FROM {join_table}
    GROUP BY potassium_loinc, quality_loinc
    ORDER BY potassium_loinc, quality_loinc
  ", .con = con))

  writeLogData("Quality-LOINCs per Potassium-LOINC:")
  writeLogData("Potassium-LOINC, Quality-LOINC, count",
               quality_loincs_per_potassium)

  # test if there are more than two of the same quality loincs matched to
  # the same potassium_value. That is an indicator that the matching
  # may have problems
  query <- glue_sql("
    SELECT count(Distinct potassium_obs_id) as count
    FROM {join_table}
    WHERE  potassium_obs_id IN (
      SELECT potassium_obs_id
      FROM {join_table}
      GROUP BY potassium_obs_id, quality_loinc
      HAVING count(distinct quality_obs_id) >= 2
    )
  ", .con = con)
  too_many_matches <- dbGetQuery(con, query)$count

  writeLogData("Measurements with two or more matchings of the same quality-LOINC: ",
               too_many_matches)

  # get result-types for quality-loincs (numeric, code or interpretation)
  query <- glue_sql("
    SELECT count(distinct quality_obs_id)
    FROM {join_table}
    WHERE quality_value is not NULL
  ", .con = con)
  valueResult <- dbGetQuery(con, query)
  writeLogData("Quality-Results with a value: ", valueResult)

  query <- glue_sql("
    SELECT count(distinct quality_obs_id)
    FROM {join_table}
    WHERE quality_value_code is not NULL
  ", .con = con)
  codeResult <- dbGetQuery(con, query)
  writeLogData("Quality-Results with a code: ", codeResult)

  query <- glue_sql("
    SELECT count(distinct quality_obs_id)
    FROM {join_table}
    WHERE quality_interpretation is not NULL
  ", .con = con)
  interpretationResult <- dbGetQuery(con, query)
  writeLogData("Quality-Results with interpretation: ", interpretationResult)

  # if numeric results are available get the unit and median
  if (valueResult > 0) {
    writeLog("Analysing numeric results")

    query <- glue_sql("
      WITH distinct_quality_values AS (
        SELECT DISTINCT quality_obs_id, quality_loinc, quality_unit, quality_value
        FROM {join_table}
        WHERE quality_value IS NOT NULL
      )
      SELECT
        quality_loinc, quality_unit,
        median(quality_value) AS median_quality_value,
        avg(quality_value) AS mean_quality_value,
        stddev_pop(quality_value) AS sd_quality_value,
        quantile_cont(quality_value, 0.25) AS p25_quality_value,
        quantile_cont(quality_value, 0.75) AS p75_quality_value
      FROM distinct_quality_values
      GROUP BY quality_loinc, quality_unit
    ", .con = con)
    unit <- dbGetQuery(con, query)

    writeLogData("Used units per quality-loinc with median, mean, sd, p25, p75: ",
                 unit, kanonymity = FALSE)
  }

  # if code results are available get system and codes
  if (codeResult > 0) {
    writeLog("Analysing CodeResults")

    query <- glue_sql("
      SELECT distinct quality_value_system
      FROM {join_table}
      where quality_value_code is not NULL
    ", .con = con)
    code_system <- dbGetQuery(con, query)
    writeLogData("Used code system: ", code_system)

    query <- glue_sql("
      WITH distinct_quality_codes AS (
        SELECT DISTINCT quality_obs_id, quality_loinc, quality_value_code
        FROM {join_table}
        WHERE quality_value_code IS NOT NULL
      )
      SELECT
      quality_loinc as loinc,
      quality_value_code AS code,
      count(*) as count
      FROM distinct_quality_codes
      GROUP BY quality_value_code, quality_loinc
    ", .con = con)
    code_value <- dbGetQuery(con, query)

    writeLogData("Used codes per quality loinc: ")
    writeLogData("loinc, code, count", code_value)
  }

  # if interpretation results are available get codes
  if (interpretationResult > 0) {
    writeLog("Analysing interpretations")

    query <- glue_sql("
      WITH distinct_quality_interpretations AS (
        SELECT DISTINCT quality_obs_id, quality_loinc, quality_interpretation
        FROM {join_table}
        WHERE quality_interpretation IS NOT NULL
      )
      SELECT
      quality_loinc AS loinc,
      quality_interpretation AS interpretation,
      count(*) as count
      FROM distinct_quality_interpretations
      GROUP BY quality_interpretation, quality_loinc
    ", .con = con)
    code_interpretation <- dbGetQuery(con, query)

    writeLogData("Used interpretations (per quality-loinc): ")
    writeLogData("loinc, interpretation, count", code_interpretation)
  }
}


analyseCodeResults <- function() {

  # are there potassium values with a code-Result?
  query <- glue_sql("
        SELECT
        count(distinct obs_id) as count
        FROM potassium
        WHERE value_code is not NULL
      ", .con = con)
  count_code_results <- dbGetQuery(con, query)

  if(count_code_results == 0) {
    writeLogData("There are no measurements with a non-numeric result")
  } else {
    writeLogData("Potassium measurements with a non-numeric result: ",
                 count_code_results)

    # get the used code systems and used distinct codes
    query <- glue_sql("
        SELECT distinct value_code_system
        FROM potassium
        where value_code is not NULL
      ", .con = con)
    code_system <- dbGetQuery(con, query)

    writeLogData("Used code system: ", code_system)

    query <- glue_sql("
        SELECT
        loinc, value_code as code, count(*) as count
        FROM potassium
        where value_code is not NULL
        GROUP BY value_code, loinc
      ", .con = con)
    count_code_results <- dbGetQuery(con, query)

    writeLogData("Used codes: ", count_code_results)
  }
}

analyseSpecimen <- function() {

  # count the number of referenced specimen
  query <- glue_sql("
        SELECT
        count(distinct spec_id)
        FROM specimen_data
      ", .con = con)
  count_specimens <- dbGetQuery(con, query)

  writeLogData("Available specimens: ", count_specimens)

  # count the number of potassium measurements with specimen-link
  query <- glue_sql("
        SELECT
        count(distinct obs_id)
        FROM potassium
        WHERE specimen_ref IS NOT NULL
      ", .con = con)
  count_specimen_ref <- dbGetQuery(con, query)

  writeLogData("Potassium measurements with specimen-ref: ", count_specimen_ref)

  # join specimen with potassium values
  query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE potassium_specimen_join AS
        SELECT DISTINCT
          loinc,
          obs_id,
          patient,
          time,
          spec_id,
          type,
          condition,
          condition_system
        FROM potassium p
        LEFT JOIN specimen_data s
        ON p.specimen_ref = concat('Specimen/', s.spec_id)
        WHERE p.specimen_ref IS NOT NULL
      ", .con = con)

  dbExecute(con, query)
  checkReference("potassium_specimen_join", "spec_id")

  # Get Specimen types and specimen conditions per loinc
  # For type we only have snomed-CT slice. no system check needed.
  query <- glue_sql("
        SELECT loinc, type, count(*) as count
        FROM potassium_specimen_join
        GROUP BY loinc, type
      ", .con = con)
  specimen_type <- dbGetQuery(con, query)

  writeLogData("Specimen types per loinc: ", specimen_type)

  query <- glue_sql("
        SELECT distinct condition_system
        FROM potassium_specimen_join
        where condition is not NULL
      ", .con = con)
  condition_system <- dbGetQuery(con, query)

  writeLogData("Used code system: ", condition_system)

  query <- glue_sql("
        SELECT loinc, condition, count(*) as count
        FROM potassium_specimen_join
        GROUP BY loinc, condition
      ", .con = con)
  specimen_condition <- dbGetQuery(con, query)

  writeLogData("Specimen condition per loinc: ", specimen_condition)
}

evaluateNotes <- function(quality_loincs_available) {

  # Notes are not included in the main potassium table

  path <- paste0(data_dir, "/", name_of_lab_csv)

  # check if there is a notes column in the lab.csv
  csv_cols <- dbGetQuery(con, glue_sql(
    "DESCRIBE SELECT * FROM read_csv_auto({path}) LIMIT 0",
    .con = con
  ))$column_name

  if (obs_note %in% csv_cols) {

    # create a table just for notes. Cast all as varchar to prevent
    # casting problems with notes.
    query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE lab_notes AS
      SELECT distinct
        {`obs_id`} AS obs_id,
        {`obs_loinc`}  AS loinc,
        {`obs_note`}   AS note
      FROM read_csv({path},all_varchar = true)
      WHERE {`obs_status`} NOT IN ('cancelled', 'entered-in-error')
        AND {`obs_note`} IS NOT NULL
        AND {`obs_loinc_system`} = 'http://loinc.org'
        AND CAST({`obs_time`} AS TIMESTAMP) >= {global_min_time}
        AND CAST({`obs_time`} AS TIMESTAMP) <= {global_max_time}
      ", .con = con)
    dbExecute(con, query)

    checkForDuplicates("lab_notes", "obs_id")

    query <- glue_sql("
        SELECT
        loinc, count(*)
        FROM lab_notes
        WHERE note IS NOT NULL
        GROUP BY loinc
      ", .con = con)

    notes_not_null <- dbGetQuery(con, query)

    if(nrow(notes_not_null) > 0) {
      writeLogData("Notes that are not NULL by LOINC: ", notes_not_null)
    } else {
      writeLogData("Notes are always NULL. Skipping further evaluation.")
      return(FALSE)
    }

    # build a regexpattern from notes_search_snippets
    pattern <- paste(notes_search_snippets, collapse = "|")

    # get all notes from potassium values. i in regexp_matches makes
    # the search case insensitiv
    query <- glue_sql("
        SELECT DISTINCT
        note, count(*)
        FROM lab_notes
        WHERE loinc IN ({LOINCs_Kalium*})
        AND regexp_matches(note, {pattern}, 'i')
        GROUP BY note
      ", .con = con)

    potassium_notes <- dbGetQuery(con, query)
    total_count_potassium <- sum(potassium_notes$count)
    writeLogData("Total regex matches with potassium notes: ", total_count_potassium)

    quality_notes <- 0

    if(quality_loincs_available) {

      # get all notes from quality observations
      query <- glue_sql("
        SELECT DISTINCT
        note, count(*)
        FROM lab_notes
        WHERE loinc IN ({LOINCs_Quality*})
        AND regexp_matches(note, {pattern}, 'i')
        GROUP BY note
      ", .con = con)

      quality_notes <- dbGetQuery(con, query)
      total_count_quality <- sum(quality_notes$count)
      writeLogData("Total regex matches with quality loinc notes: ", total_count_quality)
      writeLogData("Note: this is the number of matches within all observations
                   with a quality loinc (without matching to potassium observations)")
    }

    if(nrow(potassium_notes) >0 || nrow(quality_notes) > 0) {
      writeLogData("\n")
      writeLogData("THIS IS FREE TEXT FROM YOUR DATA.
                    PLEASE EVALUATE BEFORE SENDING THE DATA")
      writeLogData("\n")
      if(nrow(potassium_notes) >0 ) {
        writeLogData("Notes that are assoziated with hemolysis in potassium data: ", potassium_notes)
        writeLogData("\n")
      }
      if(nrow(quality_notes) > 0) {
        writeLogData("Notes that are assoziated with hemolysis in quality data: ", quality_notes)
      }
      return(TRUE)
    } else {
      writeLogData("There are no notes in potassium or quality data
                   that match the search patterns for hemolysis.")
      return(FALSE)
    }

  } else {
    writeLogData("Note: no note column in ", name_of_lab_csv)
    writeLogData("Skipping notes evaluation")
    return(FALSE)
  }
}

evaluateMethods <- function() {

  query <- glue_sql("
      SELECT count(distinct obs_id) as n
      FROM potassium
      WHERE method_code IS NOT NULL
      ", .con = con)
  method_rows <- dbGetQuery(con, query)$n

  if(method_rows > 0) {
    writeLogData("Potassium measurements with method information: ", method_rows)
    query <- glue_sql("
      SELECT
        loinc, method_system, method_code, count(*)
      FROM potassium
      WHERE method_code IS NOT NULL
      GROUP BY loinc, method_system, method_code
      ", .con = con)
    method <- dbGetQuery(con, query)
    writeLogData("Available methods by LOINC: ", method)
  } else {
    writeLogData("No method information available")
  }
}

cleanUpSampleQuality <- function() {
  query <- glue_sql("
      DROP TABLE IF EXISTS potassium_specimen_join;
      DROP TABLE IF EXISTS quality_data;
      DROP TABLE IF EXISTS specimen_data;
      DROP TABLE IF EXISTS potassium_quality_join;
    ", .con = con)
  dbExecute(con, query)
}
