
evaluateCovariates <- function() {

  writeLogHeader("Part3: Evaluate Covariates", "This part evaluates if potassium
                 values can be matched to available covariates like further
                 lab-results medication, diagnosis and procedures and have an
                 encounter context.")

  writeLog("Preparing potassium table")

  potassium_available <- loadPotassiumData()

  # load all optional data-types and store the information if data is available
  availability <- list(
    encounter    = loadEncounter(),
    medication   = loadMedicationData(),
    lab = loadLabMain(),
    procedures = loadProcedures(),
    conditions = loadConditions()
  )

  writeLog("Calculating available month")
  time_results <- calculateTimelineMain(availability)

  # if there are no lines in the potassium table, return
  if(!potassium_available) {
    writeLog("Prepared Potassium table has no rows. Skip further evaluations")
    write.csv(time_results$timeline,
              file = paste0(output_dir, "/timelineCovariates.csv"))
    return()
  }

  writeLog("Matching encounters to potassium measurements")
  assignEncounterContext(availability$encounter)

  writeLog("Check encounter references")
  checkEncounterReferences(availability)

  writeLog("Matching the chronological next potassium measurement")
  matchNextPotassium()

  if(availability$lab) {
    writeLog("Matching labresults to potassium measurements")
    matchLab()
  } else writeLog("No additional lab results. Skip matching")


  if(availability$medication) {
    writeLog("Matching medication to potassium measurements")
    matchMedication()
    matchMedicationAfter()
  } else writeLog("No medications. Skip matching")


  if(availability$procedures) {
    writeLog("Matching procedures to potassium measurements")
    matchProcedures()
    matchProceduresAfter()
  } else writeLog("No procedures. Skip matching")

  if(availability$conditions) {
    writeLog("Matching conditions to potassium measurements")
    matchConditions()
  } else writeLog("No conditions. Skip matching")


  writeLog("Evaluate resource matching by timestamp and reference")
  if(availability$conditions && availability$encounter) {
    writeLog("Evaluate condition matching by timestamp")
    conditionEncounterTimeline()
  }


  writeLog("Evaluate concurrenty measurements in blood and serum")
  compare_serum_blood <- matchAndEvaluateSerumBlood(availability, time_results$availability)


  writeLog("Running descriptive statistics")
  statistic_counts <- descriptiveStatistic(availability, time_results$availability)


  writeLog("Running linear regression")
  regression_results <- linearRegression(availability, time_results$availability)


  #####################

  # write results as .csv

  write.csv(regression_results, file = paste0(output_dir, "/covariatesRegression.csv"))

  write.csv(time_results$timeline,
            file = paste0(output_dir, "/timelineCovariates.csv"))

  write.csv(statistic_counts, file = paste0(output_dir, "/covariatesCounts.csv"))

  if (!is.null(compare_serum_blood)) {
    write.csv(compare_serum_blood, file = paste0(output_dir, "/compareSerumBlood.csv"))
  }

}


assignEncounterContext <- function(encounter_available) {

  # if for some reasons we do not have encounters, add all encounter
  # columns and treat every measurement as AMB
  if (!encounter_available) {
    query <- glue("
      CREATE OR REPLACE TEMP TABLE potassium_result AS
      SELECT *,
        'AMB'  AS enc_class,
        NULL   AS enc_id,
        NULL   AS enc_start,
        NULL   AS enc_end,
        0      AS first
      FROM potassium_result
    ")
    dbExecute(con, query)
    writeLogData("No encounter table available. All measurements are treated as AMB")
    return()
  }

  # Log open IMP/SS encounters (should not exist for historical data)
  query <- glue_sql("
    SELECT class, COUNT(*) AS n
    FROM encounter_main
    WHERE class IN ('IMP', 'SS')
      AND period_end IS NULL
    GROUP BY class
  ", .con = con)
  open_encounters <- dbGetQuery(con, query)

  if (nrow(open_encounters) > 0) {
    writeLogData("Warning: open IMP/SS encounters excluded from assignment: ",
                 open_encounters)
  }

  # Assign encounter context. For each potassium value, find all closed
  # IMP/SS encounters that contain the measurement time, then keep the
  # longest one; ties are broken via enc_id (deterministic random)
  # only one row is taken per candidates. That ensures that also prevent
  # row duplicates

  query <- glue("
    CREATE OR REPLACE TEMP TABLE potassium_result AS
    WITH candidates AS (
      SELECT
        p.*,
        e.enc_id,
        e.period_start,
        e.period_end,
        e.period_end::TIMESTAMP - e.period_start::TIMESTAMP AS duration,
        ROW_NUMBER() OVER (
          PARTITION BY p.patient, p.time
          ORDER BY e.period_end::TIMESTAMP - e.period_start::TIMESTAMP DESC,
                   e.enc_id ASC
        ) AS rn
      FROM potassium_result p
      JOIN encounter_main e
        ON  p.patient = e.patient
        AND p.time    >= e.period_start
        AND p.time    <= e.period_end
        AND e.class   IN ('IMP', 'SS')
        AND e.period_end IS NOT NULL
    )
    SELECT
      p.*,
      CASE WHEN c.enc_id IS NOT NULL THEN 'IMP' ELSE 'AMB' END AS enc_class,
      c.enc_id,
      c.period_start AS enc_start,
      c.period_end   AS enc_end
    FROM potassium_result p
    LEFT JOIN candidates c
      ON  p.patient = c.patient
      AND p.time    = c.time
      AND c.rn      = 1
  ")
  dbExecute(con, query)

  # create a new column first. This marks if the potassium measurement
  # is the first in the encounter. Only for IMP cases. If there is a tie
  # (same timestamp) both get 0 (=are excluded).
  query <- glue("
    CREATE OR REPLACE TEMP TABLE potassium_result AS
      SELECT *, CASE
      WHEN enc_id IS NULL THEN 0
      WHEN RANK() OVER (PARTITION BY enc_id ORDER BY time ASC) = 1
        AND COUNT(*) OVER (PARTITION BY enc_id, time) = 1 THEN 1
      ELSE 0
      END AS first
    FROM potassium_result;
  ")
  dbExecute(con, query)

  query <- glue("
    SELECT count(1)
    FROM potassium_result
    WHERE first = 1
  ")
  first_results <- dbGetQuery(con, query)
  writeLogData("Potassium measurements that are the first of their encounter (inpatient only): ",
               first_results)

  # get counts of measurements that have two different results at the same time
  query <- glue("
    SELECT COUNT(DISTINCT enc_id) AS count_enc_ids_without_first_flag
    FROM (
      SELECT enc_id,
             RANK() OVER (PARTITION BY enc_id ORDER BY time ASC) AS rank,
             COUNT(*) OVER (PARTITION BY enc_id, time) AS count
      FROM potassium_result
      WHERE enc_id IS NOT NULL
    ) AS subquery
    WHERE rank = 1 AND count > 1;
  ")

  multi_results <- dbGetQuery(con, query)

  if(multi_results[[1]] > 0) {
    writeLogData("Note: There are encounters with more than one first result at the same time: ",
                 multi_results[[1]])
    writeLogData("Those ambiguous encounters do not get a measurement with a first flag.")
  }

  # get the average number of measurements per encounter
  query <- glue_sql("
    SELECT
      MEAN(n)   AS mean,
      MEDIAN(n)   AS median,
      STDDEV_POP(n)  AS sd,
      MIN(n)                         AS min,
      MAX(n)                         AS max,
      QUANTILE_CONT(n, 0.25)         AS q1,
      QUANTILE_CONT(n, 0.75)         AS q3
    FROM (
      SELECT enc_id, COUNT(*) AS n
      FROM potassium_result
      WHERE enc_id IS NOT NULL
      GROUP BY enc_id
    )
  ", .con = con)

  av_measurements <- dbGetQuery(con, query)

  # round numbers to two digits
  num_cols <- sapply(av_measurements, is.numeric)
  av_measurements[num_cols] <- round(av_measurements[num_cols], 2)
  writeLogData("Average (mean, median, sd, min, max, q1, q3) duration of IMP encounters: ",
               av_measurements, kanonymity = FALSE)

  # how long are the encounters
  query <- glue_sql("
    SELECT
      MEAN(duration)                AS mean,
      MEDIAN(duration)               AS median,
      STDDEV_POP(duration)           AS sd,
      MIN(duration)                  AS min,
      MAX(duration)                  AS max,
      QUANTILE_CONT(duration, 0.25)  AS q1,
      QUANTILE_CONT(duration, 0.75)  AS q3
    FROM (
      SELECT
        enc_id,
        DATE_DIFF('day', MIN(enc_start::TIMESTAMP), MAX(enc_end::TIMESTAMP)) AS duration
      FROM potassium_result
      WHERE enc_id IS NOT NULL
        AND enc_start IS NOT NULL
        AND enc_end IS NOT NULL
      GROUP BY enc_id
    )
  ", .con = con)

  av_duration <- dbGetQuery(con, query)

  # round numbers to two digits
  num_cols <- sapply(av_duration, is.numeric)
  av_duration[num_cols] <- round(av_duration[num_cols], 2)
  writeLogData("Average (mean, median, sd, min, max, q1, q3) of measurements per IMP encounter: ",
               av_duration, kanonymity = FALSE)

  # Summary: IMP vs AMB counts
  query_summary <- glue_sql("
    SELECT enc_class, COUNT(*) AS n
    FROM potassium_result
    GROUP BY enc_class
  ", .con = con)
  writeLogData("Potassium values by encounter class: ",
               dbGetQuery(con, query_summary))
  writeLogData("Note that IMP and SS are combined to IMP")

  # AMB values: split by whether an AMB encounter starts on the same day
  query <- glue_sql("
    SELECT
      count (*)
    FROM potassium_result p
    JOIN (
      SELECT DISTINCT patient, CAST(period_start::TIMESTAMP AS DATE) AS start_date, enc_id
      FROM encounter_main
      WHERE class = 'AMB'
    ) amb
      ON  p.patient = amb.patient
      AND CAST(p.time::TIMESTAMP AS DATE)  = amb.start_date
  ", .con = con)

  amb_enc <- dbGetQuery(con, query)

  writeLogData("AMB measurements that have an AMB encounter startig on that day: ", amb_enc)

  writeLogData("This just for information. Encounter context for AMB is no further considered")

  writeLogData("Encounter context assigned successfully")
}

calculateTimelineMain <- function(availability) {

  # Mapping: availability-Key -> table names
  availability_to_table <- list(
    medication = "medadm_all_start",
    encounter  = "encounter_main",
    lab = "lab",
    procedures = "procedures_start",
    conditions = "conditions"
  )

  # get the tables for all available resources
  active_keys <- names(availability_to_table)[
    sapply(names(availability_to_table), function(k) isTRUE(availability[[k]]))
  ]

  # write all those tables to tables_to_check
  tables_to_check <- unlist(availability_to_table[active_keys])


  # create a sequence with all relevant month
  all_months <- data.frame(
    month = seq(
      lubridate::floor_date(global_min_time, "month"),
      lubridate::floor_date(global_max_time, "month"),
      by = "month"
    )
  )

  # convert from posixct to date
  all_months$month <- as.Date(all_months$month)

  # init result with the calculated month sequence
  timeline_result <- all_months

  availability_rows <- list()

  # calculate timeline for each table in tables_to_check
  for (tbl in tables_to_check) {

    # caluclate timeline
    res <- calculateTimelineCo(tbl, all_months)

    # calculate availibilty of data. Data is available if
    # there is more than 50% of the max data available
    # TODO: 50% ist hier erstmal willkürlich. Zu diskutieren.

    counts    <- res[[tbl]]
    threshold <- max(counts, na.rm = TRUE) * 0.5
    eligible  <- res$month[!is.na(counts) & counts >= threshold]

    availability_rows[[tbl]] <- data.frame(
      table_name    = tbl,
      min_verfügbar = if (length(eligible) > 0) min(eligible) else as.Date(NA),
      max_verfügbar = if (length(eligible) > 0) max(eligible) else as.Date(NA)
    )

    # apply k-Anonymity to counts per month (tbl is the count column)
    res <- applyKAnonymity(res, tbl, c("month"))

    # merge the result to overall results
    timeline_result <- merge(timeline_result, res, by = "month", all.x = TRUE)
  }

  # bind availability_rows together to a dataframe results
  availability_result <- do.call(rbind, availability_rows)

  #remove row names
  rownames(availability_result) <- NULL

  result_all <- list(
    timeline     = timeline_result,
    availability = availability_result)

  writeLogData("Range where at least 50% of max data per month is available: ",
               result_all$availability)

  return(result_all)
}


calculateTimelineCo <- function(table_to_check, all_months) {

  # get the minimun and maximum date. Filter by global_min/max because
  # there is no time-filtering when loading covariates tables.
  query <- glue::glue_sql(
          "SELECT MIN(time) AS min_t, MAX(time) AS max_t
          FROM {table_to_check}
          WHERE time >= {global_min_time}
          AND time <= {global_max_time}", .con = con)

  min_max <- dbGetQuery(con, query)

  writeLogData(paste0("min ", table_to_check, ": "),
               min_max$min_t, kanonymity = FALSE)
  writeLogData(paste0("max ", table_to_check,": "),
               min_max$max_t, kanonymity = FALSE)

  query <- glue::glue_sql(
    "SELECT
      DATE_TRUNC('month', time::TIMESTAMP) AS month,
      COUNT(*)  AS n
    FROM {table_to_check}
    WHERE time >= {global_min_time}
    AND time <= {global_max_time}
    GROUP BY 1
    ORDER BY 1
  ", .con = con)

  result <- dbGetQuery(con, query)

  # make sure that result$month really is a date in the same format
  # as the month in all_month
  result$month <- as.Date(result$month)

  # merge query results with all_month
  result <- merge(
    all_months,
    result,
    by = "month",
    all.x = TRUE
  )

  # convert na to 0
  result$n[is.na(result$n)] <- 0

  # rename column n to the name of the table
  names(result)[names(result) == "n"] <- table_to_check

  return(result)
}

matchMedication <- function() {

  # builds sql that calculates for each potassium timestamp if there was
  # an atc from the list given in the last 24 hours.
  # use the view that gives for period the end (if it exists)

  build_flag_sql <- function(atc_groups) {
    atc_groups |>
      group_by(name) |>
      summarise(
        cond = paste(paste0("m.atc LIKE '", atc, "%'"), collapse = " OR "),
        .groups = "drop"
      ) |>
      mutate(
        sql = glue(
          "CASE WHEN EXISTS (
            SELECT 1 FROM medadm_all_end m
            WHERE m.patient = p.patient
              AND m.time::TIMESTAMP >  p.time::TIMESTAMP - INTERVAL '24 hours'
              AND m.time::TIMESTAMP <= p.time::TIMESTAMP
              AND ({cond})
          ) THEN 1 ELSE 0 END AS {name}"
        )
      ) |>
      pull(sql)
  }

  flag_sql <- build_flag_sql(atc_groups)

  flag_sum <- paste(unique(atc_groups$name), collapse = " + ")

  # replace the table with a table with the new columns
  query <- glue(
    "CREATE OR REPLACE TEMP TABLE potassium_result AS
   SELECT p.*,
      {paste(flag_sql, collapse = ',\n      ')}
   FROM potassium_result p"
  )

  dbExecute(con, query)

  # add a column that indicates if there is only one med.
  query <- glue(
    "CREATE OR REPLACE TEMP TABLE potassium_result AS
   SELECT *,
      CASE WHEN ({flag_sum}) = 1 THEN 1 ELSE 0 END AS alone_med
   FROM potassium_result"
  )
  dbExecute(con, query)

  atc_cols <- unique(atc_groups$name)

  query <- glue_sql("
    SELECT SUM(
        {SQL(paste(atc_cols, collapse = ' + '))}
    ) AS anzahl
    FROM potassium_result
  ", .con = con)
  med_counts <- dbGetQuery(con, query)

  writeLogData(paste0("A medication is cosidered a match if it is given within ",
      "24 hours before the potassium measurement"))
  writeLogData("Total matches with medication: ", med_counts)

  query <- glue_sql("
    SELECT count(*)
    FROM potassium_result
    WHERE alone_med = 1
  ", .con = con)
  alone_counts <- dbGetQuery(con, query)
  writeLogData("Measurements with only one matched atc-group: "
               , alone_counts)
}

matchLab <- function() {

  # add for each lab_results 3 extra column to potassium_result (for L/N/H)

  # all labels and all possible results
  lab_labels  <- c("glucose", "bicarbonat", "pH", "crea", "GFR")
  lab_results <- c("N", "H", "L")

  # build all combinations
  combos <- expand.grid(
    label  = lab_labels,
    result = lab_results,
    stringsAsFactors = FALSE
  )

  # get the time-window for each lab-result
  combos$window_hours <- lab_windows[combos$label]

  # build SQL for each combination
  flag_fragments <- mapply(function(label, result, window_hours) {
    colname <- paste0(label, "_", result)
    interval_sql <- sprintf("INTERVAL '%d hours'", window_hours)

    glue_sql("
    CASE WHEN EXISTS (
      SELECT 1
      FROM lab AS l
      WHERE l.patient = p.patient
        AND l.label   = {label}
        AND l.result  = {result}
        AND l.time::TIMESTAMP BETWEEN
              p.time::TIMESTAMP - {DBI::SQL(interval_sql)}
              AND p.time::TIMESTAMP + {DBI::SQL(interval_sql)}
    ) THEN 1 ELSE 0 END AS {`colname`}
  ", .con = con)
  }, combos$label, combos$result, combos$window_hours)

  flag_sql <- paste(flag_fragments, collapse = ",\n  ")

  # to add a value to each matching potassium measurement.
  # if there are multiple chosse the one that is closest to the potassium
  # measurement. If there are still multiple choose by that order:
  # result: L than H than N. If still unclear choose random.

  #TODO: nachdenken/überprüfen ob das tut was es soll.

  value_fragments <- mapply(function(label, window_hours) {

      colname <- paste0(label, "_value_norm")
      interval_sql <- sprintf("INTERVAL '%d hours'", window_hours)

      glue_sql("
        (
          SELECT value_norm
          FROM lab l
          WHERE l.patient = p.patient
            AND l.label = {label}
            AND l.time::TIMESTAMP BETWEEN
                  p.time::TIMESTAMP - {DBI::SQL(interval_sql)}
                  AND p.time::TIMESTAMP + {DBI::SQL(interval_sql)}
          ORDER BY
            ABS(EXTRACT(EPOCH FROM (
              l.time::TIMESTAMP - p.time::TIMESTAMP
            ))),
            CASE l.result
              WHEN 'L' THEN 1
              WHEN 'H' THEN 2
              WHEN 'N' THEN 3
              ELSE 4
            END
          LIMIT 1
        ) AS {`colname`}
      ", .con = con)

  }, combos$label[match(lab_labels, combos$label)],
  lab_windows[lab_labels])

  value_sql <- paste(value_fragments, collapse = ",\n  ")

  # add the extra columns to potassium_result
  query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE potassium_result AS
    SELECT
      p.*,
      {DBI::SQL(flag_sql)},
      {DBI::SQL(value_sql)}
    FROM potassium_result p
  ", .con = con)

  dbExecute(con, query)

  # get the count of all matches
  new_cols <- paste0(combos$label, "_", combos$result)

  query <- glue_sql("
    SELECT SUM(
        {SQL(paste(new_cols, collapse = ' + '))}
    ) AS anzahl
    FROM potassium_result
  ", .con = con)
  lab_counts <- dbGetQuery(con, query)

  writeLogData("A lab-value is considered a match if effectiveDatetime
               is +/- hours from potassium measurement (as defined in config)")

  writeLogData("Total matches with lab: ", lab_counts)
}

matchProcedures <- function() {

  # unclear means dialysis that have a start but no end timestamp.
  before_interval    <- sprintf("INTERVAL '%d hours'",12)
  unclear_interval <- sprintf("INTERVAL '%d hours'",12)

  # SQL to check if the measurement is during the dialyse
  during_sql <- glue_sql("
    CASE WHEN EXISTS (
      SELECT 1
      FROM procedures AS d
      WHERE d.patient = p.patient
        AND d.period_end IS NOT NULL
        AND p.time::TIMESTAMP BETWEEN
              d.period_start::TIMESTAMP AND d.period_end::TIMESTAMP
    ) THEN 1 ELSE 0 END AS dialyse_during
  ", .con = con)

  # SQL to check if the measurement is clearly after the dialyse
  before_sql <- glue_sql("
    CASE WHEN EXISTS (
      SELECT 1
      FROM procedures AS d
      WHERE d.patient = p.patient
        AND d.period_end IS NOT NULL
        AND d.period_end::TIMESTAMP <= p.time::TIMESTAMP
        AND d.period_end::TIMESTAMP >=
              p.time::TIMESTAMP - {DBI::SQL(before_interval)}
        AND NOT (
          p.time::TIMESTAMP BETWEEN
            d.period_start::TIMESTAMP AND d.period_end::TIMESTAMP
        )
    ) THEN 1 ELSE 0 END AS dialyse_before
  ", .con = con)

  # SQL if we have only one timestamp and dont really know when exactly
  # the dialysis took place
  unclear_sql <- glue_sql("
    CASE WHEN EXISTS (
      SELECT 1
      FROM procedures AS d
      WHERE d.patient = p.patient
        AND d.period_end IS NULL
        AND COALESCE(d.performed::TIMESTAMP, d.period_start::TIMESTAMP) BETWEEN
              p.time::TIMESTAMP - {DBI::SQL(unclear_interval)}
              AND p.time::TIMESTAMP
    ) THEN 1 ELSE 0 END AS dialyse_unclear
  ", .con = con)

  flag_sql <- paste(
    during_sql, before_sql, unclear_sql,
    sep = ",\n  "
  )

  query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE potassium_result AS
    SELECT
      p.*,
      {DBI::SQL(flag_sql)}
    FROM potassium_result p
  ", .con = con)
  dbExecute(con, query)

  count_query <- glue_sql("
    SELECT
      SUM(dialyse_during) AS n_during,
      SUM(dialyse_before) AS n_before,
      SUM(dialyse_unclear) AS n_unclear
    FROM potassium_result
  ", .con = con)
  dialysis_counts <- dbGetQuery(con, count_query)

  writeLogData("A procedures (dialysis) is considered a match if its timestamp is
               within 12 hours before the measurement. It is distinguished between
               diaysis with clear timestamps that have already ended at the time
               of measurement, dialysis with clear timestamps and measurement during dialysis,
               and dialysis with uncertain timestamps")
  writeLogData("Total matches with dialysis (during/before/uncertain): ", dialysis_counts)
}

matchConditions <- function(){

  # build a subquery for each ICD group. Time window depends on encounter class:
  # IMP: enc_start to enc_end
  # AMB: the day of the potassium timestamp

  build_condition_flag_sql <- function(icd_groups) {
    icd_groups |>
      group_by(name) |>
      summarise(
        cond = paste(paste0("c.icd LIKE '", icd, "%'"), collapse = " OR "),
        .groups = "drop"
      ) |>
      mutate(
        sql = glue(
          "CASE WHEN EXISTS (
            SELECT 1 FROM conditions c
            WHERE c.patient = p.patient
              AND (
                (p.enc_class = 'IMP'
                  AND c.time::TIMESTAMP >= p.enc_start::TIMESTAMP
                  AND c.time::TIMESTAMP <= p.enc_end::TIMESTAMP)
                OR
                (p.enc_class = 'AMB'
                  AND c.time::TIMESTAMP >= DATE_TRUNC('day', p.time::TIMESTAMP)
                  AND c.time::TIMESTAMP < DATE_TRUNC('day', p.time::TIMESTAMP) + INTERVAL '1 day')
              )
              AND ({cond})
          ) THEN 1 ELSE 0 END AS {name}"
        )
      ) |>
      pull(sql)
  }

  flag_sql <- build_condition_flag_sql(icd_groups)

  # add new columns to potassium_result table
  query <- glue(
    "CREATE OR REPLACE TEMP TABLE potassium_result AS
     SELECT p.*,
        {paste(flag_sql, collapse = ',\n        ')}
     FROM potassium_result p"
  )
  dbExecute(con, query)

  # add a flag for measurements with only one matched condition
  flag_sum <- paste(unique(icd_groups$name), collapse = " + ")

  query <- glue(
    "CREATE OR REPLACE TEMP TABLE potassium_result AS
     SELECT *,
        CASE WHEN ({flag_sum}) = 1 THEN 1 ELSE 0 END AS alone_cond
     FROM potassium_result"
  )
  dbExecute(con, query)

  # get total matches
  cond_cols <- unique(icd_groups$name)

  sum_expr <- paste(
    glue::glue_sql("COALESCE({`cond_cols`}, 0)", .con = con),
    collapse = " + "
  )

  query <- glue_sql("
  SELECT SUM({SQL(sum_expr)}) AS anzahl
  FROM potassium_result
  ", .con = con)

  writeLogData("A condition is considered a match if the recorded time is inside
               the assigned encounter. Outpatient contacts are considered as
               one-day encounters")
  count <- dbGetQuery(con, query)
  writeLogData(paste0("Total matched conditions: "), count)
}

conditionEncounterTimeline <- function() {

  # create a view that gives only finished encounters. And all amb
  # encounters a set to period_start = period_end
  query <- glue_sql("
    CREATE OR REPLACE TEMP VIEW encounter_filtered AS
    SELECT
      patient,
      class,
      period_start::TIMESTAMP AS period_start,
      CASE
        WHEN class = 'AMB' THEN period_start::TIMESTAMP
        ELSE period_end::TIMESTAMP
      END AS period_end
    FROM encounter_main
    WHERE NOT (class IN ('IMP', 'SS') AND period_end IS NULL)
  ", .con = con)
  dbExecute(con, query)

  # view on conditions with only necessary items
  query <- glue_sql("
  CREATE OR REPLACE TEMP VIEW conditions_in_range AS
  SELECT
  con_id,
  time::TIMESTAMP  AS time,
  patient
  FROM conditions
  WHERE time >= {global_min_time}
    AND time <= {global_max_time}
  ", .con = con)
  dbExecute(con, query)

  query <- glue_sql("
    CREATE OR REPLACE TEMP VIEW condition_timing_all AS
    SELECT
      e.class,
      e.patient,
      e.period_start,
      c.con_id,
      c.time AS condition_time,
      CASE
        WHEN c.time >= e.period_start
             AND c.time <  e.period_start + INTERVAL 1 DAY
          THEN 'day1'
        WHEN c.time >= e.period_start + INTERVAL 1 DAY
             AND c.time <  e.period_start + INTERVAL 3 DAY
          THEN 'day2_3'
        WHEN c.time >= e.period_start + INTERVAL 3 DAY
             AND c.time <  e.period_end
          THEN 'day4_to_end'
        WHEN c.time >= e.period_end
             AND c.time <  e.period_end + INTERVAL 3 DAY
          THEN 'end_to_3d_after'
        WHEN c.time >= e.period_end + INTERVAL 3 DAY
             AND c.time <  e.period_end + INTERVAL 7 DAY
          THEN '3d_to_7d_after'
      ELSE 'remaining'
      END AS time_bucket,
      CASE
        WHEN c.time >= e.period_start
             AND c.time <  e.period_start + INTERVAL 1 DAY
          THEN 1
        WHEN c.time >= e.period_start + INTERVAL 1 DAY
             AND c.time <  e.period_start + INTERVAL 3 DAY
          THEN 2
        WHEN c.time >= e.period_start + INTERVAL 3 DAY
             AND c.time <  e.period_end
          THEN 3
        WHEN c.time >= e.period_end
             AND c.time <  e.period_end + INTERVAL 3 DAY
          THEN 4
        WHEN c.time >= e.period_end + INTERVAL 3 DAY
             AND c.time <  e.period_end + INTERVAL 7 DAY
          THEN 5
      ELSE 6
      END AS bucket_priority,
      CASE
        WHEN e.class IN ('IMP', 'SS') THEN 1
        ELSE 2
      END AS class_priority
    FROM conditions_in_range c
    JOIN encounter_filtered e
      ON c.patient = e.patient
  ", .con = con)
  dbExecute(con, query)

  query <- glue_sql("
    CREATE OR REPLACE TEMP VIEW condition_timing AS
    SELECT class, patient, con_id, condition_time, time_bucket
    FROM (
      SELECT *,
        ROW_NUMBER() OVER (
          PARTITION BY con_id
          ORDER BY bucket_priority ASC,
                  class_priority ASC,
          ABS(EPOCH(condition_time) - EPOCH(period_start)) ASC
        ) AS rn
      FROM condition_timing_all
    )
    WHERE rn = 1
  ", .con = con)
  dbExecute(con, query)

  result <- dbGetQuery(con, glue_sql("
    SELECT class, time_bucket, COUNT(*) AS anzahl
    FROM condition_timing
    GROUP BY class, time_bucket
    ORDER BY class, time_bucket
  ", .con = con))

  writeLogData("Condition timestamp compared to encounter timestamps: ", result)
}

matchNextPotassium <- function() {

  # For each Potassium measurement the chronological next measurement of the
  # the same patient is calculated. The new columns are:
  # next_flag, next_value, next_result, next_enc_id, next_hours
  # If there are two followup measurements that have the same timestamp obs_id
  # is used as a deterministic random (higher obs_id wins)

  query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE potassium_result AS
    SELECT
      p.*,
      n.value_norm     AS next_value,
      n.result    AS next_result,
      n.enc_id    AS next_enc_id,
      CASE WHEN n.obs_id IS NOT NULL THEN 1 ELSE 0 END AS next_flag,
      CASE WHEN n.obs_id IS NOT NULL THEN
        (EPOCH(n.time::TIMESTAMP) - EPOCH(p.time::TIMESTAMP)) / 3600.0
      ELSE NULL END AS next_hours
    FROM potassium_result p
    LEFT JOIN potassium_result n
      ON n.obs_id = (
        SELECT n2.obs_id
        FROM potassium_result n2
        WHERE n2.patient = p.patient
          AND n2.time::TIMESTAMP > p.time::TIMESTAMP
        ORDER BY n2.time::TIMESTAMP ASC, n2.obs_id DESC
        LIMIT 1
      )
  ", .con = con)
  dbExecute(con, query)

  # calculates same_encounter
  query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE potassium_result AS
    SELECT *,
      CASE
        WHEN enc_id IS NOT NULL AND next_enc_id IS NOT NULL AND enc_id = next_enc_id
        THEN 1 ELSE 0
      END AS same_encounter
    FROM potassium_result
  ", .con = con)
  dbExecute(con, query)

  # count total available follow-Up measurements. Follow-Ups in the same encounter
  # and AMB measurements that are followed by an IMP measurement within 12 hours

  count_query <- glue_sql("
    SELECT
      SUM(next_flag)  AS n_next_total,
      SUM(same_encounter) AS n_same_encounter,
      COUNT(*) FILTER (WHERE next_hours <= 120) AS n_5d
    FROM potassium_result
  ", .con = con)
  next_counts <- dbGetQuery(con, count_query)

  writeLogData("Total follow-up measurements, within same encounter, within 5d: ",
               next_counts)

  # get some statistics about the next follow-up

  # get follow-up statistics (avg, median, sd, p25, p75) per group
  stats_query <- glue_sql("
    SELECT
      AVG(next_hours)                                                     AS avg_total,
      MEDIAN(next_hours)                                                  AS median_total,
      STDDEV(next_hours)                                                  AS sd_total,
      QUANTILE_CONT(next_hours, 0.25)                                     AS p25_total,
      QUANTILE_CONT(next_hours, 0.75)                                     AS p75_total,

      AVG(next_hours)    FILTER (WHERE same_encounter = 1)                AS avg_same_encounter,
      MEDIAN(next_hours) FILTER (WHERE same_encounter = 1)                AS median_same_encounter,
      STDDEV(next_hours) FILTER (WHERE same_encounter = 1)                AS sd_same_encounter,
      QUANTILE_CONT(next_hours, 0.25) FILTER (WHERE same_encounter = 1)   AS p25_same_encounter,
      QUANTILE_CONT(next_hours, 0.75) FILTER (WHERE same_encounter = 1)   AS p75_same_encounter,

      AVG(next_hours)    FILTER (WHERE next_hours <= 120)                 AS avg_5d,
      MEDIAN(next_hours) FILTER (WHERE next_hours <= 120)                 AS median_5d,
      STDDEV(next_hours) FILTER (WHERE next_hours <= 120)                 AS sd_5d,
      QUANTILE_CONT(next_hours, 0.25) FILTER (WHERE next_hours <= 120)    AS p25_5d,
      QUANTILE_CONT(next_hours, 0.75) FILTER (WHERE next_hours <= 120)    AS p75_5d
    FROM potassium_result
  ", .con = con)

  wide <- dbGetQuery(con, stats_query)

  # reshape to get a better log output
  groups <- c("total", "same_encounter", "5d")
  stat_names <- c("avg", "median", "sd", "p25", "p75")

  result <- do.call(rbind, lapply(groups, function(g) {
    cols <- paste0(stat_names, "_", g)
    setNames(as.data.frame(wide[cols]), stat_names)
  }))
  result <- cbind(group = groups, result)

  num_cols <- sapply(result, is.numeric)
  result[num_cols] <- round(result[num_cols], 2)

  writeLogData("Follow-up in hours (avg, median, sd, p25, p75):",
               result, kanonymity = FALSE)

  # # get avarage follow-up times
  # average <- glue_sql("
  #   SELECT
  #     AVG(next_hours)                                    AS avg_hours_total,
  #     AVG(next_hours) FILTER (WHERE same_encounter = 1)  AS avg_hours_same_encounter,
  #     AVG(next_hours) FILTER (WHERE next_hours <= 120)   AS avg_hours_5d
  #   FROM potassium_result
  # ", .con = con)
  # next_average <- dbGetQuery(con, average)
  #
  # # round to two digits
  # num_cols <- sapply(next_average, is.numeric)
  # next_average[num_cols] <- round(next_average[num_cols], 2)
  #
  # writeLogData("Average hours (total, same-enc, 5d): ",
  #              next_average, kanonymity = FALSE)
}

matchProceduresAfter <- function() {

  writeLogData("Evaluate potential treatment medication. A medication is
               considered a match if it is given within 12 hours after measurement")

  after_interval <- sprintf("INTERVAL '%d hours'", 12)

  # check whether a dialysis (procedure) occured within 12 hours after
  # the potassium measurement

  after_sql <- glue_sql("
    CASE WHEN EXISTS (
      SELECT 1
      FROM procedures_start AS d
      WHERE d.patient = p.patient
        AND d.time::TIMESTAMP >  p.time::TIMESTAMP
        AND d.time::TIMESTAMP <= p.time::TIMESTAMP + {DBI::SQL(after_interval)}
    ) THEN 1 ELSE 0 END AS dialyse_after
  ", .con = con)

  query <- glue_sql("
    CREATE OR REPLACE TEMP TABLE potassium_result AS
    SELECT
      p.*,
      {DBI::SQL(after_sql)}
    FROM potassium_result p
  ", .con = con)
  dbExecute(con, query)

  count_query <- glue_sql("
    SELECT SUM(dialyse_after) AS n_after
    FROM potassium_result
  ", .con = con)
  dialysis_after_counts <- dbGetQuery(con, count_query)

  writeLogData("Evaluate subsequent dialysis. A dialysis is considered subsequent
               if it is given within 12 hours after measurement.")
  writeLogData("Total subsequent dialysis: ", dialysis_after_counts)
}

matchMedicationAfter <- function() {

  # only medication given as potential treatment is relevant
  after_groups <- atc_groups[atc_groups$name %in% c("gluc", "insul", "hyperk"), ]

  build_after_flag_sql <- function(atc_groups) {
    atc_groups |>
      group_by(name) |>
      summarise(
        cond = paste(paste0("m.atc LIKE '", atc, "%'"), collapse = " OR "),
        .groups = "drop"
      ) |>
      mutate(
        sql = glue(
          "CASE WHEN EXISTS (
            SELECT 1 FROM medadm_all_start m
            WHERE m.patient = p.patient
              AND m.time::TIMESTAMP >  p.time::TIMESTAMP
              AND m.time::TIMESTAMP <= p.time::TIMESTAMP + INTERVAL '12 hours'
              AND ({cond})
          ) THEN 1 ELSE 0 END AS {name}_after"
        )
      ) |>
      pull(sql)
  }

  flag_sql <- build_after_flag_sql(after_groups)

  query <- glue(
    "CREATE OR REPLACE TEMP TABLE potassium_result AS
   SELECT p.*,
      {paste(flag_sql, collapse = ',\n      ')}
   FROM potassium_result p"
  )
  dbExecute(con, query)

  after_cols <- paste0(unique(after_groups$name), "_after")

  query <- glue_sql("
    SELECT SUM(
        {SQL(paste(after_cols, collapse = ' + '))}
    ) AS anzahl
    FROM potassium_result
  ", .con = con)
  med_after_counts <- dbGetQuery(con, query)


  writeLogData("Total matches after: ", med_after_counts)
}

matchAndEvaluateSerumBlood <- function(availability, time_availability) {

  # if there are no conditions add a thromb column with all 0
  thromb_expr <- if (availability$conditions) {
    glue_sql("pr.thromb", .con = con)
  } else {
    glue_sql("0 AS thromb", .con = con)
  }
  # base table is a new potassium_result_loinc. Get encounter and thrombocytosis information.
  # filter out all measurements without encounter.
  query <- glue_sql("
  CREATE OR REPLACE TEMP TABLE potassium_result_loinc AS
    SELECT
      prl.*,
      pr.enc_id,
      {thromb_expr}
    FROM potassium_result_loinc prl
    JOIN potassium_result pr
      ON pr.obs_id = prl.obs_id
  ", .con = con)
  dbExecute(con, query)

  # get all serum values and match them with blood values.
  # so in the result obs_id_1/time_1/loinc_1/value_norm1 belongs to serum values.
  # xy_2 belongs to blood values.
  # diff_abs is always serum - blood
  query <- glue_sql("
  CREATE OR REPLACE TEMP TABLE kalium_pair_serum_blut AS
    SELECT *
    FROM (
      SELECT
        s.patient,
        s.enc_id,
        s.thromb,
        s.result as result_serum,
        b.result as result_blood,
        s.obs_id AS obs_id_1, s.time::TIMESTAMP AS time_1, s.value_norm AS value_norm_1, s.loinc AS loinc_1,
        b.obs_id AS obs_id_2, b.time::TIMESTAMP AS time_2, b.value_norm AS value_norm_2, b.loinc AS loinc_2,
        ABS(EPOCH(b.time::TIMESTAMP) - EPOCH(s.time::TIMESTAMP)) / 3600.0 AS hours_between,
        ABS(s.value_norm - b.value_norm)  AS diff_abs,
        s.value_norm - b.value_norm AS diff_signed,
        'serum_blut' AS pair_type,
        CAST(NULL AS VARCHAR) AS event_type,
        CASE
          WHEN s.time::TIMESTAMP = b.time::TIMESTAMP THEN 'simultaneous'
          WHEN s.time::TIMESTAMP < b.time::TIMESTAMP THEN 'serum_first'
          ELSE 'blood_first'
        END AS chronological_order,
        ROW_NUMBER() OVER (
          PARTITION BY s.obs_id
          ORDER BY ABS(EPOCH(b.time::TIMESTAMP) - EPOCH(s.time::TIMESTAMP)), b.obs_id
        ) AS rn
      FROM potassium_result_loinc s
      JOIN potassium_result_loinc b
        ON b.patient = s.patient
       AND b.enc_id  = s.enc_id
      WHERE s.sample_type = 'serum'
       AND b.sample_type = 'blood'
       AND ABS(EPOCH(b.time::TIMESTAMP) - EPOCH(s.time::TIMESTAMP)) / 3600.0 <= 6
       AND s.sample_type = 'serum'
    )
    WHERE rn = 1
  ", .con = con)
  count <- dbExecute(con, query)

  if(count == 0 ) {
    writeLogData("No measurements in blood and serum in a 6h time window. Skip
                 further evaluation")
    return(NULL)
  } else {
    writeLogData("Measurments in blood and serum in a 6h time window: ", count)
  }

  count_diff_abs <- function(where_conditions = character(0), kohorte = "all", chrono = "all") {
    conds <- where_conditions
    if (chrono != "all") {
      conds <- c(conds, glue::glue("chronological_order = '{chrono}'"))
    }
    where_sql <- if (length(conds) > 0) {
      DBI::SQL(paste("WHERE", paste(conds, collapse = " AND ")))
    } else {
      DBI::SQL("")
    }

    query <- glue::glue_sql(
      "SELECT
       COUNT(*) AS n,
       AVG(diff_signed) AS mean_diff_signed,
       PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY diff_signed) AS median_diff_signed,
       PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY diff_signed) AS p25_diff_signed,
       PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY diff_signed) AS p75_diff_signed,
       COUNT(*) FILTER (WHERE diff_signed > 0) AS n_serum_higher,
       COUNT(*) FILTER (WHERE diff_signed < 0) AS n_blood_higher,
       COUNT(*) FILTER (WHERE diff_signed = 0) AS n_equal,
       AVG(diff_abs) AS mean_diff_abs,
       PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY diff_abs) AS median_diff_abs,
       PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY diff_abs) AS p75_diff_abs,
       PERCENTILE_CONT(0.9) WITHIN GROUP (ORDER BY diff_abs) AS p90_diff_abs,
       COUNT(*) FILTER (WHERE result_serum = 'N') AS n_serum_n,
       COUNT(*) FILTER (WHERE result_serum = 'L') AS n_serum_l,
       COUNT(*) FILTER (WHERE result_serum = 'H') AS n_serum_h,
       COUNT(*) FILTER (WHERE result_blood = 'N') AS n_blood_n,
       COUNT(*) FILTER (WHERE result_blood = 'L') AS n_blood_l,
       COUNT(*) FILTER (WHERE result_blood = 'H') AS n_blood_h,
       COUNT(*) FILTER (WHERE result_serum = 'N' AND result_blood = 'N') AS n_serum_n_blood_n,
       COUNT(*) FILTER (WHERE result_serum = 'N' AND result_blood = 'L') AS n_serum_n_blood_l,
       COUNT(*) FILTER (WHERE result_serum = 'N' AND result_blood = 'H') AS n_serum_n_blood_h,
       COUNT(*) FILTER (WHERE result_serum = 'L' AND result_blood = 'N') AS n_serum_l_blood_n,
       COUNT(*) FILTER (WHERE result_serum = 'L' AND result_blood = 'L') AS n_serum_l_blood_l,
       COUNT(*) FILTER (WHERE result_serum = 'L' AND result_blood = 'H') AS n_serum_l_blood_h,
       COUNT(*) FILTER (WHERE result_serum = 'H' AND result_blood = 'N') AS n_serum_h_blood_n,
       COUNT(*) FILTER (WHERE result_serum = 'H' AND result_blood = 'L') AS n_serum_h_blood_l,
       COUNT(*) FILTER (WHERE result_serum = 'H' AND result_blood = 'H') AS n_serum_h_blood_h,
       AVG(hours_between) AS mean_hours_between,
       PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY hours_between) AS median_hours_between,
       {kohorte}  AS kohorte,
       {chrono}   AS chronological_order_filter
     FROM kalium_pair_serum_blut
     {where_sql}
     ",
      .con = con)
    dbGetQuery(con, query)
  }

  # If there are conditions and encounters evaluate thrombozytose.If not just
  # do base serum-blood comparison
  t_min <- NULL
  t_max <- NULL
  if (availability$conditions && availability$encounter) {
    min_verfügbar <- time_availability$min_verfügbar[time_availability$table_name == "conditions"]
    max_verfügbar <- time_availability$max_verfügbar[time_availability$table_name == "conditions"]
    t_min <- format(min_verfügbar, "%Y-%m-01")
    t_max <- format(max_verfügbar + months(1), "%Y-%m-01")
  }

  # init a list with all cohorts. Default is only all
  kohorten <- list(
    list(cond = character(0), label = "all")
  )

  # add thrombo kohorts if available.

  if (availability$conditions && availability$encounter) {
    thromb_time_filter <- glue::glue(
      "time_1::TIMESTAMP >= '{t_min}'::TIMESTAMP AND time_1::TIMESTAMP < '{t_max}'::TIMESTAMP"
    )
    kohorten <- c(kohorten, list(
      list(
        cond  = c("thromb = 1", thromb_time_filter, "enc_id IS NOT NULL"),
        label = "thromb"
      ),
      list(
        cond  = c("thromb = 0", thromb_time_filter, "enc_id IS NOT NULL"),
        label = "ohne_thromb"
      )
    ))
  }

  chronos <- c("all", "serum_first", "blood_first", "simultaneous")

  compare_serum_blood <- do.call(rbind, lapply(kohorten, function(k) {
    do.call(rbind, lapply(chronos, function(ch) {
      count_diff_abs(where_conditions = k$cond, kohorte = k$label, chrono = ch)
    }))
  }))

  n_thromb_all <- NULL

  if ("thromb" %in% compare_serum_blood$kohorte) {
    n_thromb_all <- compare_serum_blood$n[
      compare_serum_blood$kohorte == "thromb" &
        compare_serum_blood$chronological_order_filter == "all"
    ]
  } else {
    n_thromb_all <- 0
  }

  writeLogData("Number of pairs with a thrombocytosis diagnosis: ", n_thromb_all)

  compare_serum_blood <- applyKAnonymity(compare_serum_blood, "n", c("kohorte", "chronological_order_filter"))

  n_cols <- names(compare_serum_blood)[startsWith(names(compare_serum_blood), "n_")]

  compare_serum_blood <- applyKExtra(compare_serum_blood, n_cols)

  return(compare_serum_blood)
}

checkEncounterReferences <- function(availability) {

  if(!availability$encounter){
    writeLogData("No encounters available. Skipping")
    return(NULL)
  }

  checkEncounterTypes("potassium")

  if(availability$medication) {
    checkEncounterTypes("medadm_all")
  }

  if(availability$procedures) {
    writeLogData("icu and ops procedures:")
    checkEncounterTypes("procedures", "NOT code_type = 'icu_dauer' ")
    writeLogData("procedures from icu_dauer:")
    checkEncounterTypes("procedures", "code_type = 'icu_dauer' ")
  }

  if(availability$lab) {
    checkEncounterTypes("lab")
  }

  if(availability$condition) {
    checkEncounterTypes("conditions")
  }
}

checkEncounterTypes <- function(table, where = NULL) {

  where_expr <- glue_sql("", .con = con)

  if(!is.null(where)) {
    where_expr  <- glue_sql("WHERE {DBI::SQL(where)}", .con = con)
  }

  # check if there is a encounter reference and if yes get the encounter type
  query <- glue_sql("
       SELECT
        CASE
            WHEN t.encounter IS NULL
                THEN 'no_encounter_reference'
            WHEN e.enc_id IS NULL
                THEN 'encounter_reference_without_match'
            WHEN e.type IS NULL
                THEN 'encounter_type_is_null'
            ELSE e.type
        END AS encounter_type,
        COUNT(*) AS count
      FROM {table} AS t
      LEFT JOIN encounter_all AS e
          ON t.encounter = concat('Encounter/', e.enc_id)
      {where_expr}
      GROUP BY 1
    ", .con = con)

  found_enc_types <- dbGetQuery(con, query)

  writeLogData(paste0("Referenced encounter types in ", table, ":"))
  writeLogData(found_enc_types)

  return(found_enc_types)
}
