
calculatePotassiumStatistics <- function()
{
  writeLogHeader("Part1: Potassium-Values analysis.",
                "This part describes available data in potassium observations.
                Also it calculats basic statisics and value distribution.
                This is done separatly per loinc, unit and different age-groups.
                This part uses only potassium observations and patient data.")

  writeLog("Loading data")

  createTablesForPotassiumAnalysis()

  writeLog("Start dataset description")

  results_description_all <- describeDataset()

  writeLog("Count values per month")

  results_timeline <- calculateTimeline()

  writeLog("Calculate statisics and distribution.")
  writeLogData("This is done for the whole cohort and then repeated separately
               by gender and age.")

  results_stat_dist <- calculateStatisticsAll()
  results_stat <- results_stat_dist$stat_all
  results_dist  <- results_stat_dist$dist_all

  writeLog("Write results as .csv")

  write.csv(results_description_all,
            file = paste0(output_dir, "/descriptionPotassiumObservations.csv"))
  write.csv(results_timeline,
            file = paste0(output_dir, "/countPotassiumValuesPerMonth.csv"))
  write.csv(results_stat,
            file = paste0(output_dir, "/statisticsPotassiumValues.csv"))
  write.csv(results_dist,
            file = paste0(output_dir, "/distributionPotassiumValues.csv"))

  #clean up database
  cleanUpPotassiumAnalysis()

  writeLog("Finished Potassium-Values analysis")
}

createTablesForPotassiumAnalysis <- function() {

  # create temp table for ALL potassium_data. Base Table contains only
  # key informations
  path <- paste0(data_dir, "/", name_of_lab_csv)

  types_clause <- getVarcharTypeClause(c(obs_value_code, obs_method_code))

  query <- glue_sql("
        CREATE OR REPLACE TEMP TABLE potassium_data_raw AS
        SELECT *
        FROM read_csv_auto({path}, {types_clause})
        WHERE {`obs_loinc_system`} = 'http://loinc.org'
        AND {`obs_loinc`} IN ({LOINCs_Kalium*})
        AND {`obs_time`} >= {global_min_time}
        AND {`obs_time`} <= {global_max_time}
      ", .con = con)

  dbExecute(con, query)

  query <- glue_sql("
        CREATE OR REPLACE TEMP VIEW potassium_data AS
        SELECT *
        FROM potassium_data_raw
        WHERE {`obs_status`} IS NULL
        OR NOT {`obs_status`} IN ('cancelled', 'entered-in-error')
      ", .con = con)

  dbExecute(con, query)

  checkForDuplicates("potassium_data", obs_id, obs_loinc)

  query <- glue_sql("
      SELECT {`obs_status`}, count(*) as count
      FROM potassium_data_raw
      GROUP BY {`obs_status`}
    ", .con = con)

  count_status <- dbGetQuery(con, query)

  writeLogData("Found obs_status: ",count_status)
  writeLogData("Observations with status cancelled or entered-in-error are
               excluded from further evaluation")


  # base view for timeline
  query <- glue_sql("
        CREATE OR REPLACE TEMP VIEW potassium_data_time AS
        SELECT DISTINCT
          loinc,
          value,
          unit,
          obs_id,
          time,
          comparator
        FROM potassium
      ", .con = con)

  dbExecute(con, query)
  checkForDuplicates("potassium_data_time", "obs_id", "loinc")

  # view for the statistics part
  query <- glue_sql("
        CREATE OR REPLACE TEMP VIEW potassium_part1 AS
          SELECT DISTINCT
          obs_id, loinc, value, unit, ref_low, ref_high,
          ref_high_unit, ref_low_unit, comparator,
          time, gender, age, pat_id, patient
        FROM potassium
      ", .con = con)

  dbExecute(con, query)
  checkForDuplicates("potassium_part1", "obs_id", "loinc")
}


describeDataset <- function() {

  # this function uses the select * view with the original column names

  # write basic counts to log
  query <- glue_sql("
      SELECT count(distinct {`obs_patient`})
      FROM potassium_data
    ", .con = con)

  count_patients <- dbGetQuery(con, query)

  writeLogData("Number of patients (cohort): ", count_patients)

  query <- glue_sql("
      SELECT {`obs_loinc`}, count(distinct {`obs_id`})
      FROM potassium_data
      GROUP BY {`obs_loinc`}
    ", .con = con)

  count_loincs <- dbGetQuery(con, query)

  writeLogData("Number of measurements per LOINC: ", count_loincs)

  # get the distincts of interpretation an write counts to log

  query <- glue_sql("
      SELECT {`obs_loinc`}, {`obs_interpretation`}, count(distinct {`obs_id`})
      FROM potassium_data
      GROUP BY {`obs_loinc`}, {`obs_interpretation`}
    ", .con = con)

  count_interpretation <- dbGetQuery(con, query)


  # Count distinct interpretations. This is to check if it matches with
  # reference ranges and if we could get more detailed information
  writeLogData("Number of distint interpretations per LOINC: ",
               count_interpretation)

  # check issued
  writeLogData("Check issued timestamp: ")
  query <- glue_sql("
      SELECT
        COUNT(DISTINCT CASE WHEN issued IS NULL THEN obs_id END) AS issued_na,
        COUNT(DISTINCT CASE WHEN issued = time  THEN obs_id END) AS issued_identical,
        COUNT(DISTINCT CASE WHEN issued < time  THEN obs_id END) AS issued_before,
        COUNT(DISTINCT CASE WHEN issued > time  THEN obs_id END) AS issued_after
      FROM potassium
    ", .con = con)

  issued_comparison <- dbGetQuery(con, query)
  writeLogData("Issued NA/same/before/after effectivedatetime (all data): ", issued_comparison)

  # describe which data is present in the observation table

  # get the name and type of all columns
  table_info <- dbGetQuery(con, "PRAGMA table_info('potassium_data')")

  # counts all non-NA entries of every column.
  # empty strings "" should count as NA. This check is only possible for
  # VARCHAR columns.

  count_expr_list <- map(
    seq_len(nrow(table_info)),
    function(i) {
      col  <- table_info$name[i]
      type <- table_info$type[i]

      if (grepl("VARCHAR", type)) {
        glue_sql(
          "COUNT(NULLIF({`col`}, '')) AS {`col`}",
          .con = con
        )
      } else {
        glue_sql(
          "COUNT({`col`}) AS {`col`}",
          .con = con
        )
      }
    }
  )

  # count_expr_list contains a count_expression for each column in a list.
  # this generates one SQL that can be used in the final query
  count_expr <- glue_sql_collapse(count_expr_list, sep = ",\n")

  query <- glue_sql("
      SELECT
        {`obs_loinc`} AS loinc_code,
        {count_expr}
      FROM potassium_data
      GROUP BY {`obs_loinc`}
    ", .con = con)

  results_description_all <- dbGetQuery(con, query)

  results_description_all <- applyKAnonymity(results_description_all,
                                            obs_patient, c("loinc_code"))

  # change rows and columns to make the result more readable
  results_description_all <- as.data.frame(t(results_description_all))

  return (results_description_all)
}

calculateTimeline <- function() {

  min_max <- dbGetQuery(con,
                        "SELECT MIN(time) AS min_t, MAX(time) AS max_t
     FROM potassium_data_time")
  writeLogData("min_potassium_date: ", min_max$min_t, kanonymity = FALSE)
  writeLogData("max_potassium_date: ", min_max$max_t, kanonymity = FALSE)

  result <- dbGetQuery(con, "
    SELECT
      loinc,
      unit,
      DATE_TRUNC('month', time::TIMESTAMP) AS month,
      COUNT(*)  AS n,
      COUNT(*) FILTER (WHERE comparator IS NOT NULL OR value IS NULL) AS n_excluded,
      MEDIAN(value) FILTER (WHERE comparator IS NULL AND value IS NOT NULL) AS median_value
    FROM potassium_data_time
    GROUP BY loinc, unit, DATE_TRUNC('month', time::TIMESTAMP)
    ORDER BY loinc, unit, month
  ")

  result <- applyKAnonymity(result, "n", c("loinc", "unit", "month"))

  return(result)
}

calculateStatisticsAll <- function () {

  # get patient counts per gender
  query <- glue_sql("
      SELECT gender, count(distinct pat_id)
      FROM potassium_part1
      GROUP BY gender
    ", .con = con)

  count_patients <- dbGetQuery(con, query)

  writeLogData("Total patients per gender: ", count_patients)

  # get measurements counts per gender
  query <- glue_sql("
      SELECT gender, count(distinct obs_id)
      FROM potassium_part1
      GROUP BY gender
    ", .con = con)

  count_measure <- dbGetQuery(con, query)
  writeLogData("Total measurements per gender: ", count_measure)

  # get measurements with comparators
  query <- glue_sql("
      SELECT count(1) as n
      FROM potassium_part1
      WHERE comparator IS NOT NULL
    ", .con = con)

  count_comp <- dbGetQuery(con, query)$n
  writeLogData("Measurements with comparator: ", count_comp)

  if(count_comp > 0) {
    writeLogData(paste0("Note: Measurements with a comparator-result will be ",
                 "excluded from statistics and distribution"))
  }

  # get min and max age
  range_query <- glue_sql("
    SELECT MIN(age) AS min_age,
           MAX(age) AS max_age
    FROM potassium_part1
  ", .con = con)

  age_range <- dbGetQuery(con, range_query)

  writeLogData("min age: ",age_range$min_age, kanonymity = FALSE)
  writeLogData("max age: ",age_range$max_age, kanonymity = FALSE)

  # get count < 0 and count > 120
  range_query <- glue_sql("
    SELECT count(distinct obs_id) as n
    FROM potassium_part1
    WHERE age < 0
  ", .con = con)

  count_low_age <- dbGetQuery(con, range_query)$n

  range_query <- glue_sql("
    SELECT count(distinct obs_id) as n
    FROM potassium_part1
    WHERE age > 120
  ", .con = con)

  count_high_age <- dbGetQuery(con, range_query)$n

  # write counts to log
  writeLogData("count measurements with age < 0: ", count_low_age)
  writeLogData("count measurements with age > 120: ", count_high_age)


  writeLogData(paste0("Info: Patients with unknown gender ",
               "are excluded from further gender statistics"))

  # define relevant age groups
  age_groups <- list(
    c("all", "all"),
    c(0, 0),
    c(1, 5),
    c(6, 12),
    c(13, 17),
    c(18, 39),
    c(40, 64),
    c(65, 120)
  )

  # define genders. Note that unknown is not included. Unknown patients are
  # excluded from gender/age statistics
  genders <- list(
    all = "all",
    female = "female",
    male   = "male"
  )

  # init results and a counter
  results_stat_all <- list()
  results_dist_all <- list()

  # do the statistics for all age_groups and all gender (including all)

  for (age_range in age_groups) {

    age_min <- age_range[1]
    age_max <- age_range[2]

    for (gender_name in names(genders)) {

      gender <- genders[[gender_name]]

      createViewForStatistics(age_min, age_max, gender)

      # calculate statistics
      result_stat <- calculateStatistics("potassium_data_gender_age")

      #if there are no data at all for this gender/age combination skip
      if (nrow(result_stat) == 0) next

      # add information about the current calculations
      result_stat$age_min <- age_min
      result_stat$age_max <- age_max
      result_stat$gender  <- gender_name

      # combine the statistics results
      results_stat_all[[length(results_stat_all) + 1]] <- result_stat

      # calculate distribution
      result_dist <- calculateDistribution("potassium_data_gender_age")

      # if there are no data it should have already stopped after
      # calculateStatistics. But to be sure skip again if there
      # there are no data
      if (nrow(result_dist) == 0) next

      # add information about the current calculations
      result_dist$age_min <- age_min
      result_dist$age_max <- age_max
      result_dist$gender  <- gender_name

      # combine the distribution results
      results_dist_all[[length(results_dist_all) + 1]] <- result_dist
    }
  }

  # combine all and return together as list
  stat_all <- do.call(rbind, results_stat_all)
  dist_all <- do.call(rbind, results_dist_all)

  return(list(
    stat_all = stat_all,
    dist_all  = dist_all
  ))
}

createViewForStatistics <- function (age_low, age_high, gender) {

  # if age_low and/or age_high are "all" no age filter is applied
  age_filter <- if(age_low != "all" && age_high != "all"){
    glue_sql("AND age BETWEEN {age_low} AND {age_high}", .con = con)
  } else {
    SQL("")
  }

  # if gender = "all" no gender filter is applied
  gender_filter <- if (!gender == "all") {
    glue_sql("AND gender = {gender}", .con = con)
  } else {
    SQL("")
  }

  # where 1= 1 enables the filter snippets to add anything with AND
  query <- glue_sql("
        CREATE OR REPLACE TEMP VIEW potassium_data_gender_age AS
        SELECT *
        FROM potassium_part1
        WHERE 1 = 1
          {age_filter}
          {gender_filter}
      ", .con = con)

  dbExecute(con, query)
}

calculateStatistics <- function(table_to_check) {

  # For the complete referencerange get the low and high referencerange
  # and paste them together with a "-".
  # If low is missing add "ab " if high is missing add "bis "

  ref_range_expr <- DBI::SQL(glue_sql("
    CASE
      WHEN ref_high IS NULL AND ref_low IS NOT NULL
        THEN CONCAT('ab ', CAST(ref_low AS VARCHAR))
      WHEN ref_low IS NULL AND ref_high IS NOT NULL
        THEN CONCAT('bis ', CAST(ref_high AS VARCHAR))
      WHEN ref_low IS NOT NULL AND ref_high IS NOT NULL
        THEN CONCAT(
          CAST(ref_low AS VARCHAR),
          '-',
          CAST(ref_high AS VARCHAR)
        )
      ELSE NULL
    END", .con = con))

  # expressions to calculate the number of values under/in/above/without
  # the referencerange

  n_above_ref_expr <- DBI::SQL(glue_sql("
    COUNT(CASE WHEN value IS NOT NULL
               AND ref_high IS NOT NULL
               AND value > ref_high THEN 1 END)",
                                        .con = con))

  n_below_ref_expr <- DBI::SQL(glue_sql("
    COUNT(CASE WHEN value IS NOT NULL
               AND ref_low IS NOT NULL
               AND value < ref_low THEN 1 END)",
                                        .con = con))

  n_within_ref_expr <- DBI::SQL(glue_sql("
    COUNT(CASE WHEN value IS NOT NULL
               AND ref_low IS NOT NULL
               AND ref_high IS NOT NULL
               AND value >= ref_low
               AND value <= ref_high THEN 1 END)",
                                         .con = con))

  n_no_ref_expr <- DBI::SQL(glue_sql("
    COUNT(CASE WHEN value IS NOT NULL
               AND ref_low IS NULL
               AND ref_high IS NULL THEN 1 END)",
                                     .con = con))


  # calculate all the statistics and add the expressions defined above
  query <- glue_sql("
  SELECT
    loinc,
    unit,
    AVG(value) AS mean_value,
    MEDIAN(value) AS median_value,
    STDDEV(value) AS sd_value,
    MIN(value) AS min_value,
    MAX(value) AS max_value,
    COUNT(*) AS n,
    COUNT(distinct patient) AS n_pat,
    quantile_cont(value, 0.01) AS p01,
    quantile_cont(value, 0.05) AS p05,
    quantile_cont(value, 0.10) AS p10,
    quantile_cont(value, 0.15) AS p15,
    quantile_cont(value, 0.20) AS p20,
    quantile_cont(value, 0.25) AS p25,
    quantile_cont(value, 0.30) AS p30,
    quantile_cont(value, 0.35) AS p35,
    quantile_cont(value, 0.40) AS p40,
    quantile_cont(value, 0.45) AS p45,
    quantile_cont(value, 0.50) AS p50,
    quantile_cont(value, 0.55) AS p55,
    quantile_cont(value, 0.60) AS p60,
    quantile_cont(value, 0.65) AS p65,
    quantile_cont(value, 0.70) AS p70,
    quantile_cont(value, 0.75) AS p75,
    quantile_cont(value, 0.80) AS p80,
    quantile_cont(value, 0.85) AS p85,
    quantile_cont(value, 0.90) AS p90,
    quantile_cont(value, 0.95) AS p95,
    quantile_cont(value, 0.99) AS p99,
    SUM(CASE WHEN value IS NULL THEN 1 ELSE 0 END) AS n_na,
    {n_below_ref_expr} AS n_below_ref,
    {n_within_ref_expr} AS n_within_ref,
    {n_above_ref_expr} AS n_above_ref,
    {n_no_ref_expr} AS n_no_ref,
    STRING_AGG(DISTINCT ({ref_range_expr}),', ') AS reference_ranges,
    STRING_AGG(DISTINCT ref_low_unit,', ') AS reference_low_units,
    STRING_AGG(DISTINCT ref_high_unit,', ') AS reference_high_units
  FROM {`table_to_check`}
  WHERE comparator IS NULL
  GROUP BY loinc, unit
  ", .con = con)

  results_statistics_all <- dbGetQuery(con, query)

  results_statistics_all <- applyKAnonymity(results_statistics_all,
                                            "n_pat", c("loinc", "unit"))

  return(results_statistics_all)
}

calculateDistribution <- function(table_to_check) {

  # get all relevant groups (loinc/units)
  # this should be the same for all age/gender whatever groups
  # so here we use just the base table

  query_stats <- glue::glue_sql("
    SELECT DISTINCT loinc, unit
    FROM potassium_part1
    WHERE value IS NOT NULL
    ", .con = con)

  bounds <- DBI::dbGetQuery(con, query_stats)

  # most potassium values are measured in mmol/l.
  # 2-8 should cover the clinical range.

  bounds$lower <- 2.0
  bounds$upper <- 8.0

  results <- list()

  # do the calculation for each bin entry. (each loinc/unit pair has its
  # own entries)
  # filter is done in the where clause
  for (i in seq_len(nrow(bounds))) {

    loinc  <- bounds$loinc[i]
    unit   <- bounds$unit[i]
    lower  <- bounds$lower[i]
    upper  <- bounds$upper[i]

    # query to get the counts for each bin.
    # generate_series can only handle real numbers. Therefore
    # bins and values are *10. The names of the bins are shown as
    # /10 to correct for this.

    query <- glue_sql("
      WITH bins AS (
        -- min bin
        SELECT
          -1 AS bin_key,
          NULL::DOUBLE AS bin_start,
          {lower} AS bin_end

        UNION ALL
        -- regular bins
        SELECT
          val AS bin_key,
          val / 10.0 AS bin_start,
          (val + 1) / 10.0 AS bin_end
        FROM generate_series(
          CAST({lower} * 10 AS BIGINT),
          CAST({upper} * 10 - 1 AS BIGINT),
          1
        ) AS t(val)

        UNION ALL
        -- max bin
        SELECT
          999999 AS bin_key,
          {upper} AS bin_start,
          NULL::DOUBLE AS bin_end
      ),

      counts AS (
        SELECT
          CASE
            WHEN value < {lower} THEN -1
            WHEN value >= {upper} THEN 999999
            ELSE FLOOR(value * 10)
          END AS bin_key,
          COUNT(*) AS n
        FROM {`table_to_check`}
        WHERE loinc = {loinc}
          AND unit = {unit}
          AND value IS NOT NULL
          AND comparator IS NULL
        GROUP BY 1
      )

      SELECT
        {loinc} AS loinc,
        CAST({unit} AS VARCHAR) AS unit,
        b.bin_start,
        b.bin_end,
        COALESCE(c.n, 0) AS n
      FROM bins b
      LEFT JOIN counts c USING (bin_key)
      ORDER BY b.bin_key
    ", .con = con)

    results[[i]] <- dbGetQuery(con, query)
  }

  hist_data <- bind_rows(results)

  hist_data <- applyKAnonymity(hist_data, "n", c("loinc", "unit",
                                                 "bin_start", "bin_end"))
  return(hist_data)
}

cleanUpPotassiumAnalysis <- function() {

  # keep base table potassium. delete everything else
  query <- glue_sql("
      DROP VIEW IF EXISTS potassium_data_time;
      DROP VIEW IF EXISTS potassium_data;
      DROP VIEW IF EXISTS potassium_data_gender_age;
      DROP VIEW IF EXISTS potassium_part1;
      DROP TABLE IF EXISTS potassium_data_raw;
    ", .con = con)
  dbExecute(con, query)
}
