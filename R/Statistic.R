descriptiveStatistic <- function(availability, time_availability) {

  writeLogData("Counting the cooccurence of low/high potassium
                results with available covariates. Only
                time windows with reasonable available data are considered")

  all_results <- list()

  # init results
  results <- list()

  # get counts of next potassium measurement result (L/N/H or no result)
  count_next <- function(extra_where = "", kohorte = "all", followup_time = 120) {

    # convert parameter extra_and in SQL
    extra_where_sql <- DBI::SQL(extra_where)

    # next measurements older than 5 days are not counted as follow-up!
    query <- glue::glue_sql(
      " WITH base AS (
          SELECT
            result,
            CASE
              WHEN next_flag = 0 THEN 'next_no'
              WHEN next_flag = 1 AND next_hours > {followup_time} THEN 'next_no'
              WHEN next_result = 'H' THEN 'next_H'
              WHEN next_result = 'N' THEN 'next_N'
              WHEN next_result = 'L' THEN 'next_L'
            END AS gruppe
          FROM potassium_result p
          {extra_where_sql}
        )
        SELECT
          gruppe,
          result,
          COUNT(*) AS n_match,
          SUM(COUNT(*)) OVER (PARTITION BY result) - COUNT(*) AS n_no_match,
          SUM(COUNT(*)) OVER (PARTITION BY result) AS n_gesamt,
          {kohorte} AS kohorte
        FROM base
        GROUP BY gruppe, result
        ORDER BY result, gruppe
    ",
      .con = con
    )

    dbGetQuery(con, query)
  }

  counts_all <- count_next("", kohorte = "5days_all")

  # next measurements within 2 and 2-48 hours
  counts_2h <- count_next(extra_where = "", kohorte = "2hours_all", followup_time = 2)

  counts_48h <- count_next(extra_where = "", kohorte = "48hours_all", followup_time = 48)

  # same encounter
  counts_same_enc <- count_next("WHERE same_encounter = 1", kohorte = "same_encounter")

  # only first of encounter
  counts_first <- count_next("WHERE first = 1", kohorte = "only_first_5d")

  # bind all together
  all_next <- dplyr::bind_rows(counts_all, counts_2h, counts_48h, counts_same_enc, counts_first)

  # apply k-Anonymity (note as for lab at this point it is no real k-anonymization)
  counts_combined <- applyKAnonymity(all_next,
                                     "n_match", c("gruppe","kohorte","result"))

  # add a label
  counts_combined$source <- "next_measurement"
  # add it to all_results
  all_results <- c(all_results, list(counts_combined))
  # write results als .csv
  writeLogData("Result of next measurement done.")


  if(availability$medication) {

    # get the timestamps for reasonabe available data
    min_verfügbar <- time_availability$min_verfügbar[time_availability$table_name == "medadm_all_start"]
    max_verfügbar <- time_availability$max_verfügbar[time_availability$table_name == "medadm_all_start"]

    # the timestamp contain always the first day of the month. To do valid
    # comparisons later on add +1 month to the max timestamp
    t_min <- format(min_verfügbar, "%Y-%m-01")
    t_max <- format(max_verfügbar + months(1), "%Y-%m-01")

    # get atc col names
    atc_cols <- unique(atc_groups$name)

    count_atc <- function(extra_and = "", kohorte = "all") {

      # convert parameter extra_and in SQL
      extra_and_sql <- DBI::SQL(extra_and)

      # init results
      results <- list()

      # get the count for each atc in atc_cols and combine them in results
      for (col in atc_cols) {
        query <- glue::glue_sql(
          "SELECT
             {col}  AS gruppe,
             result,
             SUM({`col`} )     AS n_match,
             SUM(1 - {`col`} ) AS n_no_match,
             COUNT(*)   AS n_gesamt,
             {kohorte}  AS kohorte
           FROM potassium_result p
           WHERE p.time::TIMESTAMP >= {t_min}::TIMESTAMP
           AND p.time::TIMESTAMP < {t_max}::TIMESTAMP
           {extra_and_sql}
           GROUP BY p.result"
          , .con = con)

        results[[col]] <- dbGetQuery(con, query)
      }
      results <- bind_rows(results)
      return (results)
    }

    # get the counts for all cases (AMB and IMP)
    counts_alle <- count_atc()

    # get the counts for IMP only
    counts_no_amb <- count_atc("AND NOT enc_class = 'AMB'", kohorte = "without_AMB")

    # get the counts for first measurements only
    counts_first <- count_atc("AND first = 1", kohorte = "only_first")

    # get the counts for first measurements that are also the only match
    counts_alone <- count_atc("AND first = 1 AND alone_med = 1", kohorte = "first_alone")

    # bind all together
    counts_combined <- bind_rows(counts_alle, counts_no_amb, counts_first, counts_alone)

    # apply k-Anonymity
    counts_combined <- applyKAnonymity(counts_combined,
                                       "n_match", c("gruppe","kohorte","result"))

    # add a label
    counts_combined$source <- "medication"
    # add it to all_results
    all_results <- c(all_results, list(counts_combined))

    # Evaluate medication that is given AFTER the measurement

    # this is only relevant für medication given as potential treatment
    after_cols <- paste0(c("gluc", "insul", "hyperk"), "_after")

    count_atc_after <- function(extra_and = "", kohorte = "all") {
      extra_and_sql <- DBI::SQL(extra_and)
      results <- list()
      for (col in after_cols) {
        query <- glue::glue_sql(
          "SELECT
             {col}  AS gruppe,
             result,
             SUM({`col`} )     AS n_match,
             SUM(1 - {`col`} ) AS n_no_match,
             COUNT(*)   AS n_gesamt,
             {kohorte}  AS kohorte
           FROM potassium_result p
           WHERE p.time::TIMESTAMP >= {t_min}::TIMESTAMP
           AND p.time::TIMESTAMP < {t_max}::TIMESTAMP
           {extra_and_sql}
           GROUP BY p.result"
          , .con = con)

        results[[col]] <- dbGetQuery(con, query)
      }
      results <- bind_rows(results)
      return (results)
    }

    counts_after_alle    <- count_atc_after()
    counts_after_no_amb  <- count_atc_after("AND NOT enc_class = 'AMB'", kohorte = "without_AMB")
    counts_after_first   <- count_atc_after("AND first = 1", kohorte = "only_first")

    counts_after_combined <- bind_rows(counts_after_alle, counts_after_no_amb, counts_after_first)

    # apply k-Anonymity
    counts_after_combined <- applyKAnonymity(counts_after_combined,
                                             "n_match", c("gruppe","kohorte","result"))

    counts_after_combined$source <- "medication_after"
    all_results <- c(all_results, list(counts_after_combined))

    # write results als .csv
    writeLogData("Medication done")
  }

  if (availability$lab) {

    # all labels and all possible results
    lab_keys  <- c("glucose", "bicarbonat", "pH", "crea", "GFR")

    # init results
    results <- list()

    count_lab <- function(extra_where = "", kohorte = "lab") {

      # convert parameter extra_and in SQL
      extra_where_sql <- DBI::SQL(extra_where)

      # get the count for each column in lab_cols and combine them in results
      for (key in lab_keys) {
        for (suffix in c("H", "L")) {

          col        <- paste0(key, "_", suffix)
          normal_col <- paste0(key, "_N")
          low_col    <- paste0(key, "_L")
          high_col   <- paste0(key, "_H")

          query <- glue::glue_sql(
            "SELECT
             {col}  AS gruppe,
             result,
             SUM({`col`} )     AS n_match,
             SUM({`normal_col`} ) AS n_normal,
             SUM(
               CASE WHEN {`high_col`} + {`normal_col`} + {`low_col`} > 0
                    THEN 1 ELSE 0 END
             )                    AS n_measured,
             {kohorte}  AS kohorte
           FROM potassium_result p
          {extra_where_sql}
           GROUP BY p.result"
            , .con = con)

          results[[col]] <- dbGetQuery(con, query)
        }
      }

      result_lab <- bind_rows(results)
    }

    result_lab_all <- count_lab()
    # note: first = 1 is only for inpatient data.
    result_lab_first <- count_lab("WHERE first = 1", "lab_only_first")

    # first and amb. For lab values amb is also relevant if there are
    # measurements
    result_lab_first_or_amb <- count_lab("WHERE (first = 1 OR enc_class = 'AMB')",
                                         "lab_first_or_amb")

    # bind all together
    counts_comb_lab <- bind_rows(result_lab_all, result_lab_first, result_lab_first_or_amb)

    # apply k-Anonymity (note: no real k-anonymization because of matrix)
    result_lab <- applyKAnonymity(counts_comb_lab,
                                  "n_match", c("gruppe", "kohorte", "result"))

    result_lab$source <- "lab"
    all_results <- c(all_results, list(result_lab))
    # write results als .csv
    writeLogData("Lab done")
  }

  if (availability$procedures) {

    # get the timestamps for reasonable available data
    min_verfügbar <- time_availability$min_verfügbar[time_availability$table_name == "procedures_start"]
    max_verfügbar <- time_availability$max_verfügbar[time_availability$table_name == "procedures_start"]
    t_min <- format(min_verfügbar, "%Y-%m-01")
    t_max <- format(max_verfügbar + months(1), "%Y-%m-01")

    # define the three dialysis columns
    proc_cols <- c("dialyse_during", "dialyse_before", "dialyse_unclear", "dialyse_after")

    count_procedures <- function(extra_and = "", kohorte = "all") {
      extra_and_sql <- DBI::SQL(extra_and)
      results <- list()
      for (col in proc_cols) {
        query <- glue::glue_sql(
          "SELECT
           {col}      AS gruppe,
           result,
           SUM({`col`})     AS n_match,
           SUM(1 - {`col`}) AS n_no_match,
           COUNT(*)   AS n_gesamt,
           {kohorte}  AS kohorte
         FROM potassium_result p
         WHERE p.time::TIMESTAMP >= {t_min}::TIMESTAMP
         AND p.time::TIMESTAMP < {t_max}::TIMESTAMP
         {extra_and_sql}
         GROUP BY p.result"
          , .con = con)
        results[[col]] <- dbGetQuery(con, query)
      }
      results <- bind_rows(results)
      return(results)
    }

    # all encounters
    counts_alle <- count_procedures()
    # only IMP
    counts_no_amb <- count_procedures("AND NOT enc_class = 'AMB'", kohorte = "without_AMB")
    # only IMP and only first potassium_measurement
    counts_first <- count_procedures(
      "AND NOT enc_class = 'AMB' AND first = 1",
      kohorte = "without_AMB_first"
    )

    counts_combined <- bind_rows(counts_alle, counts_no_amb, counts_first)

    # apply k-anonymity
    counts_combined <- applyKAnonymity(counts_combined,
                                       "n_match", c("gruppe", "kohorte", "result"))

    counts_combined$source <- "procedures"
    all_results <- c(all_results, list(counts_combined))

    writeLogData("Procedures done")
  }

  if(availability$conditions) {
    # get the timestamps for reasonabe available data
    min_verfügbar <- time_availability$min_verfügbar[time_availability$table_name == "conditions"]
    max_verfügbar <- time_availability$max_verfügbar[time_availability$table_name == "conditions"]
    # the timestamp contain always the first day of the month. To do valid
    # comparisons later on add +1 month to the max timestamp
    t_min <- format(min_verfügbar, "%Y-%m-01")
    t_max <- format(max_verfügbar + months(1), "%Y-%m-01")
    # get icd col names
    cond_cols <- unique(icd_groups$name)
    count_condition <- function(extra_and = "", kohorte = "all") {
      # convert parameter extra_and in SQL
      extra_and_sql <- DBI::SQL(extra_and)
      # init results
      results <- list()
      # get the count for each condition group in cond_cols and combine them in results
      for (col in cond_cols) {
        query <- glue::glue_sql(
          "SELECT
             {col}  AS gruppe,
             result,
             SUM({`col`} )     AS n_match,
             SUM(1 - {`col`} ) AS n_no_match,
             COUNT(*)   AS n_gesamt,
             {kohorte}  AS kohorte
           FROM potassium_result p
           WHERE p.time::TIMESTAMP >= {t_min}::TIMESTAMP
           AND p.time::TIMESTAMP < {t_max}::TIMESTAMP
           {extra_and_sql}
           GROUP BY p.result"
          , .con = con)
        results[[col]] <- dbGetQuery(con, query)
      }
      results <- bind_rows(results)
      return (results)
    }
    # get the counts for all cases (AMB and IMP)
    counts_alle <- count_condition()
    # get the counts for IMP only
    counts_no_amb <- count_condition("AND NOT enc_class = 'AMB'", kohorte = "without_AMB")
    # get the counts for first measurements only
    counts_first <- count_condition("AND first = 1", kohorte = "only_first")
    # get the counts for first measurements that are also the only matched condition
    counts_alone <- count_condition("AND first = 1 AND alone_cond = 1", kohorte = "first_alone")
    # bind all together
    counts_combined <- bind_rows(counts_alle, counts_no_amb, counts_first)
    # apply k-Anonymity
    counts_combined <- applyKAnonymity(counts_combined,
                                       "n_match", c("gruppe","kohorte","result"))

    counts_combined$source <- "conditions"

    all_results <- c(all_results, list(counts_combined))
    # write results als .csv
    writeLogData("Conditions done")
  }

  final_counts <- bind_rows(all_results)

  return(final_counts)
}

##### linear regression ####

linearRegression <- function(availability, time_availability) {

  writeLogData("Measurements with unknown gender or age are excluded. Only
               time windows with reasonable available data are considered")

  all_results <- list()

  # function to get the timewindow for a given resource
  getWindow <- function(table_name) {
    row <- time_availability[time_availability$table_name == table_name, ]
    list(
      t_min = format(row$min_verfügbar, "%Y-%m-01"),
      t_max = format(row$max_verfügbar + months(1), "%Y-%m-01")
    )
  }

  # filter out patients with unknown gender and/or unknown age
  base_filter <- "AND gender IN ('male', 'female') AND age IS NOT NULL"

  # for all execpt lab we only want the first measurement of IMP cases
  stationary_filter <- paste0(base_filter, " AND first = 1")
  lab_filter <- paste0(base_filter, " AND (first = 1 OR enc_class = 'AMB')")



  # get number of excluded rows
  query_excluded <- glue_sql("
    SELECT
      SUM(CASE WHEN gender NOT IN ('male','female') THEN 1 ELSE 0 END) AS n_gender_unknown,
      SUM(CASE WHEN age IS NULL THEN 1 ELSE 0 END) AS n_age_null
    FROM potassium_result
  ", .con = con)
  excluded_counts <- dbGetQuery(con, query_excluded)
  writeLogData(
    "Mesurements excluded because of unknown gender/ unknown age: ",
    excluded_counts
  )

  # medication: all ATC together in one linear regresssion
  if (availability$medication) {
    w <- getWindow("medadm_all_start")
    atc_cols <- unique(atc_groups$name)

    atc_cols <- getValidCovariates(
      cols = atc_cols, start_time = w$t_min, end_time = w$t_max,
      k_value = k_value, stationary_filter, isContinuous = FALSE
    )

    if(length(atc_cols) > 0) {
      cols_sql <- paste(atc_cols, collapse = ", ")

      query <- glue_sql("
        SELECT value_norm, gender, age, {DBI::SQL(cols_sql)}
        FROM potassium_result
        WHERE time::TIMESTAMP >= {w$t_min}::TIMESTAMP
          AND time::TIMESTAMP <  {w$t_max}::TIMESTAMP
          {DBI::SQL(stationary_filter)}
      ", .con = con)
      res <- runLinearRegressionMain(query, atc_cols, "medication")
      all_results <- c(all_results, list(res))
    } else {
      writeLogData("Skipping medication")
    }

    # evaluate medication that is given AFTER the measurement
    after_labels <- c("gluc", "insul", "hyperk")
    for (lbl in after_labels) {
      res_med_after <- runAfterOutcomeRegression(
        paste0(lbl, "_after"), paste0("medication_after_", lbl),
        w, stationary_filter)
      all_results <- c(all_results, list(res_med_after))
    }
    writeLogData("Medication done")
  }

  # lab: do all lab-values separatly, we dont have them all measured at once
  if (availability$lab) {
    w <- getWindow("lab")
    lab_labels <- c("glucose", "bicarbonat", "pH", "crea", "GFR")

    for (label in lab_labels) {
      value_col <- paste0(label, "_value_norm")

      valid_lab_col <- getValidCovariates(
        cols = value_col, start_time = w$t_min, end_time = w$t_max,
        k_value = k_value, lab_filter, isContinuous = TRUE
      )

      if (length(valid_lab_col) > 0) {
        query <- glue_sql("
          SELECT value_norm, gender, age, {DBI::SQL(value_col)}
          FROM potassium_result
          WHERE time::TIMESTAMP >= {w$t_min}::TIMESTAMP
            AND time::TIMESTAMP <  {w$t_max}::TIMESTAMP
            AND {DBI::SQL(value_col)} IS NOT NULL
            {DBI::SQL(lab_filter)}
        ", .con = con)
        res <- runLinearRegressionMain(query, valid_lab_col,paste0("lab_", label))
        all_results <- c(all_results, list(res))
      }else {
        writeLogData(paste0("Skipping ", label))
      }
    }
    writeLogData("Lab done")
  }

  # procedures: all columns separately.
  if (availability$procedures) {
    w <- getWindow("procedures_start")
    proc_cols <- c("dialyse_during", "dialyse_before", "dialyse_unclear")

    for (col in proc_cols) {

      valid_proc_col <- getValidCovariates(cols = col,
        start_time = w$t_min, end_time = w$t_max,k_value = k_value,
        stationary_filter, isContinuous = FALSE)

      if(length(valid_proc_col) > 0) {
      query <- glue_sql("
        SELECT value_norm, gender, age, {DBI::SQL(col)}
        FROM potassium_result
        WHERE time::TIMESTAMP >= {w$t_min}::TIMESTAMP
          AND time::TIMESTAMP <  {w$t_max}::TIMESTAMP
          {DBI::SQL(stationary_filter)}
      ", .con = con)
      res <- runLinearRegressionMain(query,valid_proc_col,
                                    paste0("procedures_", col))
      all_results <- c(all_results, list(res))
      } else {
        writeLogData(paste0("Skipping ", col))
      }
    }
    # evaluate (any) dialysis given AFTER the result
    res_proc_after <- runAfterOutcomeRegression(
      "dialyse_after", "dialysis_after", w, stationary_filter)
    all_results <- c(all_results, list(res_proc_after))

    writeLogData("Procedures done")
  }

  # diagnosis: all together
  if (availability$conditions) {
    w <- getWindow("conditions")

    cond_cols <- unique(icd_groups$name)
    valid_cond_cols <- getValidCovariates(
      cols = cond_cols, start_time = w$t_min, end_time = w$t_max,
      k_value = k_value,stationary_filter, isContinuous = FALSE)

    cols_sql  <- paste(cond_cols, collapse = ", ")

    if (length(valid_cond_cols) > 0) {
      cols_sql <- paste(valid_cond_cols, collapse = ", ")
      query <- glue_sql("
        SELECT value_norm, gender, age, {DBI::SQL(cols_sql)}
        FROM potassium_result
        WHERE time::TIMESTAMP >= {w$t_min}::TIMESTAMP
          AND time::TIMESTAMP <  {w$t_max}::TIMESTAMP
          {DBI::SQL(stationary_filter)}
      ", .con = con)

      res <- runLinearRegressionMain(query, valid_cond_cols, "conditions")
      all_results <- c(all_results, list(res))
    } else {
      writeLogData("Skipping conditions")
    }
    writeLogData("Conditions done")
  }

  # time to next measurement vs. initial potassium result
  res_next_timing <- nextKTimingRegression()

  writeLogData("Follow-up measurement done")

  all_results <- c(all_results, list(res_next_timing))

  final_results <- bind_rows(all_results)

  return(final_results)
}

runLinearRegressionMain <- function(query, covariate_cols, source_name,
                                    row_threshold = 500000, outcome = "value_norm",
                                    factor_levels = list()) {

  # count all relevent rows
  count_query <- paste0("SELECT COUNT(*) AS n FROM (", query, ") AS sub")
  n_rows <- dbGetQuery(con, count_query)$n

  # skip if there are no rows
  if (n_rows == 0) {
    writeLogData(paste0("Skipping ", source_name, ": no matching rows"))
    return(NULL)
  }

  # mean age of this exact cohort. Centering age (age -mean_age) instead
  # of using raw age doesn't change the age coefficient or
  # any other coefficient, but it turns the Intercept into the model's
  # prediction for the reference gender/factor levels AT THE COHORT'S MEAN
  # AGE - i.e. the "average person" - instead of the meaningless/unobserved
  # "age = 0" case.
  mean_age_query <- paste0("SELECT AVG(age) AS mean_age FROM (", query, ") AS sub")
  mean_age <- dbGetQuery(con, mean_age_query)$mean_age

  # if there is only a small dataset do a normal regression. If it is a big
  # dataset run lmbig which gets the data in chunks.

  if (n_rows <= row_threshold) {
    data <- dbGetQuery(con, query)
    res <- runLinearRegression(data, covariate_cols, source_name,
                               outcome = outcome, factor_levels = factor_levels,
                               mean_age = mean_age)
    rm(data)
    gc()
    return(res)
  } else {
    return(runBigLinearRegression(query, covariate_cols, source_name,
                                  outcome = outcome, factor_levels = factor_levels,
                                  mean_age = mean_age))
  }
}

runLinearRegression <- function(data, covariate_cols, source_name,
                                outcome = "value_norm", factor_levels = list(),
                                mean_age = NULL) {

  # fix gender order so that we always have the same reference
  data$gender <- factor(data$gender, levels = c("male", "female"))

  # apply requested factor releveling (e.g. result: reference level = "N")
  # before fitting, so the reference category is explicit and deterministic
  # instead of relying on alphabetical default ordering
  for (col in names(factor_levels)) {
    data[[col]] <- factor(data[[col]], levels = factor_levels[[col]])
  }

  # center age on the cohort mean  so the Intercept becomes the prediction
  # for the "average person" (reference gender/factor levels, mean age)
  # instead of the unobserved "age = 0" case
  if (!is.null(mean_age) && !is.na(mean_age)) {
    data$age <- data$age - mean_age
  }

  # build linear regression with base values + covariates_cols
  formula_reg <- as.formula(
    paste(outcome, "~ gender + age +",
          paste(paste0("`", covariate_cols, "`"), collapse = " + ")))

  # human-readable model string
  model_string <- paste0(outcome, " ~ gender + age + ",
                         paste(covariate_cols, collapse = " + "))

  n_total <- nrow(data)

  model <- tryCatch(
    lm(formula_reg, data = data),
    error = function(e) {
      writeLogData(paste0("Linear regression failed for: ", source_name,
                          " with: ", conditionMessage(e)))
      return(NULL)
    }
  )

  if (is.null(model)) return(NULL)

  n_used <- nobs(model)

  if (n_used < k_value) {
    writeLogData(paste0(
      "Linear regression for: ", source_name, " had < k rows and was discarded"))
    rm(model)
    gc()
    return(NULL)
  }

  coefs <- as.data.frame(summary(model)$coefficients)
  coefs$term      <- rownames(coefs)
  rownames(coefs) <- NULL
  names(coefs) <- c("estimate", "std_error", "t_value", "p_value", "term")
  coefs$source  <- source_name
  coefs$n_used  <- n_used
  coefs$n_total <- n_total
  coefs$regression_type <- "normal"
  coefs$mean_age <- mean_age
  coefs$model <- model_string

  rm(model)
  gc()
  return(coefs)
}

runBigLinearRegression <- function(query, covariate_cols, source_name,
                                   outcome = "value_norm", factor_levels = list(),
                                   mean_age = NULL) {

  formula_reg <- as.formula(
    paste( outcome, "~ gender + age +",
           paste(paste0("`", covariate_cols, "`"), collapse = " + ")))

  model_string <- paste0(outcome, " ~ gender + age + ",
                         paste(covariate_cols, collapse = " + "))

  rs <- dbSendQuery(con, query)

  model <- NULL
  n_total <- 0
  first_chunk <- TRUE

  # do the regression in chunks of 100000
  repeat {
    data_chunk <- dbFetch(rs, n = 100000)

    if (nrow(data_chunk) == 0)
      break

    # make sure that in every chunk are male and female
    data_chunk$gender <- factor(data_chunk$gender, levels = c("male", "female"))

    # apply requested factor releveling (e.g. result: reference level = "N")
    # consistently in every chunk, otherwise biglm's update() would break
    # on chunks with differing/missing factor levels
    for (col in names(factor_levels)) {
      data_chunk[[col]] <- factor(data_chunk[[col]], levels = factor_levels[[col]])
    }

    # center age on the cohort mean, consistent with runLinearRegression()
    if (!is.null(mean_age) && !is.na(mean_age)) {
      data_chunk$age <- data_chunk$age - mean_age
    }


    n_total <- n_total + nrow(data_chunk)

    if (first_chunk) {
      model <- biglm(formula_reg, data = data_chunk)
      first_chunk <- FALSE
    } else {
      model <- update(model, moredata = data_chunk)
    }
    rm(data_chunk)
    gc()
  }

  dbClearResult(rs)

  if (is.null(model)) {
    writeLogData( paste0("No data for: ", source_name))
    return(NULL)
  }

  # get coefs (for biglm there is no $coefficients but $mat)
  coefs <- as.data.frame(summary(model)$mat)

  # rename columns. Original names are: Coef, CI_lower, CI_upper, SE, p
  names(coefs) <- c("estimate", "ci_lower", "ci_upper", "std_error", "p_value")

  coefs$t_value <- coefs$estimate / coefs$std_error

  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL

  coefs$source  <- source_name
  coefs$n_used  <- n_total
  coefs$n_total <- n_total
  coefs$regression_type <- "big"
  coefs$mean_age <- mean_age
  coefs$model    <- model_string

  # get the same column order as for normal lineare regression
  coefs <- coefs[, c("estimate", "std_error", "t_value", "p_value",
                      "term", "source", "n_used", "n_total", "regression_type",
                     "mean_age", "model")]

  if (n_total < k_value) {
    writeLogData(paste0("Big linear regression for: ", source_name,
                        " had < k rows and was discarded"))
    rm(model)
    gc()
    return(NULL)
  }

  rm(model)
  gc()
  return(coefs)
}

getValidCovariates <- function(cols, start_time, end_time, k_value, basefilter,
                               isContinuous = FALSE) {

  # get all cols that have no data or are constant. If it is a binery
  # column test if we have a relevant number of each case (min = k-value)

  if (isContinuous) {
    check_sql <- paste(
      sapply(cols, function(col) {
        as.character(glue_sql("
          SELECT
            {col} AS column_name,
            COUNT({`col`}) AS n_values,
            COUNT(DISTINCT {`col`}) AS n_unique,
            STDDEV({`col`}) AS sd_value
          FROM potassium_result
          WHERE time::TIMESTAMP >= {start_time}::TIMESTAMP
            AND time::TIMESTAMP < {end_time}::TIMESTAMP
            AND {`col`} IS NOT NULL
            {DBI::SQL(basefilter)}
        ", .con = con))
      }), collapse = "\nUNION ALL\n")
  } else {
    check_sql <- paste(
      sapply(cols, function(col) {
        as.character(glue_sql("
          SELECT
            {col} AS column_name,
            COUNT(DISTINCT {`col`}) AS n_unique,
            SUM(CASE WHEN {`col`} = 0 THEN 1 ELSE 0 END) AS n_zero,
            SUM(CASE WHEN {`col`} = 1 THEN 1 ELSE 0 END) AS n_one
          FROM potassium_result
          WHERE time::TIMESTAMP >= {start_time}::TIMESTAMP
            AND time::TIMESTAMP < {end_time}::TIMESTAMP
            {DBI::SQL(basefilter)}
        ", .con = con))
      }), collapse = "\nUNION ALL\n")
  }

  check_result <- dbGetQuery(con,check_sql)

  if (isContinuous) {

    problematic_cols <- check_result$column_name[
      check_result$n_values < k_value |
        check_result$n_unique <= 1 |
        check_result$sd_value == 0]
  } else {

    problematic_cols <- check_result$column_name[
      check_result$n_unique <= 1 |
        (check_result$n_unique == 2
         & pmin(check_result$n_zero, check_result$n_one, na.rm = TRUE) < k_value)
    ]
  }

  valid_cols <- setdiff(cols, problematic_cols)

  return(valid_cols)
}

nextKTimingRegression <- function() {

  # follow-up measurements after 5 days no longer count as follow-up
  max_days = 5
  max_hours <- max_days * 24

  # get counts of valid and excluded rows
  query_followUps <- glue_sql("
    SELECT
      SUM(CASE WHEN next_flag = 1 AND next_hours <= {max_hours} THEN 1 ELSE 0 END)
        AS n_valid,
      SUM(CASE WHEN next_flag = 0 THEN 1 ELSE 0 END) AS n_no_next,
      SUM(CASE WHEN next_flag = 1 AND next_hours > {max_hours} THEN 1 ELSE 0 END)
        AS n_next_too_late
    FROM potassium_result
  ", .con = con)
  followup_counts <- dbGetQuery(con, query_followUps)
  writeLogData("Measurements with a follow-up/ no follow-up/ too late follow-up: ",
    followup_counts)


  all_results <- list()

  # runs both models (categorical + numeric) for one cohort
  run_cohort <- function(extra_filter_sql, cohort_suffix) {

    base_filter <- glue_sql("
      WHERE gender IN ('male', 'female')
        AND age IS NOT NULL
        AND next_flag = 1
        AND next_hours <= {max_hours}
        {DBI::SQL(extra_filter_sql)}
    ", .con = con)

    # gender is a predictor in both models - if it isn't safely estimable in
    # this cohort, neither model is
    gender_ok <- hasValidFactorLevels(
      base_filter, "gender", paste0("next_k_timing_*_", cohort_suffix))

    res_cat <- NULL
    res_num <- NULL

    if (gender_ok) {

      # Model A (primary): categorical result, reference level = "N"
      if (hasValidFactorLevels(
        base_filter, "result",
        paste0("next_k_timing_categorical_", cohort_suffix))) {

        query_cat <- glue_sql("
          SELECT next_hours, gender, age, result
          FROM potassium_result
          {base_filter}
        ", .con = con)

        res_cat <- runLinearRegressionMain(
          query_cat, "result", paste0("next_k_timing_categorical_", cohort_suffix),
          outcome = "next_hours",
          factor_levels = list(result = c("N", "L", "H"))
        )
      }
    }

    # Second model: is there a follow up at all? First model only evaluates
    # cases with a follow up
    base_filter_occurred <- glue_sql("
      WHERE gender IN ('male', 'female')
        AND age IS NOT NULL
        {DBI::SQL(extra_filter_sql)}
    ", .con = con)

    res_occurred <- NULL

    if (hasValidFactorLevels(
      base_filter_occurred, "gender",
      paste0("next_k_occurred_", cohort_suffix)) &&
      hasValidFactorLevels(
        base_filter_occurred, "result",
        paste0("next_k_occurred_", cohort_suffix))) {

      query_occurred <- glue_sql("
        SELECT next_flag, gender, age, result
        FROM potassium_result
        {base_filter_occurred}
      ", .con = con)

      res_occurred <- runLinearRegressionMain(
        query_occurred, "result", paste0("next_k_occurred_", cohort_suffix),
        outcome = "next_flag",
        factor_levels = list(result = c("N", "L", "H"))
      )
    }

    return(list(res_cat, res_num, res_occurred))
  }


  # all measurements
  all_results <- run_cohort("", "all")
  # only the first measurement per encounter
  all_results <- c(all_results, run_cohort("AND first = 1", "only_first"))

  final_results <- bind_rows(all_results)
  return(final_results)
}

# checks whether every observed level of a categorical predictor has at
# least k_value rows in the given (already filtered) cohort.
hasValidFactorLevels <- function(base_filter_sql, col, source_name) {

  level_query <- glue_sql("
      SELECT {`col`} AS level, COUNT(*) AS n
      FROM potassium_result
      {base_filter_sql}
      GROUP BY {`col`}
    ", .con = con)
  level_counts <- dbGetQuery(con, level_query)

  if (nrow(level_counts) < 2) {
    writeLogData(paste0(
      "Skipping ", source_name, ": only one observed level of ", col))
    return(FALSE)
  }

  sparse <- level_counts$level[level_counts$n < k_value]
  if (length(sparse) > 0) {
    writeLogData(paste0(
      "Skipping ", source_name, " ", col, " has < k_value rows",
      paste(sparse, collapse = ", ")))
    return(FALSE)
  }

  return(TRUE)
}

runAfterOutcomeRegression <- function(outcome_col, label, w, extra_filter_sql) {

  base_filter <- glue_sql("
    WHERE time::TIMESTAMP >= {w$t_min}::TIMESTAMP
      AND time::TIMESTAMP <  {w$t_max}::TIMESTAMP
      {DBI::SQL(extra_filter_sql)}
  ", .con = con)

  all_results <- list()

  # gender is a predictor in both models - if it isn't safely estimable,
  # neither model is
  gender_ok <- hasValidFactorLevels(
    base_filter, "gender", paste0(label, "_*"))
  outcome_ok <- hasValidFactorLevels(
    base_filter, outcome_col, paste0(label, "_*"))

  if (gender_ok && outcome_ok) {

    # Model A: categorical result, reference level = "N"
    if (hasValidFactorLevels(
      base_filter, "result", paste0(label, "_categorical"))) {

      query_cat <- glue_sql("
        SELECT {`outcome_col`}, gender, age, result
        FROM potassium_result
        {base_filter}
      ", .con = con)

      res_cat <- runLinearRegressionMain(
        query_cat, "result", paste0(label, "_categorical"),
        outcome = outcome_col,
        factor_levels = list(result = c("N", "L", "H"))
      )
      all_results <- c(all_results, list(res_cat))
    }
  }

  return(bind_rows(all_results))
}


