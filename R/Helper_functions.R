######
# Functions for logging
######

# creates the output dir
createOutputDir <- function(main_output_dir) {
  # create a folder with the current timestamp
  # the <<- writes it in the global environment
  output_dir <<- paste0(main_output_dir, "/",
                       format(Sys.time(), "%Y-%m-%d_%H-%M-%S") )
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  return (output_dir)
}

# creates a new log file in the output dir
createLog <- function(output_dir) {
  # create logfile
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  log_file <- file.path(output_dir, paste0("Kalium360_",
                                           timestamp,"-log", ".txt"))
  # open connection for logfile
  log_con <- file(log_file, open = "wt")
  sink(log_con, append = TRUE, split = TRUE)
  # this adds errors to log. But they no longer appear in console
  sink(log_con, append = TRUE, type = "message")
  return(log_con)
}

# writes a message in the log_file and adds a timestamp
writeLog <- function(message) {
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message_with_time <- paste0(time, ": ", message)
  cat("\n", message_with_time, "\n", sep = "")
}

# writes a header with part(title) and description
writeLogHeader<- function(part, description) {
  cat("\n-------------------\n")
  cat(part, "\n")
  # add breaks after 100 characters
  wrapped <- strwrap(description, width = 100)
  cat(paste(wrapped, collapse = "\n"), "\n")
  cat("-------------------\n")
}

# writes something to the logfine without adding a timestamp.
# it can handle simple messages as well as dataframes
# By default k-anonymity is applied. Note that numbers in a string are not affected.
writeLogData <- function(message = NULL, data = NULL, kanonymity = TRUE) {

  # helperfunction to apply k-Anonymity.
  # If x is numeric and < k it is replaced by < k
  censorValue <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    if (!is.na(num) && num != 0 && num < k_value) {
      paste0("<", k_value)
    } else as.character(x)
  }

  # process data: if data is a dataframe handle every line
  # else just take the data (in each case check for k-Anonymity)
  processData <- function(data) {
    if (is.data.frame(data)) {
      rows <- apply(data, 1, function(row) {
        if (kanonymity) {
          row <- sapply(row, censorValue)
        }
        paste(row, collapse = " ")
      })
      paste(rows, collapse = "\n")
    } else {
      val <- as.character(data)
      if (kanonymity) {
        censorValue(val)
        } else val
    }
  }

  # create output with message and/or data. If only one input is there
  # it can be data or text.
  out <- if (!is.null(message) && !is.null(data)) {
    data_str <- processData(data)
    # if the result has multiple rows ad an additional "\n"
    sep <- if (grepl("\n", data_str, fixed = TRUE)) "\n" else ""
    paste0(as.character(message), sep, data_str)
  } else if (!is.null(data)) {
    processData(data)
  } else if (!is.null(message)) {
    message <- processData(message)
    # add breaks after 100 characters
    wrapped <- strwrap(message, width = 100)
    paste(wrapped, collapse = "\n")
  } else {
    ""
  }

  cat(out, sep = "\n")
}

# give a final success or error message and closes the log and db-connection
stopScript <- function(log_con, success, free_text){
  # final message:
  if(success){
    time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    #message_with_time <- paste0(time, ": ", "Execution completed without errors.")
    #cat("\n", message_with_time, "\n", sep = "")
    writeLog("Execution completed without errors.")
   # cat("Execution completed without errors.")
    cat("\nThank you for participating.")
  }

  if(free_text) {
    cat("\n\n")
    writeLogData("THIS LOG CONTAINS FREE TEXT FROM YOUR DATA")
    writeLogData("Before sending, please check the log (at the end of Part 2) to
                 make sure it contains no sensitive data.")
  }

  #close all connections
  sink(type = "message")
  while (sink.number() > 0) {
    sink()
  }
  close(log_con)
  closeAllConnections()

  # close duckdb-connection and remove temp duckdb-file
  dbDisconnect(con, shutdown = TRUE)
  unlink(file.path(working_dir, "temp.duckdb"))

  # because of sink error messages are only in the log
  if(!success){
    cat("An error occcured. Please check the log for details")
  }
}

######
# functions for table checks
######

checkForDuplicates <- function(table_to_check, id, code = NULL) {

  query <- glue_sql("
        SELECT {`id`}
        FROM {`table_to_check`}
        GROUP BY {`id`}
        HAVING COUNT(*) > 1
      ", .con = con)

  duplicate_check <- dbGetQuery(con, query)

  # If duplicates are found, give a warning.

  if (nrow(duplicate_check) > 0) {
    writeLog(paste0("NOTE: Table ", table_to_check,
                    " has multiple rows per ", id))

    # if a code is defined check if the duplicates remain
    # after grouping by code

    if(!is.null(code)) {
      query <- glue_sql("
        SELECT {`id`}
        FROM {`table_to_check`}
        GROUP BY {`id`}, {`code`}
        HAVING COUNT(*) > 1
      ", .con = con)

      duplicate_check <- dbGetQuery(con, query)

      # If duplicates are found, give a warning.
      # If everything is fine say so

      if (nrow(duplicate_check) > 0) {
        writeLogData(paste0("NOTE: ", table_to_check,
                        " has multiple rows per ", id, " and ", code))
      } else {
        writeLogData(paste0("But ", table_to_check,
                        " has no multiple rows per ", id, " and ", code))

        # if there are multiple codes per id, get all combinations
        query <- glue_sql("
          SELECT code_combo, COUNT(*) as n_ids
          FROM (
            SELECT {`id`},
            string_agg(DISTINCT {`code`}, ', ' ORDER BY {`code`}) AS code_combo
            FROM {`table_to_check`}
            GROUP BY {`id`}
          )
          GROUP BY code_combo
          ORDER BY n_ids DESC
        ", .con = con)

        combo_check <- dbGetQuery(con, query)
        writeLogData("Codes that are together in one id (with counts): "
                     , combo_check)
      }
    }
  }
}

checkReference <- function(table_to_check, joined_table_id) {

  # checks if the reference is working. Works only for LEFT Joins.
  # If a reference is invalid, the main-id of the joined table is NULL (like
  # all columns coming from the joined table)

  query <- glue_sql("
        SELECT count(*) as n
        FROM {`table_to_check`}
        WHERE {`joined_table_id`} IS NULL
      ", .con = con)

  reference_check <- dbGetQuery(con, query)$n

  if (reference_check > 0) {
  writeLogData(paste0("Number of invalid references in ", table_to_check,
                        ": " , reference_check))
  }
}

# check if a column exists and generate a corresponding SQL-expression
getColumnExpr <- function(name_of_file, name_of_column, type_of_column) {

  path <- paste0(data_dir, "/", name_of_file)

  csv_cols <- dbGetQuery(con, glue_sql(
    "DESCRIBE SELECT * FROM read_csv_auto({path}) LIMIT 0",
    .con = con
  ))$column_name

  if (name_of_column %in% csv_cols) {
    expr <- glue_sql("{`name_of_column`}", .con = con)
    return(expr)
  } else {
    writeLogData(paste0("Note: no ", name_of_column, " column in ", name_of_file))
    expr <- glue_sql("CAST(NULL AS {DBI::SQL(type_of_column)})", .con = con)
    return(expr)
  }
}

# builds a type_clause for duckdb to ensure that the columns are typed as
# Varchar. This is helpful to prevent that codes, like snomed codes, are
# typed as numbers.
getVarcharTypeClause <- function(cols) {

  entries <- sprintf(
    "%s: 'VARCHAR'",
    sapply(cols, function(x) DBI::dbQuoteString(con, x))
  )

  type_cast <- DBI::SQL(paste0("types={", paste(entries, collapse = ", "), "}"))

  return(type_cast)
}

######
# function to cut-off data that does not meet the required k-anonymity
######

applyKAnonymity <- function(df, key_var, static_vars = NULL) {

  # get a true/false information for all lines
  lines_to_anonymise <- df[[key_var]] < k_value & df[[key_var]] != 0

  # anonymise everything except the static_vars
  data_vars <- setdiff(names(df), c(key_var, static_vars))

  # write "< k_value" to the Key_var (normally the var with the patient count)
  df[[key_var]][lines_to_anonymise] <- paste0("<", k_value)

  # set all other vars to NA
  df[lines_to_anonymise, data_vars] <- NA

  return (df)
}

# takes a dataframe and suppresses every value in cols that is <5
# Note: that is no real k-anonymization since in many cases values can
# be re-calculated. So use with care
applyKExtra <- function(df, cols) {

  for (col in cols) {
    x <- as.character(df[[col]])
    is_num <- suppressWarnings(!is.na(as.numeric(x)))
    num <- suppressWarnings(as.numeric(x))
    x[is_num & num > 0 & num < k_value] <- paste0("<", k_value)
    df[[col]] <- x
  }
  return (df)
}


