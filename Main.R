library(DBI)
library(duckdb)
library(glue)
library(dplyr)
library(purrr)
library(lubridate)
library(tibble)
library(biglm)


source("./config.R")
source("./R/Kalium_value_description.R")
source("./R/Helper_functions.R")
source("./R/Evaluate_sample_quality.R")
source("./R/Prepare_kalium.R")
source("./R/Evaluate_covariates.R")
source("./R/Loading.R")
source("./R/Statistic.R")

version <- "0.8"

startKalium <- function() {

  # get docker or local data dir and write it to env
  writeDataDirsToEnv()

  # marker to know if everything finished correctly
  success <- FALSE

  # marker if there is potential sensitive freetext data in the log
  free_text <- FALSE

  # create output_dir with timestamp
  output_dir <- createOutputDir(main_output_dir)

  # init the log
  log_con <- createLog(output_dir)
  on.exit(stopScript(log_con, success, free_text))
  writeLog("Start Kalium360")
  writeLogData("Script version: ", version, kanonymity = FALSE)
  writeLogData("Used CRTDL: ", used_CRTDL, kanonymity = FALSE)
  writeLogData("Site pseudonym: ", site_pseudonym, kanonymity = FALSE)
  writeLogData("\n")
  writeLogData("The project evaluates how well clinically relevant
        aspects of laboratory values are represented in FHIR data. This includes
        not only the laboratory values themselves, but also quality-determining
        aspects (e.g., whether the sample was hemolyzed) as well as procedures
        (e.g., dialysis) or medications that influence potassium values.")

  # unlinking (deleting) potential old files
  unlink(file.path(working_dir, "temp.duckdb.wal"))
  unlink(file.path(working_dir, "temp.duckdb"))

  # create a duckdb and establish a connection
  con <<- dbConnect(duckdb(), dbdir = file.path(working_dir, "temp.duckdb"))

  tempdir <- paste0(working_dir, "/temp_Duckdb")
  DBI::dbExecute(con, glue_sql("SET temp_directory = {tempdir};", .con = con))


  # disable DuckDB progress-bar. This clutters the log.
  DBI::dbExecute(con, "PRAGMA disable_progress_bar;")

  # create a base table that is used by all parts
  # and check input files
  prepareKalium()

  # calculate the Potassium-value-statistics
  calculatePotassiumStatistics()

  # check if sample-quality information is available. If yes, is it
  # linkable to potassium-lab-results
  free_text <- evaluateSampleQuality()

  # evaluate all the Covariates that can influence potassium values
  evaluateCovariates()

  success <- TRUE
}

writeDataDirsToEnv <- function() {

  # if running in docker use the docker dir, if there is no docker dir use the path
  # in the .env. If there is no path in .env try to get if from config.
  # If still no path, stop with error.

  if (file.exists(".env")) {
    readRenviron(".env")
  }

  resolvePath <- function(config_default, container_var, host_var) {
    if (Sys.getenv(container_var) != "") {
      return(Sys.getenv(container_var))
    }
    if (Sys.getenv(host_var) != "") {
      return(Sys.getenv(host_var))
    }

    tryCatch(
      config_default,
      error = function(e) stop("Missing path info", call. = FALSE)
    )
  }

  data_dir        <<- resolvePath(data_dir, "KALIUM_DATA_DIR", "KALIUM_DATA_PATH")
  main_output_dir <<- resolvePath(main_output_dir, "KALIUM_OUTPUT_DIR", "KALIUM_OUTPUT_PATH")
  working_dir     <<- resolvePath(working_dir, "KALIUM_WORKING_DIR", "KALIUM_WORKING_PATH")

}


startKalium()

