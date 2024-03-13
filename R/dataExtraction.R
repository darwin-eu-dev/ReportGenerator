# Copyright 2023 DARWIN EU®
#
# This file is part of ReportGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' `joinDatabase()` joins several zip or csv folders into a list of dataframes.
#'
#' @param fileDataPath File path(s) in character
#' @param fileName Name of the file in character to process in case the input is only csv
#' @param csvLocation Path folder location to uncompress the zip files
#'
#' @return A list of dataframes
#'
#' @import yaml
#' @export
joinDatabase <- function(fileDataPath = NULL,
                         fileName = NULL,
                         csvLocation = NULL) {

  # Loading yml file
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  packagesNames <- names(configData)

  # Check zip
  if (grepl(".zip", fileDataPath[1], fixed = TRUE)) {
    # Empty list to allocate data
    data <- list()
    # Folder count to allocate multiple databases
    folderNumber <- 0
    # Unzips every zip and puts the files in a separate folder in the temp dir
    for (fileLocation in fileDataPath) {
      folderNumber <- folderNumber + 1
      unzip(zipfile = fileLocation,
            exdir = file.path(csvLocation, paste0("database", as.character(folderNumber))))
    }
    # List of unzipped database directories where files are located
    databaseFolders <- dir(csvLocation, pattern = "database", full.names = TRUE)
    # Iterates through each database folder to list the files inside
    for (filesList in databaseFolders) {
      filesLocation <- list.files(filesList,
                                  pattern = ".csv",
                                  full.names = TRUE,
                                  recursive = TRUE)
      # Iterates every individual file
      for (file in filesLocation) {
        resultsData <- read_csv(file, show_col_types = FALSE)
        resultsColumns <- names(resultsData)
        if (grepl("metadata.csv", file)) {
          metadata <- read_csv(file = file,
                               show_col_types = FALSE)
          databaseName <- metadata$cdmSourceName
        }
        # Checks the type of every individual file
        data <- loadFileData(data, file, configData, resultsData, resultsColumns, databaseName)
      }
    }
  } else if (grepl(".csv", fileDataPath[1], fixed = TRUE)) {
    data <- list()
    for (i in seq(1:length(fileDataPath))) {
      resultsData <- read_csv(fileDataPath[i], show_col_types = FALSE)
      resultsColumns <- names(resultsData)
      data <- loadFileData(data, fileName[i], configData, resultsData, resultsColumns, databaseName = NULL)
    }
  }
  return(data)
}

#' Load file data and save it in a list.
#'
#' @param data named list with data.
#' @param fileName the file to load
#' @param configData config loaded from file
#' @param resultsData the loaded file data
#' @param resultsColumns the loaded file columns
#' @param databaseName db name
#'
#' @return the list with data
loadFileData <- function(data, fileName, configData, resultsData, resultsColumns, databaseName) {
  resultType <- NULL

  if (nrow(resultsData) > 0 && !endsWith(fileName, "dbinfo.csv") && !endsWith(fileName, "metadata.csv")) {
    if ("result_type" %in% resultsColumns) {
      resultType <- unique(resultsData$result_type)
      if (resultType == "Summary characteristics") {
        resultsData$result_type <- "summarised_characteristics"
        resultType <- "summarised_characteristics"
      }
    }
    for (pkg in names(configData)) {
      pkgConfigData <- configData[[pkg]]

      # if possible use resultType
      if (!is.null(resultType)) {
        if (resultType == "Survival estimate") {
          analysis_type <- unique(resultsData$analysis_type)
          if (analysis_type == "Competing risk") {
            resultType <- "Survival cumulative incidence"
          }
        }
        if (resultType %in% names(pkgConfigData)) {
          configColumns <- pkgConfigData[[resultType]]
          configColumns <- unlist(configColumns$names)
          if (all(configColumns %in% resultsColumns)) {
            message(paste0(resultType, ": match (using resultType)"))
            data[[pkg]][[resultType]] <- bind_rows(data[[pkg]][[resultType]], resultsData)
          }
        }
      } else {
        for (val in names(pkgConfigData)) {
          configColumns <- pkgConfigData[[val]]
          configColumns <- unlist(configColumns$names)
          if (val == "incidence_attrition") {
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "prevalence_attrition") {
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "incidence_estimates") {
            if (all(configColumns %in% resultsColumns)) {
              if ("denominator_days_prior_history" %in% resultsColumns) {
                colnames(resultsData)[colnames(resultsData) == "denominator_days_prior_history"] <- "denominator_days_prior_observation"
              }
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "prevalence_estimates") {
            if (all(configColumns %in% resultsColumns)) {
              if ("denominator_days_prior_history" %in% resultsColumns) {
                colnames(resultsData)[colnames(resultsData) == "denominator_days_prior_history"] <- "denominator_days_prior_observation"
              }
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "treatmentPathways") {
            if (all(configColumns %in% resultsColumns)) {
              if (!('cdm_name' %in% resultsColumns)) {
                resultsData <- mutate(resultsData,
                                      cdm_name = databaseName)
              }
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "Summarised Characteristics") {
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "Summarised Large Scale Characteristics") {
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "Survival estimate") {
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "Survival cumulative incidence") {
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match"))
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          }
        }
      }
    }
  }
  return(data)
}

#' Writes variablesConfig file in Yaml
#'
#' `variablesConfigYaml()` the user can load the column names to the variablesConfig.yaml so ReportGenerator can recognize its data.
#'
#' @param fileDataPath A zip folder with csv results from an analytical packages, e.g. IncidencePrevalence, PatientProfiles, TreatmentPatterns, etc.
#' @param package A string to identify the name of the package, default IncidencePrevalence.
#' @param version A string to identify which version of IncidencePrevalence is used to generate the results.
#'
#' @return Adds column names to the variablesConfig.yaml
#' @export
#' @import dplyr
#' @importFrom utils unzip
#' @importFrom readr read_csv
variablesConfigYaml <- function(fileDataPath = NULL,
                                package = "IncidencePrevalence",
                                version = NULL) {
  # fileDataPath <- fileDataPath[1]

  if (package == "IncidencePrevalence") {
    csvLocation <- file.path(tempdir(), "dataLocation")
    dir.create(csvLocation)
    utils::unzip(zipfile = fileDataPath,
                 exdir = csvLocation)
    csvFiles <- list.files(path = csvLocation,
                           pattern = ".csv",
                           full.names = TRUE,
                           recursive = TRUE)

    incidenceEstimatesPath <- csvFiles[stringr::str_detect(csvFiles, "incidence_estimates")]
    incidenceEstimates <- read_csv(incidenceEstimatesPath, show_col_types = FALSE)
    columnNamesIncidence <- names(incidenceEstimates)[grepl("incidence", names(incidenceEstimates))]
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["incidence_estimates"]][["names"]] <- columnNamesIncidence
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    prevalenceEstimatesPath <- csvFiles[stringr::str_detect(csvFiles, "prevalence_estimates")]
    prevalenceEstimates <- read_csv(prevalenceEstimatesPath, show_col_types = FALSE)
    columnNamesPrevalence <- names(prevalenceEstimates)[grepl("prevalence", names(prevalenceEstimates))]
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["prevalence_estimates"]][["names"]] <- columnNamesPrevalence
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    incidenceAttritionPath <- csvFiles[stringr::str_detect(csvFiles, "incidence_attrition")]
    incidenceAttrition <- read_csv(incidenceAttritionPath, show_col_types = FALSE)
    columnNamesIncidenceAttrition <- setdiff(names(incidenceAttrition), names(incidenceEstimates))
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["incidence_attrition"]][["names"]] <- columnNamesIncidenceAttrition
    configData[[package]][[version]][["prevalence_attrition"]][["names"]] <- columnNamesIncidenceAttrition
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    unlink(csvLocation, recursive = TRUE)

  } else if (package == "TreatmentPatterns") {
    csvFiles <- fileDataPath
    treatmentPathwaysPath <- csvFiles[stringr::str_detect(csvFiles, "treatmentPathways")]
    treatmentPathways <- read_csv(treatmentPathwaysPath, show_col_types = FALSE)
    columnNamesTreatmentPathways <- names(treatmentPathways)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["treatmentPathways"]][["names"]] <- columnNamesTreatmentPathways
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

  } else if (package == "PatientProfiles") {
    if (is.null(version)) {
      version <- as.character(packageVersion(package))
    }
    checkmate::expect_character(version)

    cdm <- PatientProfiles::mockPatientProfiles(patient_size = 10)
    summaryPP <- PatientProfiles::summariseCharacteristics(cohort = cdm$cohort1, cdm = cdm)
    columnNamesCharacteristics <- names(summaryPP)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][[unique(summaryPP$result_type)]][["names"]] <- columnNamesCharacteristics
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    cdm[["cohort1"]] <- cdm[["cohort1"]]%>%
      PatientProfiles::addDemographics()

    lscPP <- PatientProfiles::summariseLargeScaleCharacteristics(cohort = cdm[["cohort1"]],
                                                                 strata = list("age",
                                                                               "sex"),
                                                                 window = list(c(-30, -1),
                                                                               c(0, 0),
                                                                               c(1, 30),
                                                                               c(31, 365)),
                                                                 eventInWindow = "condition_occurrence",
                                                                 episodeInWindow = "condition_occurrence",
                                                                 cdm = attr(cdm[["cohort1"]], "cdm_reference"))
    lscPP <- read.csv(lsc_csv)
    columnNamesLSC <- names(lscPP)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][[unique(lscPP$result_type)]][["names"]] <- columnNamesLSC
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

  } else if (package == "CohortSurvival") {
    if (is.null(version)) {
      version <- as.character(packageVersion(package))
    }
    cdm <- CohortSurvival::mockMGUS2cdm()
    MGUS_death <- CohortSurvival::estimateSingleEventSurvival(cdm,
                                                              targetCohortTable = "mgus_diagnosis",
                                                              outcomeCohortTable = "death_cohort"
    )
    columnSurvival <- names(MGUS_death)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][[unique(MGUS_death$result_type)]][["names"]] <- columnSurvival
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  }
}

dataCleanAttrition <- function(incidence_attrition = NULL,
                               prevalence_attrition = NULL) {
  if (!is.null(prevalence_attrition)) {
    prevalence_attrition$reason <- gsub("Prior history requirement not fullfilled during study period",
                                        "Prior history requirement not fulfilled during study period ",
                                        prevalence_attrition$reason)
    if (!("reason_id" %in% names(prevalence_attrition))) {
      prevalence_attrition <- prevalence_attrition %>%
        mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                     reason == "Missing year of birth"  ~ 2,
                                     reason == "Missing sex"  ~ 3,
                                     reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                     reason == "No observation time available during study period"  ~ 5,
                                     reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                     reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                     reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                     reason == "Not Female"  ~ 9,
                                     reason == "Not Male"  ~ 10,
                                     reason == "Starting analysis population" ~ 11,
                                     reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                     reason == "Not observed during the complete database interval"  ~ 14,
                                     reason == "Do not satisfy full contribution requirement for an interval"  ~ 16),
               number_subjects = current_n,
               excluded_subjects = excluded)
    } else {
      prevalence_attrition <- prevalence_attrition %>%
        mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                     reason == "Missing year of birth"  ~ 2,
                                     reason == "Missing sex"  ~ 3,
                                     reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                     reason == "No observation time available during study period"  ~ 5,
                                     reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                     reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                     reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                     reason == "Not Female"  ~ 9,
                                     reason == "Not Male"  ~ 10,
                                     reason == "Starting analysis population" ~ 11,
                                     reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                     reason == "Not observed during the complete database interval"  ~ 14,
                                     reason == "Do not satisfy full contribution requirement for an interval"  ~ 16))
    }
    return(prevalence_attrition)
  } else if (!is.null(incidence_attrition)) {
    incidence_attrition$reason <- gsub("Prior history requirement not fullfilled during study period",
                                       "Prior history requirement not fulfilled during study period ",
                                       incidence_attrition$reason)
    if (!("reason_id" %in% names(incidence_attrition))) {
      incidence_attrition <- incidence_attrition %>%
        mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                     reason == "Missing year of birth"  ~ 2,
                                     reason == "Missing sex"  ~ 3,
                                     reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                     reason == "No observation time available during study period"  ~ 5,
                                     reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                     reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                     reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                     reason == "Not Female"  ~ 9,
                                     reason == "Not Male"  ~ 10,
                                     reason == "Starting analysis population" ~ 11,
                                     reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                     reason == "Not observed during the complete database interval"  ~ 14,
                                     reason == "Do not satisfy full contribution requirement for an interval"  ~ 16),
               number_subjects = current_n,
               excluded_subjects = excluded)
    } else {
      incidence_attrition <- incidence_attrition %>%
        mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                     reason == "Missing year of birth"  ~ 2,
                                     reason == "Missing sex"  ~ 3,
                                     reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                     reason == "No observation time available during study period"  ~ 5,
                                     reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                     reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                     reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                     reason == "Not Female"  ~ 9,
                                     reason == "Not Male"  ~ 10,
                                     reason == "Starting analysis population" ~ 11,
                                     reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                     reason == "Not observed during the complete database interval"  ~ 14,
                                     reason == "Do not satisfy full contribution requirement for an interval"  ~ 16))
    }
    return(incidence_attrition)
  }
}

#' `testData()` extracts data from zip results and saves it in .rda format
#' @param testFilesIP testthat dir location of the test files with version number of folder (internal use).
#' @param testFilesTP testthat dir of treatment pattern files
#' @param testFilesPP testthat dir of patient profiles files
#' @param testFilesCS testthat dir of cohort survival files
#'
#' @importFrom usethis use_data
#'
#' @return sysdata.rda instruction
testData <- function(testFilesIP = testthat::test_path("IP", "0.6.0", "zip"),
                     testFilesTP = testthat::test_path("TP", "2.5.2", "csv", "CHUBX"),
                     testFilesPP = testthat::test_path("PP", "0.5.1", "zip"),
                     testFilesCS = testthat::test_path("CS", "0.2.5", "zip")) {
  checkmate::assertDirectoryExists(testFilesIP)
  checkmate::assertDirectoryExists(testFilesTP)
  checkmate::assertDirectoryExists(testFilesPP)
  checkmate::assertDirectoryExists(testFilesCS)
  uploadedFilesIP <- list.files(testFilesIP,
                                pattern = ".zip",
                                full.names = TRUE,
                                recursive = TRUE)
  treatmentPathways_test <- read_csv(file.path(testFilesTP, "treatmentPathways.csv"),
                                     show_col_types = FALSE)
  uploadedFilesPP <- list.files(testFilesPP,
                                pattern = ".zip",
                                full.names = TRUE,
                                recursive = TRUE)
  uploadedFilesCS <- list.files(testFilesCS,
                                pattern = ".zip",
                                full.names = TRUE,
                                recursive = TRUE)
  checkmate::assertFileExists(uploadedFilesIP)
  checkmate::assertFileExists(uploadedFilesPP[1])
  checkmate::assertFileExists(uploadedFilesCS[1])
  checkmate::assertTibble(treatmentPathways_test)
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)

  # Extract
  testData <- joinDatabase(fileDataPath  = uploadedFilesIP,
                           csvLocation = csvLocation)$IncidencePrevalence
  testData[["treatmentPathways_test"]] <- treatmentPathways_test
  testDataPP <- joinDatabase(fileDataPath  = uploadedFilesPP,
                             csvLocation = csvLocation)$PatientProfiles
  testDataCS <- joinDatabase(fileDataPath  = uploadedFilesCS,
                             csvLocation = csvLocation)$CohortSurvival

  testData <- c(testData, testDataPP, testDataCS)
  unlink(csvLocation, recursive = TRUE)
  return(testData)
}
