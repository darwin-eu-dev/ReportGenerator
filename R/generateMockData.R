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

#' Creates a mock data set for ReportGenerator
#'
#' `generateMockData()` uses [IncidencePrevalence::mockIncidencePrevalenceRef()] function to create csv files to test ReportGenerator.
#'
#' @param databaseName A vector with the name in characters of each database.
#' @param simulatePopulation TRUE or FALSE to simulate different population sizes. TRUE is default.
#' @param outputDir A character vector of the directory to export mock data.
#' @import dplyr tidyr IncidencePrevalence duckdb
#' @importFrom utils head write.csv packageVersion
#' @importFrom stats time
#' @importFrom zip zip
#' @return csv files
#' @export
generateMockData <- function(databaseName = c("CHUBX",
                                              "CPRD GOLD",
                                              "IMASIS",
                                              "IPCI",
                                              "SIDIAP"),
                             simulatePopulation = TRUE,
                             outputDir = getwd()) {

  for (i in databaseName) {

    if (simulatePopulation == TRUE) {

        if (i == "CHUBX") {

          sampleSize <- 2152385

        } else if (i == "CPRD GOLD") {

          sampleSize <- 15662217

        } else if (i == "IMASIS") {

          sampleSize <- 1014735

        } else if (i == "IPCI") {

          sampleSize <- 2674547

        } else if (i == "SIDIAP") {

          sampleSize <- 8265343

        }

      } else {

        sampleSize <- 50000

        }

    cdm <- IncidencePrevalence::mockIncidencePrevalenceRef(
      sampleSize = sampleSize,
      outPre = 0.5
      )

    # Denominator data
    cdm <- IncidencePrevalence::generateDenominatorCohortSet(cdm = cdm,
                                                             name = "denominator",
                                                             cohortDateRange = c(as.Date("2008-01-01"),
                                                                                 as.Date("2012-01-01")),
                                                             ageGroup  = list(c(18, 39),
                                                                              c(40, 59),
                                                                              c(18, 99)),
                                                             sex  = c("Female", "Male", "Both"),
                                                             daysPriorObservation = 365)

    # %>% mutate(cdm_name = i)

    # Incidence data
    incidence_estimates <- IncidencePrevalence::estimateIncidence(cdm = cdm,
                                                                  denominatorTable = "denominator",
                                                                  outcomeTable = "outcome",
                                                                  interval = c("years", "overall"),
                                                                  completeDatabaseIntervals = TRUE,
                                                                  outcomeWashout = 180,
                                                                  repeatedEvents = FALSE,
                                                                  minCellCount = 5,
                                                                  temporary = TRUE,
                                                                  returnParticipants = FALSE)

    incidence_attrition <- IncidencePrevalence::incidenceAttrition(incidence_estimates)

    # Prevalence data, both point and period
    prevalencePoint <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,
                                                                    denominatorTable = "denominator",
                                                                    outcomeTable = "outcome",
                                                                    interval = "years",
                                                                    timePoint = "start")

    prevalence_point_attrition <- prevalenceAttrition(prevalencePoint)

    prevalencePeriod <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,
                                                                      denominatorTable = "denominator",
                                                                      outcomeTable = "outcome")

    prevalence_period_attrition <- prevalenceAttrition(prevalencePeriod)
    prevalence_estimates <- rbind(prevalencePoint, prevalencePeriod)
    prevalence_attrition <- rbind(prevalence_point_attrition, prevalence_period_attrition)

    # Add database label
    incidence_estimates <- incidence_estimates %>% mutate(cdm_name = i)
    incidence_attrition <- incidence_attrition %>% mutate(cdm_name = i)
    prevalence_estimates <- prevalence_estimates %>% mutate(cdm_name = i)
    prevalence_attrition <- prevalence_attrition %>% mutate(cdm_name = i)

    class(incidence_attrition) <- c("IncidencePrevalenceResult",
                                    "IncidenceResult",
                                    "tbl_df",
                                    "tbl",
                                    "data.frame")

    class(prevalence_attrition) <- c("IncidencePrevalenceResult",
                                    "PrevalenceResult",
                                    "tbl_df",
                                    "tbl",
                                    "data.frame")

    # Results
    incPreVersion <- packageVersion("IncidencePrevalence")
    outputDirExp <- outputDir
    outputDirExp <- file.path(outputDir, "results", incPreVersion)
    if (!dir.exists(outputDirExp)) {
      dir.create(outputDirExp, recursive = TRUE)
    }

    IncidencePrevalence::exportIncidencePrevalenceResults(resultList = list("incidence_estimates" = incidence_estimates,
                                                                            "prevalence_estimates" = prevalence_estimates,
                                                                            "incidence_attrition" = incidence_attrition,
                                                                            "prevalence_attrition" = incidence_attrition),
                                                                            zipName = paste0("mock_data_ReportGenerator_", i),
                                                                            outputFolder = paste0(outputDirExp))

  }
  duckdb::duckdb_shutdown(duckdb::duckdb())
}
