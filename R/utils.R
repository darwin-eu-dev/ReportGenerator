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

#' Checks items available for ReportGenerator
#'
#' Get the items that the user can choose from in the report generator. The list is loaded from the configuration file
#' and filtered by the files that have been uploaded.
#'
#' @param items vector of uploaded filenames.
#'
#' @return a dataframe with the properties of the items
getItemsList <- function(items) {
  menuData <- yaml.load_file(system.file("config", "menuConfig.yaml", package = "ReportGenerator"))
  menuList <- lapply(menuData, function(menuData, itemsList) {
    if (all(menuData[["type"]] %in% itemsList)) {
      menuData[["title"]]
    }
  }, itemsList = items)
  result <- list()
  for (i in menuList) {
    if (!is.null(i)) {
      result <- rbind(result, i)
    }
  }
  result <- unlist(result)
  return(result)
}

#' Checks items available for ReportGenerator
#'
#' Get the items that the user can choose from in the report generator. The list is loaded from the configuration file
#' and filtered by the files that have been uploaded.
#'
#' @param input Declare the type of value such as "title" to look up in the yml file.
#' @param output Expected value from the yml file such as the "function" to evaluate.
#' @param inputValue The actual value in character to look up.
#'
#' @return a dataframe with the properties of the items
getItemConfig <- function(input = NULL,
                          output = NULL,
                          inputValue = NULL) {

  checkmate::assertSetEqual(length(input), 1)
  checkmate::assertSetEqual(length(output), 1)
  checkmate::assertSetEqual(length(inputValue), 1)
  checkmate::assertCharacter(input)
  checkmate::assertCharacter(output)
  checkmate::assertCharacter(inputValue)

  menuData <- yaml.load_file(system.file("config", "menuConfig.yaml", package = "ReportGenerator"))
  functionText <- lapply(menuData, function(menuData, title) {
    if (menuData[[input]] == inputValue) {
      menuData[[output]]
    }
  }, title = title)
  result <- list()
  for (i in functionText) {
    if (!is.null(i)) {
      result <- rbind(result, i)
    }
  }
  result <- unlist(result)
  return(result)
}

#' Adds the given type to the current previewItem string.
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemType <- function(previewItemString, previewItemType) {

  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }
  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'cdm_name'"
  } else if (previewItemType == "Facet by database, colour by strata_name"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'strata_name'"
  } else if (previewItemType == "Facet by database"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'outcome_cohort_name'"
  }
  result <- gsub("colour", colour, previewItemString)
  result <- gsub("facet", facet, result)
  return(result)
}

#' Adds the given type to the current previewItem string, by sex
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemTypeSex <- function(previewItemString, previewItemType) {

  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }

  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'denominator_sex'"
  } else if (previewItemType == "Facet by database"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'denominator_sex'"
  }
  result <- gsub("colour", colour, previewItemString)
  result <- gsub("facet", facet, result)
  return(result)
}

#' Adds the given type to the current previewItem string, by sex
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemTypeAge <- function(previewItemString, previewItemType) {

  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }
  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'denominator_age_group'"
  } else if (previewItemType == "Facet by database"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'denominator_age_group'"
  }
  result <- gsub("colour", colour, previewItemString)
  result <- gsub("facet", facet, result)
  return(result)
}

#' Export list of package results
#'
#' @param resultList Named list with results from a darwin package
#' @param zipName name to give zip folder
#' @param outputFolder directory to save zip folder containing results as a set
#' of CSV files
#'
#' @return zip folder of results saved in the outputFolder
#' @export
#'
#' @examples
#' \donttest{
#' exportResults(
#'   resultList = list("mtcars" = mtcars),
#'   zipName = "test",
#'   outputFolder = tempdir()
#' )
#' }
exportResults <- function(resultList,
                          zipName,
                          outputFolder) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertList(resultList, add = errorMessage)
  checkmate::assertNamed(resultList, add = errorMessage)
  checkmate::assertCharacter(zipName, add = errorMessage)
  checkmate::assertDirectoryExists(outputFolder, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  tempDir <- zipName
  tempDirCreated <- FALSE
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
    tempDirCreated <- TRUE
  }

  # write results to disk
  for (i in seq_along(resultList)) {
    workingResult <- resultList[[i]]
    workingName <- names(resultList)[[i]]
    if (is.null(workingName)) {
      workingName <- paste0("result_", i)
    }
    utils::write.csv(workingResult,
                     file = file.path(
                       tempDir,
                       paste0(
                         unique(workingResult$cdm_name), "_",
                         workingName, "_",
                         format(Sys.Date(), format = "%Y_%m_%d"),
                         ".csv"
                       )
                     ),
                     row.names = FALSE
    )
  }
  zip::zip(
    zipfile = file.path(outputFolder, paste0(zipName, ".zip")),
    files = list.files(tempDir, full.names = TRUE)
  )
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }

  invisible(resultList)
}

getRandomId <- function() {
  chars <- c(0:9, letters, LETTERS)
  randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "")
  return(randomId)
}
