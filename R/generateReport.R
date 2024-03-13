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

#' Generate report
#'
#' @param reportDocx the report document
#' @param dataReportList the list of items to be added
#' @param fileName the name of the report file
#'
#' @return NULL, the report is written to given file
generateReport <- function(reportDocx, dataReportList, fileName) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(reportDocx, "rdocx", add = errorMessage)
  checkmate::assertList(dataReportList, add = errorMessage)
  checkmate::assertPathForOutput(fileName, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (length(dataReportList) > 0) {
    # Loop through ever object selected in the menu
    for (i in seq(1:length(dataReportList))) {
      # Get the function to generate and print in report
      titleText <- names(dataReportList[[i]])
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = titleText)
      # Get relevant options for the function
      itemOptions <- getItemConfig(input = "title",
                                   output = "options",
                                   inputValue = titleText)

      # Additional parameter if there are options for the graphs
      if (!is.null(itemOptions)) {
        if (grepl("by sex", titleText)) {
          expression <- expression %>%
            addPreviewItemTypeSex(dataReportList[[i]][[1]][["plotOption"]])
        } else if (grepl("by age", titleText)) {
          expression <- expression %>%
            addPreviewItemTypeAge(dataReportList[[i]][[1]][["plotOption"]])
        } else  {
          expression <- expression %>%
            addPreviewItemType(dataReportList[[i]][[1]][["plotOption"]])
        }
        if (grepl("Ribbon", itemOptions)) {
          expression <- expression %>%
            addPreviewItemRibbon(dataReportList[[i]][[1]][["ribbon"]])
        }
        if (grepl("Options", itemOptions)) {
          expression <- do.call(addPlotOptions, append(list(expression),
                                                       as.list(dataReportList[[i]][[1]][["options"]])))
        }
      }

      # Evaluate function
      object <- eval(parse(text = expression), envir = dataReportList[[i]][[1]])

      # Check class of every function and add it to the word report accordingly
      if ("gt_tbl" %in% class(object)) {
        body_end_section_landscape(reportDocx)
        body_add_gt(reportDocx, value = object)
        body_add(reportDocx,
                 value = dataReportList[[i]][[1]][["caption"]])
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
        body_end_section_portrait(reportDocx)

      } else if ("ggplot" %in% class(object)) {
        body_end_section_landscape(reportDocx)
        plotDim <- getGGPlotDimensions()
        body_add_gg(x = reportDocx,
                    value = object,
                    width = plotDim[["width"]],
                    height = plotDim[["height"]],
                    style = "Normal")
        body_add(reportDocx,
                 value = dataReportList[[i]][[1]][["caption"]])
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
        body_end_section_portrait(reportDocx)

      } else if ("huxtable" %in% class(object)) {
        body_end_section_landscape(reportDocx)
        body_add_table(reportDocx,
                       value = object,
                       style = "Table Paragraph",
                       header = FALSE)
        body_add_par(reportDocx, " ")
        body_add(reportDocx,
                 value = dataReportList[[i]][[1]][["caption"]])
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
        body_end_section_portrait(reportDocx)
      }

      if (titleText == "Sunburst Plot - TreatmentPatterns") {
        body_add_img(x = reportDocx,
                     src = dataReportList[[i]][[1]][["fileImage"]],
                     height = 5.5,
                     width = 7)
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")

      }  else if (titleText == "Sankey Diagram - TreatmentPatterns") {
        body_add_img(x = reportDocx,
                     src = dataReportList[[i]][[1]][["fileImage"]],
                     height = 3,
                     width = 7)
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
      }
    }
  } else {
    message("No items added to the report")
  }
  body_add_toc(reportDocx)
  print(reportDocx,
        target = fileName)
}

getGGPlotDimensions <- function() {
  return(list("width" = 10.5,
              "height" = 4.75))
}
