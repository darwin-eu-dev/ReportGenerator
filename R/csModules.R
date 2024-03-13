# CohortSurivial
cohortSurvivalUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  outResult <- NULL
  topN <- NULL
  dlPlot <- NULL
  if (id == "survivalTable") {
    outResult <- DT::dataTableOutput(ns("cs_data"))
    dataset <- uploadedFiles$dataCS$`Survival estimate`
  } else if (id == "survivalPlot") {
    outResult <- plotOutput(ns("cs_plot"))
    dataset <- uploadedFiles$dataCS$`Survival estimate`
  } else if (id == "failureTable") {
    outResult <- DT::dataTableOutput(ns("cu_inc_data"))
    dataset <- uploadedFiles$dataCS$`Survival cumulative incidence`
  } else if (id == "failurePlot") {
    outResult <- plotOutput(ns("cu_inc_plot"))
    dataset <- uploadedFiles$dataCS$`Survival cumulative incidence`
  }
  if (grepl("Plot", id)) {
    dlPlot <- createDownloadPlotUI(ns)
  } else {
    topN <- column(2, numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100))
  }
  captionUI <- fluidRow(
    column(12,
           uiOutput(outputId = ns("caption"))
    ),
  )

  groupLevelOptions <- unique(dataset$group_level)
  strataNameOptions <- unique(dataset$strata_name)
  pickerOptions <- list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
  cdmOptions <- unique(dataset$cdm_name)

  tagList(
    fluidRow(
      column(4,
             pickerInput(
               inputId = ns("cdm_name"),
               label = "Database",
               choices = cdmOptions,
               selected = cdmOptions,
               options = pickerOptions,
               multiple = TRUE
             )
      ),
      column(4,
             pickerInput(
              inputId = ns("group_level"),
              label = "Group Level",
              choices = groupLevelOptions,
              selected = groupLevelOptions[1],
              options = pickerOptions,
              multiple = TRUE
             )
      ),
      column(4,
             pickerInput(
               inputId = ns("strata_name"),
               label = "Strata Name",
               choices = strataNameOptions,
               selected = strataNameOptions[1],
               options = pickerOptions,
               multiple = TRUE
             )
      )
    ),
    captionUI,
    fluidRow(createAddItemToReportUI(ns(paste0("lock", id))),
             topN,
             dlPlot),
    tags$br(),
    fluidRow(column(12, outResult))
  )
}

cohortSurvivalServer <- function(id, uploadedFiles) {

  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      uploadedFiles <- uploadedFiles()
      if (id == "survivalTable"  || id == "survivalPlot") {
        dataset <- uploadedFiles$dataCS$`Survival estimate`
      } else if (id == "failureTable"  || id == "failurePlot") {
        dataset <- uploadedFiles$dataCS$`Survival cumulative incidence`
      }
      dataset %>%
        filter(cdm_name %in% input$cdm_name,
               group_level %in% input$group_level,
               strata_name %in% input$strata_name)
    })

    getTableData <- reactive({
      getData() %>%
        dplyr::slice_head(n = input$top_n) %>%
        select(c("cdm_name", "result_type", "group_level", "strata_name",
                 "strata_level", "variable_type", "time", "estimate"))
    })

    if (id == "survivalTable") {
      output$cs_data <- DT::renderDataTable(server = FALSE, {
        createDataTable(getTableData())
      })
    } else if (id == "survivalPlot") {
      output$cs_plot <- renderPlot({
        previewFigure()
      })
    } else if (id == "failureTable") {
      output$cu_inc_data <- DT::renderDataTable(server = FALSE, {
        createDataTable(getTableData())
      })
    } else if (id == "failurePlot") {
      output$cu_inc_plot <- renderPlot({
        previewFigure()
      })
    }

    previewFigure <- reactive({
      plot <- NULL
      if (nrow(getData()) > 0) {
        if (id == "survivalPlot") {
          plot <- CohortSurvival::plotSurvival(getData(),
                                               facet = "cdm_name",
                                               colour = "strata_name")
        } else if (id == "failurePlot") {
            plot <- CohortSurvival::plotCumulativeIncidence(getData(),
                                                            facet = "cdm_name",
                                                            colour = "strata_name")
          }
      }
      return(plot)
    })

    output$downloadFigure <- downloadHandler(
      filename = function() {
        paste(id, ".png", sep = "")
      },
      content = function(file) {
        saveGGPlot(file = file,
                   plot = previewFigure(),
                   height = as.numeric(input$plotHeight),
                   width = as.numeric(input$plotWidth),
                   dpi = as.numeric(input$plotDpi))
      }
    )

    output$caption <- renderUI({
      captionId <- captionText <- NULL
      if (id == "survivalTable") {
        captionText <- "Table 1. Survival Probability"
        captionId <-  "captionSurvivalEstimateData"
      } else if (id == "survivalPlot") {
        captionText <- "Figure 1. Survival Probability"
        captionId <-  "captionSurvivalEstimate"
      } else if (id == "failureTable") {
        captionText <- "Table 1. Cumulative Survival Probability"
        captionId <-  "captionCumulativeIncidenceData"
      } else if (id == "failurePlot") {
        captionText <- "Figure 1. Cumulative Survival Probability"
        captionId <-  "captionCumulativeIncidence"
      }
      captionText <- glue::glue("{captionText} in {paste0(input$cdm_name, collapse = ',')} (group_level: {paste0(input$group_level, collapse = ',')}; strata: {paste0(input$strata_name, collapse = ',')})")
      createCaptionInput(inputId = ns(captionId),
                         value = captionText)
    })

    addObject <- reactiveVal()
    observeEvent(input$locksurvivalTable, {
      if (nrow(getData()) > 0) {
        addObject(
          list(`Survival table` = list(survivalEstimate = getTableData(),
                                       caption = input$captionSurvivalEstimateData))
        )
      }
    })

    observeEvent(input$locksurvivalPlot, {
      if (nrow(getData()) > 0) {
        addObject(
          list(`Survival plot` = list(survivalEstimate = getData(),
                                      plotOption = "Facet by database, colour by strata_name",
                                      caption = input$captionSurvivalEstimate))
        )
      }
    })

    observeEvent(input$lockfailureTable, {
      if (nrow(getData()) > 0) {
        addObject(
          list(`Cumulative incidence table` = list(cumulativeSurvivalEstimate = getTableData(),
                                                   caption = input$captionCumulativeIncidenceData))
        )
      }
    })

    observeEvent(input$lockfailurePlot, {
      if (nrow(getData()) > 0) {
        addObject(
          list(`Cumulative incidence plot` = list(cumulativeSurvivalEstimate = getData(),
                                                  plotOption = "Facet by database, colour by strata_name",
                                                  caption = input$captionCumulativeIncidence))
        )
      }
    })

    return(addObject)
  })
}
