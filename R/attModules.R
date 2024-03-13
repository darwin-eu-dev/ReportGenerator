attritionUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  if (id == "Table - Number of participants") {

    lockName <- "lockTableNumPar"
    analysisChoices <- unique(uploadedFiles$dataIP$incidence_attrition$analysis_id)
    captionText <- table1aAutText(uploadedFiles$dataIP$incidence_attrition, uploadedFiles$dataIP$prevalence_attrition)
    outputTableName <- "previewTableAtt"

  } else if (id == "Table - Incidence Attrition") {

    lockName <- "lockTableIncAtt"
    analysisChoices <- unique(uploadedFiles$dataIP$incidence_attrition$analysis_id)
    captionText <- tableAttrition(uploadedFiles$dataIP$incidence_attrition)
    outputTableName <- "previewTableAttInc"

    } else if (id == "Table - Prevalence Attrition") {

      lockName <- "lockTablePrevAtt"
      analysisChoices <- unique(uploadedFiles$dataIP$prevalence_attrition$analysis_id)
      captionText <- tableAttrition(uploadedFiles$dataIP$prevalence_attrition)
      outputTableName <- "previewTableAttPrev"

    }

  tagList(
    fluidRow(
      column(4,
             selectInput(inputId = ns("analysisIdTable1"),
                         label = "Analysis ID",
                         choices = analysisChoices)
      ),
      column(12,
             createCaptionInput(inputId = ns("captionTableAtt"),
                                value = captionText,
                                height = "80px")
      )
    ),
    fluidRow(
      column(4,
             actionButton(ns(lockName), "Add item to report")
      )
    ),
    tags$br(),
    fluidRow(
      column(12,
             tableOutput(ns(outputTableName))
      )
    )
  )
}


attritionServer <- function(id, uploadedFiles) {
  moduleServer(id, function(input, output, session) {

      prevalenceAttritionCommon <- reactive({
        uploadedFiles <- uploadedFiles()
        if (!is.null(uploadedFiles$dataIP$prevalence_attrition)) {
          commonData <- uploadedFiles$dataIP$prevalence_attrition
          if (inherits(commonData$excluded_records, "character")) {
            commonData$excluded_records <- as.numeric(commonData$excluded_records)
          }
          if (inherits(commonData$excluded_subjects, "character")) {
            commonData$excluded_subjects <- as.numeric(commonData$excluded_subjects)
          }
          commonData <- commonData %>%
            mutate_if(is.numeric, list(~replace_na(., 0))) %>%
            filter(analysis_id %in% c(input$analysisIdTable1))
          commonData
        } else {
          NULL
        }
      })

      # incidence_attrition

      incidenceAttritionCommon <- reactive({
        uploadedFiles <- uploadedFiles()
        if (!is.null(uploadedFiles$dataIP$incidence_attrition)) {
          commonData <- uploadedFiles$dataIP$incidence_attrition
          if (inherits(commonData$excluded_records, "character")) {
            commonData$excluded_records <- as.numeric(commonData$excluded_records)
          }
          if (inherits(commonData$excluded_subjects, "character")) {
            commonData$excluded_subjects <- as.numeric(commonData$excluded_subjects)
          }
          commonData <- commonData %>%
            mutate_if(is.numeric, list(~replace_na(., 0))) %>%
            filter(analysis_id %in% c(input$analysisIdTable1))
          commonData
        } else {
          NULL
        }
      })

      addObject <- reactiveVal()

      if (id == "Table - Number of participants") {

        output$previewTableAtt <- renderTable({
          prevalence_attrition <- prevalenceAttritionCommon()
          incidence_attrition <- incidenceAttritionCommon()
          eval(parse(text = getItemConfig(input = "title",
                                          output = "function",
                                          inputValue = id)))
        }, colnames = FALSE)

        observeEvent(input$lockTableNumPar, {
          addObject(
            list(`Table - Number of participants` = list(prevalence_attrition = prevalenceAttritionCommon(),
                                                         incidence_attrition = incidenceAttritionCommon(),
                                                         caption = input$captionTableAtt))
          )
        })

      } else if (id == "Table - Incidence Attrition") {

        attritionDataType <- "incidence"

        output$previewTableAttInc <- renderTable({
          incidence_attrition <- incidenceAttritionCommon()
          incidence_attrition <- incidenceAttritionCommon()
          eval(parse(text = getItemConfig(input = "title",
                                          output = "function",
                                          inputValue = id)))
        }, colnames = FALSE)

        observeEvent(input$lockTableIncAtt, {
          addObject(
            list(`Table - Incidence Attrition` = list(incidence_attrition = incidenceAttritionCommon(),
                                                      attritionDataType = attritionDataType,
                                                      caption = input$captionTableAtt))
          )
        })

      } else if (id == "Table - Prevalence Attrition") {

        attritionDataType <- "prevalence"

        output$previewTableAttPrev <- renderTable({
            prevalence_attrition <- prevalenceAttritionCommon()
            eval(parse(text = getItemConfig(input = "title",
                                            output = "function",
                                            inputValue = id)))
        }, colnames = FALSE)

        observeEvent(input$lockTablePrevAtt, {
          addObject(
            list(`Table - Prevalence Attrition` = list(prevalence_attrition = prevalenceAttritionCommon(),
                                                       attritionDataType = attritionDataType,
                                                       caption = input$captionTableAtt))
            )
          })
      }

      return(addObject)
  })
}


