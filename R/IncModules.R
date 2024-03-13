incidenceUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  if (id == "Plot - Incidence rate per year") {

    databaseChoices <- unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)
    databaseSelected <- databaseChoices

    sexChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_sex)
    sexSelected <-  sexChoices[1]
    sexMultiple <- FALSE

    ageChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group)
    ageSelected <- ageChoices[1]
    ageMultiple <- FALSE

    captionValue <-"Figure 1. Incidence rate/s of drug/s use over calendar time (per year) overall by database [Add months if relevant]"

    lockName <- "lockDataIncidenceYear"
  } else if (id == "Plot - Incidence rate per year by sex") {

    databaseChoices <- unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)
    databaseSelected <- databaseChoices[1]

    sexChoices <- c("Male", "Female")
    sexSelected <- sexChoices
    sexMultiple <- TRUE

    ageChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group)
    ageSelected <- ageChoices[1]
    ageMultiple <- FALSE

    captionValue <- "Figure 2. Incidence rate/s of drug/s use over calendar time (per year) stratified by sex and database [Add months if relevant]"

    lockName <- "lockDataIncidenceSex"
  } else if (id == "Plot - Incidence rate per year by age") {

    databaseChoices <- unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)
    databaseSelected <- databaseChoices[1]

    sexChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_sex)
    sexSelected <-  sexChoices[1]
    sexMultiple <- FALSE

    ageChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group)
    ageSelected <- ageChoices
    ageMultiple <- TRUE

    captionValue <- "Figure 3. Incidence rate/s of drug/s use over calendar time (per year) overall stratified by age and database [Add months if relevant]"

    lockName <- "lockDataIncidenceAge"
  }

  pickerOptions <- list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
  startDateChoices <- as.character(unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date))
  washoutChoices <- unique(uploadedFiles$dataIP$incidence_estimates$analysis_outcome_washout)
  washoutSelected <- washoutChoices[1]
  daysPriorHistoryChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_days_prior_observation)
  daysPriorHistorySelected <- daysPriorHistoryChoices[1]
  outcomeChoices <- unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name)
  dbPickerOptions <- list()
  if (length(databaseChoices) > 1) {
    dbPickerOptions <- pickerOptions
  }
  outcomePickerOptions <- list()
  if (length(outcomeChoices) > 1) {
    outcomePickerOptions <- pickerOptions
  }
  sexPickerOptions <- list()
  if (sexMultiple) {
    sexPickerOptions <- pickerOptions
  }
  agePickerOptions <- list()
  if (ageMultiple) {
    agePickerOptions <- pickerOptions
  }

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("facetIncidence"),
                         label = "Select plot type",
                         choices = c("Facet by outcome", "Facet by database"),
                         selected = "Facet by outcome",
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("databaseIncidence"),
                         label = "Database",
                         choices = databaseChoices,
                         selected = databaseSelected,
                         multiple = TRUE,
                         options = dbPickerOptions)
      ),
      column(4,
             pickerInput(inputId = ns("outcomeIncidence"),
                         label = "Outcome",
                         choices = outcomeChoices,
                         selected = outcomeChoices,
                         multiple = TRUE,
                         options = outcomePickerOptions)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("washoutIncidence"),
                         label = "Washout",
                         choices = washoutChoices,
                         selected = washoutSelected,
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("daysPriorIncidence"),
                         label = "Days Prior History",
                         choices = daysPriorHistoryChoices,
                         selected = daysPriorHistorySelected,
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("intervalIncidence"),
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_interval),
                         multiple = FALSE),
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("sexIncidence"),
                         label = "Sex",
                         choices = sexChoices,
                         selected = sexSelected,
                         multiple = sexMultiple,
                         options = sexPickerOptions)
      ),
      column(4,
             pickerInput(inputId = ns("ageIncidence"),
                         label = "Age",
                         choices = ageChoices,
                         selected = ageSelected,
                         multiple = ageMultiple,
                         options = agePickerOptions)
      ),
      column(4,
             pickerInput(inputId = ns("repeatedIncidence"),
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_repeated_events),
                         multiple = FALSE),
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("timeFromIncidence"),
                         label = "From",
                         choices = startDateChoices,
                         selected = min(startDateChoices),
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("timeToIncidence"),
                         label = "To",
                         choices = startDateChoices,
                         selected = max(startDateChoices),
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("ribbonIncidence"),
                         label = "Ribbon",
                         choices = c(TRUE, FALSE),
                         selected = TRUE,
                         multiple = FALSE)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("showCIIncidence"),
                         label = "Show confidence interval",
                         choices = c(TRUE, FALSE),
                         selected = TRUE,
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("stackPlotsIncidence"),
                         label = "Stack plots",
                         choices = c(TRUE, FALSE),
                         selected = FALSE,
                         multiple = FALSE)
      )
    ),
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionInc"),
                                value = captionValue)
      )
    ),
    fluidRow(
      createAddItemToReportUI(ns(lockName)),
      createDownloadPlotUI(ns)
    ),
    tags$br(),
    fluidRow(
      column(12,
        plotOutput(ns("previewFigure"))
      )
    )
  )
}

incidenceServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    # Figure 1
    incidenceCommonData <- reactive({
      uploadedFiles <- uploadedFiles()
      incidence_estimates <- uploadedFiles$dataIP$incidence_estimates
      class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
      incidence_estimates <- incidence_estimates %>%
        mutate_if(is.numeric, list(~replace_na(., 0)))

      # Washout
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_outcome_washout %in% c(input$washoutIncidence))
      # Days Prior
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_days_prior_observation %in% c(input$daysPriorIncidence))
      # Database
      incidence_estimates <- incidence_estimates %>%
        filter(cdm_name %in% c(input$databaseIncidence))
      # Outcome
      incidence_estimates <- incidence_estimates %>%
        filter(outcome_cohort_name %in% input$outcomeIncidence)
      # Sex
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_sex %in% input$sexIncidence)
      # Age group
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_age_group %in% input$ageIncidence)

      # Start Time
      incidence_estimates <- incidence_estimates %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidence),
                       as.Date(input$timeToIncidence)))
      # Interval
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_interval == input$intervalIncidence)
      # Repeated events
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_repeated_events == input$repeatedIncidence)

      return(incidence_estimates)
    })

    previewFigure <- reactive({
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = id)
      if (id == "Plot - Incidence rate per year") {
        expression <- expression %>%
          addPreviewItemType(input$facetIncidence)
      } else if (id == "Plot - Incidence rate per year by sex") {
        expression <- expression %>%
          addPreviewItemTypeSex(input$facetIncidence)
      } else if (id == "Plot - Incidence rate per year by age") {
        expression <- expression %>%
          addPreviewItemTypeAge(input$facetIncidence)
      }
      expression <- expression %>%
        addPreviewItemRibbon(input$ribbonIncidence) %>%
        addPlotOptions(input$showCIIncidence, input$stackPlotsIncidence)
      incidence_estimates <- incidenceCommonData()
      if (nrow(incidence_estimates) > 0) {
        eval(parse(text = expression))
      }
    })

    output$previewFigure <- renderPlot({
      req(previewFigure())
      previewFigure()
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

    addObject <- reactiveVal()
    observeEvent(input$lockDataIncidenceYear, {
      addObject(
        list(`Plot - Incidence rate per year` = list(incidence_estimates = incidenceCommonData(),
                                                     plotOption = input$facetIncidence,
                                                     caption = input$captionInc,
                                                     ribbon = input$ribbonIncidence,
                                                     options = c(input$showCIIncidence, input$stackPlotsIncidence)))
      )
    })

    observeEvent(input$lockDataIncidenceSex, {
      addObject(
        list(`Plot - Incidence rate per year by sex` = list(incidence_estimates = incidenceCommonData(),
                                                            plotOption = input$facetIncidence,
                                                            caption = input$captionInc,
                                                            ribbon = input$ribbonIncidence,
                                                            options = c(input$showCIIncidence, input$stackPlotsIncidence)))
      )
    })

    observeEvent(input$lockDataIncidenceAge, {
      addObject(
        list(`Plot - Incidence rate per year by age` = list(incidence_estimates = incidenceCommonData(),
                                                            plotOption = input$facetIncidence,
                                                            caption = input$captionInc,
                                                            ribbon = input$ribbonIncidence,
                                                            options = c(input$showCIIncidence, input$stackPlotsIncidence)))
      )
    })
    return(addObject)
  })
}
