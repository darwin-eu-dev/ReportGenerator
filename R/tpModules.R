patternsUI <- function(id, uploadedFiles) {
  ns <- NS(id)

  cdmChoices <- unique(uploadedFiles$dataTP$treatmentPathways$cdm_name)
  sexChoices <- unique(uploadedFiles$dataTP$treatmentPathways$sex)
  ageChoices <- unique(uploadedFiles$dataTP$treatmentPathways$age)
  yearChoices <- unique(uploadedFiles$dataTP$treatmentPathways$indexYear)

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdmPatterns"),
                         label = "CDM",
                         choices = cdmChoices,
                         selected = cdmChoices[1],
                         multiple = FALSE)),
      column(4,
             pickerInput(inputId = ns("sexPatterns"),
                         label = "Sex",
                         choices = sexChoices,
                         selected = sexChoices[1],
                         multiple = FALSE)),
      column(4,
             pickerInput(inputId = ns("agePatterns"),
                         label = "Age",
                         choices = ageChoices,
                         selected = ageChoices[1],
                         multiple = FALSE))
    ),
    fluidRow(
        column(4,
               pickerInput(
               inputId = ns("indexPatterns"),
               label = "Index year",
               choices = yearChoices,
               selected = yearChoices[1],
               multiple = FALSE))
    ),
    tags$br(),
    fluidRow(
      tabsetPanel(type = "tabs",
                  tabPanel("Sunburst", br(),
                           fluidRow(column(6, actionButton(ns("lockSunburst"), "Add plot to the report")),
                                    column(6, downloadButton(ns("downloadSunburst"), "Download Plot"))),
                           htmlOutput(ns("previewSunburst"))),
                  tabPanel("Sankey", br(),
                           fluidRow(column(6, actionButton(ns("lockSankey"), "Add diagram to the report")),
                                    column(6, downloadButton(ns("downloadSankey"), "Download Plot"))),
                           htmlOutput(ns("previewSankey"))))
      )
    )
}

patternsServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {

    pathwaysData <- reactive({
      uploadedFiles <- uploadedFiles()
      treatmentPathways <- uploadedFiles$dataTP$treatmentPathways
      treatmentPathways %>%
        filter(cdm_name == input$cdmPatterns,
               sex == input$sexPatterns,
               age == input$agePatterns,
               indexYear == input$indexPatterns)
      })

    # Sunburst

    output$previewSunburst <- renderUI({
      treatmentPathways <- pathwaysData()
      if (nrow(treatmentPathways) > 0) {
        TreatmentPatterns::createSunburstPlot(treatmentPathways,
                                              groupCombinations = FALSE)
      }
    })

    # Sankey

    output$previewSankey <- renderUI({
      treatmentPathways <- pathwaysData()
      if (nrow(treatmentPathways) > 0) {
        TreatmentPatterns::createSankeyDiagram(treatmentPathways,
                                               groupCombinations = TRUE)
      }
    })

    output$downloadSunburst <- downloadHandler(
      filename = function() {
        paste("SunburstDiagram", ".html", sep = "")
      },
      content = function(file) {
        outputFile <- tempfile(pattern = "sunburstPlot", fileext = ".html")
        treatmentPathways <- pathwaysData()
        if (nrow(treatmentPathways) > 0) {
          outputSunburst <- TreatmentPatterns::createSunburstPlot(
            treatmentPathways = treatmentPathways,
            groupCombinations = TRUE,
          )
          htmlwidgets::saveWidget(outputSunburst, outputFile)
          file.copy(outputFile, file)
        }
      }
    )

    output$downloadSankey <- downloadHandler(
      filename = function() {
        paste("SankeyDiagram", ".html", sep = "")
      },
      content = function(file) {
        outputFile <- tempfile(pattern = "sankeyDiagram", fileext = ".html")
        treatmentPathways <- pathwaysData()
        if (nrow(treatmentPathways) > 0) {
          outputSankey <- TreatmentPatterns::createSankeyDiagram(
            treatmentPathways = treatmentPathways,
            groupCombinations = TRUE)
          htmlwidgets::saveWidget(outputSankey, outputFile)
          file.copy(outputFile, file)
        }
      }
    )

    addObject <- reactiveVal()

    observeEvent(input$lockSunburst, {
      outputFile <- here::here("sunburstPlot.html")

      treatmentPathways <- pathwaysData()
      if (nrow(treatmentPathways) > 0) {
        outputSunburst <- TreatmentPatterns::createSunburstPlot(
          treatmentPathways = treatmentPathways,
          groupCombinations = TRUE,
        )
        htmlwidgets::saveWidget(outputSunburst, outputFile)
        sunburstPNG <- tempfile(pattern = "sunburstPlot", fileext = ".png")
        webshot2::webshot(
          url = outputFile,
          file = sunburstPNG,
          vwidth = 1200,
          vheight = 1200)
        sunburstPNG <- normalizePath(sunburstPNG)
        addObject(
          list(`Sunburst Plot - TreatmentPatterns` = list(treatmentPathways = treatmentPathways,
                                                          groupCombinations = TRUE,
                                                          fileImage = sunburstPNG))
        )
      }
    })

    observeEvent(input$lockSankey, {
      outputFile <- here::here("sankeyDiagram.html")

      treatmentPathways <- pathwaysData()
      if (nrow(treatmentPathways) > 0) {
        outputSankey <- TreatmentPatterns::createSankeyDiagram(
          treatmentPathways = treatmentPathways,
          groupCombinations = TRUE,
        )
        htmlwidgets::saveWidget(outputSankey, outputFile)
        sankeyPNG <- tempfile(pattern = "sankeyDiagram", fileext = ".png")
        webshot2::webshot(
          url = outputFile,
          file = sankeyPNG,
          vwidth = 1200,
          vheight = 1200)
        sankeyPNG <- normalizePath(sankeyPNG)
        addObject(
          list(`Sankey Diagram - TreatmentPatterns` = list(treatmentPathways = treatmentPathways,
                                                           groupCombinations = TRUE,
                                                           fileImage = sankeyPNG)))
      }
    })
    return(addObject)
  })
}
