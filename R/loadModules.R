datasetLoadUI <- function(id) {
  uiOutput(NS(id, "datasetLoad"))
}


datasetLoadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    configData <- yaml.load_file(system.file("config",
                                             "variablesConfig.yaml",
                                             package = "ReportGenerator"))
    output$datasetLoad <- renderUI({
      tagList(tags$div(tags$h4("Load results"), class = "form-group shiny-input-container"),
              fileInput("datasetLoad",
                        "Upload your files",
                        accept = c(".zip", ".csv"),
                        multiple = TRUE,
                        placeholder = "ZIP or CSV")
      )
    })
  })
}
