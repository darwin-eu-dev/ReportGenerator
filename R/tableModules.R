tableUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionTable"),
                                value = "Table 1. Displays the total number of drug users, for each of the databases, during the study period. Total number of drug A users ranged from XXX to XXX across databases. Total number of drug B users ranged from XXX to XXX across databases. [continue for each drug]. When stratified by sex, number of male drug A users ranged from XXX to XXX  across databases,whereas number of female drug A users ranged from XXX to XXX across databases. [continue for each drug]. When stratified by age, number of drug A users in age group 1 ranged from XXX to XXX across databases, whereas number of drug A users in age group 2 ranged from XXX to XXX across databases. [continue for each age group and each drug ]. In summary, there were more users of [insert drug with highest count]; and least users of [insert drug with lowest count]. [Describe other observed patterns in the data].",
                                height = "130px")
      )
    ),
    fluidRow(
      column(4,
             actionButton(ns("lockTable"), "Add item to report")
      ),
    ),
    tags$br(),
    fluidRow(
      column(12,
             gt_output(ns("previewTable"))
      ),
    )
  )
}

tableServer <- function(id, uploadedFiles) {
  moduleServer(id, function(input, output, session) {

    output$previewTable <- render_gt({
      uploadedFiles <- uploadedFiles()
      incidence_estimates <- uploadedFiles$dataIP$incidence_estimates
      eval(parse(text = getItemConfig(input = "title",
                                      output = "function",
                                      inputValue = id)))
    })

    addObject <- reactiveVal()
    observeEvent(input$lockTable, {
      addObject(
        list(`Table - Number of participants by sex and age group` = list(incidence_estimates = uploadedFiles()$dataIP$incidence_estimates,
                                                                          caption = input$captionTable))
      )
    })
    return(addObject)
  })
}
