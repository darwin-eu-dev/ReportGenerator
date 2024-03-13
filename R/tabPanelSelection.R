tabPanelSelection <- function(selection, uploadedFiles) {
  if (selection == "Table - Number of participants") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Table - Incidence Attrition") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Table - Prevalence Attrition") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Table - Number of participants by sex and age group") {
    tabPanel(selection, tableUI(selection, uploadedFiles))
  } else if (selection == "Plot - Incidence rate per year") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Incidence rate per year by sex") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Incidence rate per year by age") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Prevalence per year") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Prevalence per year by sex") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Prevalence per year by age") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Treatment Pathways Interactive Plots") {
    tabPanel(selection, patternsUI(selection, uploadedFiles))
  } else if (selection == "summarised_characteristics") {
    tabPanel(selection, characteristicsUI("characteristics", uploadedFiles))
  } else if (selection == "Summarised Large Scale Characteristics") {
    tabPanel(selection, characteristicsUI("lsc", uploadedFiles))
  } else if (selection == "Survival table") {
    tabPanel(selection, cohortSurvivalUI("survivalTable", uploadedFiles))
  } else if (selection == "Survival plot") {
    tabPanel(selection, cohortSurvivalUI("survivalPlot", uploadedFiles))
  } else if (selection == "Cumulative incidence table") {
    tabPanel(selection, cohortSurvivalUI("failureTable", uploadedFiles))
  } else if (selection == "Cumulative incidence plot") {
    tabPanel(selection, cohortSurvivalUI("failurePlot", uploadedFiles))
  }
}
