testData <- testData()

incidence_attrition_test <- testData$incidence_attrition
incidence_estimates_test <- testData$incidence_estimates
prevalence_attrition_test <- testData$prevalence_attrition
prevalence_estimates_test <- testData$prevalence_estimates
treatmentPathways_test <- testData$treatmentPathways_test
summarisedCharacteristics <- testData$`Summarised Characteristics`
summarisedCharacteristicsLSC <- testData$`Summarised Large Scale Characteristics`
survivalEstimate <- testData$`Survival estimate`
survivalCumulativeIncidence <- testData$`Survival cumulative incidence`

test_that("datasetLoad IncPrev", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("StudyResults.zip"),
                                               datapath = c(test_path("studies",
                                                                      "zip",
                                                                      "StudyResults.zip"))))

    expect_equal(length(uploadedFiles), 3)
  })
})

test_that("reset data", {
  testServer(reportGenerator(), {
    session$setInputs(resetData = TRUE)
    expect_equal(uploadedFiles$dataIP, NULL)
    expect_equal(uploadedFiles$dataTP, NULL)
    expect_equal(uploadedFiles$dataPP, NULL)
    expect_equal(itemsList$objects, NULL)
    expect_s3_class(datasetLoadServer("IncidencePrevalence"), "shiny.render.function")
    expect_s3_class(datasetLoadServer("TreatmentPatterns"), "shiny.render.function")
    expect_s3_class(datasetLoadServer("PatientProfiles"), "shiny.render.function")
  })
})

test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics",
                                      summarisedCharacteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          summarisedCharacteristics), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      summarisedCharacteristicsLSC), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          summarisedCharacteristicsLSC), "reactiveVal")
  })
})

test_that("survival modules classes", {
  testServer(reportGenerator(), {
    expect_s3_class(cohortSurvivalUI("survivalTable", survivalEstimate), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("survivalTable", survivalEstimate), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("survivalPlot", survivalEstimate), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("survivalPlot", survivalEstimate), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("failureTable", survivalCumulativeIncidence), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("failureTable", survivalCumulativeIncidence), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("failurePlot", survivalCumulativeIncidence), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("failurePlot", survivalCumulativeIncidence), "reactiveVal")
  })
})




