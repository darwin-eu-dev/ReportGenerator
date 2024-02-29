testData <- testData()

incidence_estimates_test <- testData$incidence_estimates

test_that("incidenceServer works", {
  uploadedFiles <- reactiveValues(dataIP = list(incidence_estimates = incidence_estimates_test))

  testServer(incidenceServer, args = list(uploadedFiles = reactive(uploadedFiles)), {
    expect_equal(class(incidenceCommonData()), c("IncidencePrevalenceResult",
                                                 "IncidenceResult",
                                                 "tbl_df",
                                                 "tbl",
                                                 "data.frame"))
  })

})
