test_that("void", {
  expect_error(
    TreatmentPatterns::createSunburstPlot()
  )
})

test_that("minimal", {
  data <- data.frame(
    path = c(
      "Aspirin-End",
      "Acetaminophen-End",
      "Amoxicillin+Clavulanate-End",
      "Acetaminophen-Aspirin-End",
      "Aspirin-Acetaminophen-End",
      "Acetaminophen-Amoxicillin+Clavulanate-End",
      "Aspirin-Amoxicillin+Clavulanate-End",
      "Clavulanate-End",
      "Amoxicillin-End",
      "Amoxicillin+Clavulanate-Aspirin-End",
      "Acetaminophen+Amoxicillin-End",
      "Amoxicillin+Clavulanate-Acetaminophen-End"),
    freq = c(211, 206, 48, 14, 12, 6, 6, 3, 2, 2, 1, 1))

  expect_equal(class(TreatmentPatterns::createSunburstPlot(
    treatmentPathways = data,
    groupCombinations = TRUE)), c("sunburst", "htmlwidget"))
})
