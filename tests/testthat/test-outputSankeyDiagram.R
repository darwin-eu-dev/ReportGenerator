# Dummy data
data <- data.frame(
  path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate", "Acetaminophen-Aspirin"),
  freq = c(206, 6, 14),
  sex = c("all", "all", "all"),
  age = c("all", "all", "all"),
  indexYear = c("all", "all", "all"))

test_that("void", {
  expect_error(outputSankeyDiagram())
})

test_that("Sankey created file", {

  outputFile <- paste0(tempdir(), "sankeyPlot.html")
  outputSankey <- TreatmentPatterns::createSankeyDiagram(
    treatmentPathways = data,
    groupCombinations = TRUE)

  htmlwidgets::saveWidget(outputSankey, outputFile)

  expect_true(!is.null(outputSankey))
})
