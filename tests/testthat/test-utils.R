test_that("getItemsList all", {
  items <- c("incidence_attrition", "prevalence_attrition", "incidence_estimates", "prevalence_estimates", "treatmentPathways")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 12)
})

test_that("getItemsList attrition both", {
  items <- c("incidence_attrition", "prevalence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Table - Number of participants",
                           "Table - Incidence Attrition",
                           "Table - Prevalence Attrition"))
})

test_that("getItemsList only incidence_attrition", {
  items <- c("incidence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Table - Incidence Attrition"))
})

test_that("getItemsList only prevalence_attrition", {
  items <- c("prevalence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Table - Prevalence Attrition"))
})

test_that("getItemsList only incidence_estimate", {
  items <- c("incidence_estimates")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 4)
})

test_that("getItemsList only prevalence", {
  items <- c("prevalence_estimates")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 3)
})

test_that("getItemsList treatmentPatterns", {
  items <- c("treatmentPathways")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Sunburst Plot - TreatmentPatterns",
                           "Sankey Diagram - TreatmentPatterns"))
})

test_that("getItemsList joining to apps", {
  itemsList <- list(objects = NULL)
  itemsIP <- c("incidence_attrition", "prevalence_attrition", "incidence_estimates", "prevalence_estimates")
  itemsTP <- c("treatmentPathways")
  menuListIP <- getItemsList(itemsIP)
  menuListTP <- getItemsList(itemsTP)
  itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], menuListIP)
  itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], menuListTP)
  expect_equal(length(itemsList$objects[["items"]]), 12)
})

test_that("getItemConfig for getting a function", {
  title <- c("Table - Number of participants")
  expression <- getItemConfig(input = "title",
                              output = "function",
                              inputValue = title)
  expect_equal(expression, c("table1NumPar(incidence_attrition, prevalence_attrition)"))
})

test_that("getItemConfig for getting options", {
  title <- c("Table - Number of participants")
  itemOptions <- getItemConfig(input = "title",
                               output = "options",
                               inputValue = title)
  expect_equal(itemOptions, NULL)
})

test_that("getFunctionReport error more than length 1", {
  title <- c("Table - Number of participants", "Table - Incidence Attrition")
  expect_error(getItemConfig(input = "title",
                             output = "function",
                             inputValue = title))
})

test_that("addPreviewItemType happy flow", {
  result <- addPreviewItemType(previewItemString = getItemConfig(input = "title",
                                                                 output = "function",
                                                                 inputValue = "Plot - Incidence rate per year"),
                               previewItemType = "Facet by outcome")

  expect_equal(class(result), "character")
  expect_equal(result, "plotIncidence(incidence_estimates, colour = 'cdm_name', facet = 'outcome_cohort_name')")

  # type might be empty, set default
  result <- addPreviewItemType(previewItemString = getItemConfig(input = "title",
                                                                 output = "function",
                                                                 inputValue = "Plot - Incidence rate per year"),
                               previewItemType = NULL)

  expect_equal(class(result), "character")
  expect_equal(result, "plotIncidence(incidence_estimates, colour = 'cdm_name', facet = 'outcome_cohort_name')")
})


test_that("addPreviewItemType edge cases", {
  result <- addPreviewItemType(previewItemString = "incidenceRatePerYearPlot_no_brackets",
                               previewItemType = "Facet by outcome")
  expect_equal(result, "incidenceRatePerYearPlot_no_brackets")

  result <- addPreviewItemType(previewItemString = "",
                               previewItemType = "Facet by outcome")
  expect_equal(result, "")
})

