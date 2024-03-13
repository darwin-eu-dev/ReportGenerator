test_that("Loading 1 zip files whole study", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[1],
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading multiple zip files whole study", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

# Melanoma (runs just with local dataset)

# test_that("Loading multiple zip files whole study", {
#   csvLocation <- file.path(tempdir(), "dataLocation")
#   dir.create(csvLocation)
#   fileDataPath <- list.files(here::here("results",
#                                         "MultipleMyeloma",
#                                         "zip"),
#                              pattern = "zip",
#                              full.names = TRUE)
#   uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
#                                 csvLocation = csvLocation)
#   expect_equal(length(uploadedFiles), 3)
#   expect_type(uploadedFiles, "list")
#   expect_true("PatientProfiles" %in% names(uploadedFiles))
#   unlink(csvLocation, recursive = TRUE)
# })

test_that("Loading 1 csv files whole study", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("studies", "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[3],
                                fileName = fileName[3],
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 1)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading multiple csv files whole study", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("studies", "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                fileName = fileName,
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})
