library(testthat)
rm(list = ls())
source("rapid/objects.R")

context("Argument trait")

test_that("the backup Argument trait constructor is well defined", {
  expect_error(ArgumentBckp$new(), "PROTO: You created an instance of the Trait 'Argument' without specifying the needed parameter 'name'.")
  expect_error(ArgumentBckp$new(name = ""), "PROTO: You created an instance of the Trait 'Argument' without specifying the needed parameter 'description'.")
})

test_that("the Argument trait constructor is well defined", {
  expect_error(Argument$new(), "PROTO: You created an instance of the Trait 'Argument' without specifying the needed parameter 'name'.")
  expect_error(Argument$new(name = ""), "PROTO: You created an instance of the Trait 'Argument' without specifying the needed parameter 'description'.")
  arg1 <- Argument$new(name = "arg1", description = "An argument")
  expect_equal(arg1$expectedValues, NA)
  expect_equal(arg1$defaultValue, "<none>")
  })

# test_that("the Argument ")

# 
# rapid.action <- "/helloWorld"
# 
# s_KT <- Argument$new(name = "s_KT"
#                      , description = "Name of the temporal key"
#                      , expectedValueType = "one string"
#                      , expectedValues = KTNames
#                      , default = '<required>'
#                      , addParam = 5)