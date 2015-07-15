rm(list = ls())

library(testthat)
source("rapid/objects.R")

context("Argument trait")
rapid.action <- "/test"

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


test_that("the default check function behaves correctly when there is no expectedValueType specified", {
  
  miscArg <- Argument$new(name = "miscArg", description = "")
  expect_equal(miscArg$check("5"), "5")
  expect_equal(miscArg$check("A string"), "A string")
  expect_equal(miscArg$check(""), "")
  
  withinArg <- Argument$new(name = "miscArg", description = "", expectedValues = c("A string", "Another string"))
  expect_equal(withinArg$check("A string"), "A string")
  expect_equal(withinArg$check("Another string"), "Another string")
  expect_error(withinArg$check("5"), "ARGS: Wrong value for")
  expect_error(withinArg$check(""), "ARGS: Wrong value for")
  
})


test_that("the default check function behaves correctly when the expectedValueType is integer", {
  
  miscArg <- Argument$new(name = "miscArg", description = "", expectedValueType = "integer")
  expect_equal(miscArg$check("5"), 5)
  expect_error(miscArg$check("A string"), "ARGS: Wrong value type for")
  expect_equal(miscArg$check(""), NA)
  
  withinArg <- Argument$new(name = "withinArg", description = "", expectedValues = c(5, 6), expectedValueType = "integer")
  expect_equal(withinArg$check("5"), 5)
  expect_error(withinArg$check("8"), "ARGS: Wrong value")
  expect_equal(withinArg$check(""), NA)
  
  rangeArg <- Argument$new(name = "rangeArg", description = "", expectedValues = list(min = 4, max = 6), expectedValueType = "integer")
  expect_equal(rangeArg$check("5"), 5)
  expect_error(rangeArg$check("8"), "ARGS: Value is not in range")
  expect_equal(rangeArg$check(""), NA)
  
})

# 
# rapid.action <- "/helloWorld"
# 
# s_KT <- Argument$new(name = "s_KT"
#                      , description = "Name of the temporal key"
#                      , expectedValueType = "one string"
#                      , expectedValues = KTNames
#                      , default = '<required>'
#                      , addParam = 5)