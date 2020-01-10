library(testthat)

source("classes.R")
source("validity_checks.R")
source("constructors.R")

context("Classes")
test_that("animal class handles incorrect input correctly", {
  expect_error(animal(name = 10, weight = 20, female = TRUE), 'should be or extend class "character')
  expect_error(animal(name = "Sepp", weight = "20", female = TRUE), 'should be or extend class "numeric')
  expect_error(animal(name = "Sepp", weight = 20, female = "TRUE"), 'should be or extend class "logical')
  expect_error(animal(name = c("1", "1"), weight = 20, female = TRUE), "@name must be a character of length 1")
  expect_error(animal(name = "Sepp", weight = c(1:10), female = TRUE), "@weight must be a numeric of length 1")
  expect_error(animal(name = "Sepp", weight = 1, female = c(TRUE, FALSE)), "@female must be a logical of length 1")
})


test_that("prey class handles incorrect input correctly", {
  expect_error(prey(name = "Sepp", weight = 20, female = TRUE, hide = "1"), 'should be or extend class "numeric"')
  expect_error(prey(name = "Sepp", weight = 20, female = TRUE, hide = mtcars), 'should be or extend class "numeric"')
  expect_error(prey(name = "Sepp", weight = 20, female = TRUE, hide = 1.5), '@hide must be a numeric of length 1 in \\[0,1]')
  expect_error(prey(name = "Sepp", weight = "20", female = TRUE, hide = 0.8), "class \"character\" is not valid for slot 'weight'")
})

