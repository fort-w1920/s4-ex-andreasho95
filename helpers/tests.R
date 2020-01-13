library(testthat)

source("classes.R")
source("validity_checks.R")
source("constructors.R")

context("Classes handle correctly with incorrect input")
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
  expect_error(prey(name = "Sepp", weight = 20, female = TRUE, hide = 1.5), '@hide must be a numeric of length 1 in \\[0, 1]')
  expect_error(prey(name = "Sepp", weight = "20", female = TRUE, hide = 0.8), "class \"character\" is not valid for slot 'weight'")
})

test_that("predator class handles incorrect input correctly", {
  expect_error(predator(name = "Sepp", weight = 20, female = TRUE, seek = "1"), 'should be or extend class "numeric"')
  expect_error(predator(name = "Sepp", weight = 20, female = TRUE, seek = mtcars), 'should be or extend class "numeric"')
  expect_error(predator(name = "Sepp", weight = 20, female = TRUE, seek = 1.5), '@seek must be a numeric of length 1 in \\[0, 1]')
  expect_error(predator(name = "Sepp", weight = "20", female = TRUE, seek = 0.8), "class \"character\" is not valid for slot 'weight'")
})

test_that("mouse class handles incorrect input correctly", {
  expect_error(mouse(name = "Sepp", weight = 0.8, female = TRUE, hide = "1"), 'should be or extend class "numeric"')
  expect_error(mouse(name = "Sepp", weight = 0.8, female = TRUE, hide = mtcars), 'should be or extend class "numeric"')
  # Next one is first checked by prey class (Checks wether hide in intervall [0, 1])
  expect_error(mouse(name = "Sepp", weight = 0.8, female = TRUE, hide = 1.5), '@hide must be a numeric of length 1 in \\[0, 1]')
  # Next one is first checked by mouse class (Checks wether hide in intervall [0.6, 1])
  expect_error(mouse(name = "Sepp", weight = 0.8, female = TRUE, hide = 0.2), '@hide must be a numeric of length 1 in \\[0.6, 1]')
  expect_error(mouse(name = "Sepp", weight = "0.8", female = TRUE, hide = 0.8), "class \"character\" is not valid for slot 'weight'")
  expect_error(mouse(name = "Sepp", weight = 20, female = TRUE, hide = 0.7), '@weight must be a numeric of length 1 in \\[0.5, 1]')
  expect_error(mouse(name = "Sepp", weight = 0.2, female = TRUE, hide = 0.7), '@weight must be a numeric of length 1 in \\[0.5, 1]')
})

test_that("rabbit class handles incorrect input correctly", {
  expect_error(rabbit(name = "Sepp", weight = 0.8, female = TRUE, hide = "1"), 'should be or extend class "numeric"')
  expect_error(rabbit(name = "Sepp", weight = 0.8, female = TRUE, hide = mtcars), 'should be or extend class "numeric"')
  # Next one is first checked by prey class (Checks wether hide in intervall [0, 1])
  expect_error(rabbit(name = "Sepp", weight = 0.8, female = TRUE, hide = 1.5), '@hide must be a numeric of length 1 in \\[0, 1]')
  # Next one is first checked by rabbit class (Checks wether hide in intervall [0.3, 0.8])
  expect_error(rabbit(name = "Sepp", weight = 2, female = TRUE, hide = 0.2), '@hide must be a numeric of length 1 in \\[0.3, 0.8]')
  expect_error(rabbit(name = "Sepp", weight = "2", female = TRUE, hide = 0.8), "class \"character\" is not valid for slot 'weight'")
  expect_error(rabbit(name = "Sepp", weight = 20, female = TRUE, hide = 0.7), '@weight must be a numeric of length 1 in \\[1, 5]')
  expect_error(rabbit(name = "Sepp", weight = 0.2, female = TRUE, hide = 0.7), '@weight must be a numeric of length 1 in \\[1, 5]')
})

test_that("deer class handles incorrect input correctly", {
  expect_error(deer(name = "Sepp", weight = 0.8, female = TRUE, hide = "1"), 'should be or extend class "numeric"')
  expect_error(deer(name = "Sepp", weight = 0.8, female = TRUE, hide = mtcars), 'should be or extend class "numeric"')
  # Next one is first checked by prey class (Checks wether hide in intervall [0, 1])
  expect_error(deer(name = "Sepp", weight = 0.8, female = TRUE, hide = 1.5), '@hide must be a numeric of length 1 in \\[0, 1]')
  # Next one is first checked by deer class (Checks wether hide in intervall [0.2, 0.7])
  expect_error(deer(name = "Sepp", weight = 2, female = TRUE, hide = 0.9), '@hide must be a numeric of length 1 in \\[0.2, 0.7]')
  expect_error(deer(name = "Sepp", weight = "2", female = TRUE, hide = 0.8), "class \"character\" is not valid for slot 'weight'")
  expect_error(deer(name = "Sepp", weight = 5, female = TRUE, hide = 0.7), '@weight must be a numeric of length 1 in \\[15, 30]')
  expect_error(deer(name = "Sepp", weight = 0.2, female = TRUE, hide = 0.7), '@weight must be a numeric of length 1 in \\[15, 30]')
})

test_that("hawk class handles incorrect input correctly", {
  expect_error(hawk(name = "Sepp", weight = 0.8, female = TRUE, seek = "1"), 'should be or extend class "numeric"')
  expect_error(hawk(name = "Sepp", weight = 0.8, female = TRUE, seek = mtcars), 'should be or extend class "numeric"')
  # Next one is first checked by prey class (Checks wether seek in intervall [0, 1])
  expect_error(hawk(name = "Sepp", weight = 0.8, female = TRUE, seek = 1.5), '@seek must be a numeric of length 1 in \\[0, 1]')
  # Next one is first checked by hawk class (Checks wether seek in intervall [0.2, 0.7])
  expect_error(hawk(name = "Sepp", weight = 5, female = TRUE, seek = 0.2), '@seek must be a numeric of length 1 in \\[0.6, 1]')
  expect_error(hawk(name = "Sepp", weight = "5", female = TRUE, seek = 0.8), "class \"character\" is not valid for slot 'weight'")
  expect_error(hawk(name = "Sepp", weight = 50, female = TRUE, seek = 0.7), '@weight must be a numeric of length 1 in \\[3, 8]')
  expect_error(hawk(name = "Sepp", weight = 0.2, female = TRUE, seek = 0.7), '@weight must be a numeric of length 1 in \\[3, 8]')
})

test_that("lynx class handles incorrect input correctly", {
  expect_error(lynx(name = "Sepp", weight = 0.8, female = TRUE, seek = "1"), 'should be or extend class "numeric"')
  expect_error(lynx(name = "Sepp", weight = 0.8, female = TRUE, seek = mtcars), 'should be or extend class "numeric"')
  # Next one is first checked by prey class (Checks wether seek in intervall [0, 1])
  expect_error(lynx(name = "Sepp", weight = 0.8, female = TRUE, seek = 1.5), '@seek must be a numeric of length 1 in \\[0, 1]')
  # Next one is first checked by lynx class (Checks wether seek in intervall [0.2, 0.7])
  expect_error(lynx(name = "Sepp", weight = 50, female = TRUE, seek = 0.2), '@seek must be a numeric of length 1 in \\[0.5, 0.9]')
  expect_error(lynx(name = "Sepp", weight = "50", female = TRUE, seek = 0.8), "class \"character\" is not valid for slot 'weight'")
  expect_error(lynx(name = "Sepp", weight = 5, female = TRUE, seek = 0.7), '@weight must be a numeric of length 1 in \\[20, 60]')
  expect_error(lynx(name = "Sepp", weight = 0.2, female = TRUE, seek = 0.7), '@weight must be a numeric of length 1 in \\[20, 60]')
})


context("Classes handle correctly with proper input")
test_that("animal class handles proper input correctly", {
  test_animal <- animal(name = "Dieter", weight = 20, female = FALSE)
  expect_true(class(test_animal) == "animal")
  expect_true(test_animal@name == "Dieter")
  expect_true(test_animal@female == FALSE)
  expect_true(test_animal@weight == 20)
})

test_that("prey class handles proper input correctly", {
  test_prey <- prey(name = "Dieter", weight = 20, female = FALSE, hide = 1)
  expect_true(class(test_prey) == "prey")
  expect_true(test_prey@name == "Dieter")
  expect_true(test_prey@female == FALSE)
  expect_true(test_prey@weight == 20)
  expect_true(test_prey@hide == 1)
})

test_that("predator class handles proper input correctly", {
  test_predator <- predator(name = "Dieter", weight = 20, female = FALSE, seek = 0.5)
  expect_true(class(test_predator) == "predator")
  expect_true(test_predator@name == "Dieter")
  expect_true(test_predator@female == FALSE)
  expect_true(test_predator@weight == 20)
  expect_true(test_predator@seek == 0.5)
})

test_that("mouse class handles proper input correctly", {
  test_mouse <- mouse(name = "Dieter", weight = 0.5, female = TRUE, hide = 0.8)
  expect_true(class(test_mouse) == "mouse")
  expect_true(test_mouse@name == "Dieter")
  expect_true(test_mouse@female == TRUE)
  expect_true(test_mouse@weight == 0.5)
  expect_true(test_mouse@hide == 0.8)
})

test_that("rabbit class handles proper input correctly", {
  test_rabbit <- rabbit(name = "Dieter", weight = 2, female = TRUE, hide = 0.8)
  expect_true(class(test_rabbit) == "rabbit")
  expect_true(test_rabbit@name == "Dieter")
  expect_true(test_rabbit@female == TRUE)
  expect_true(test_rabbit@weight == 2)
  expect_true(test_rabbit@hide == 0.8)
})

test_that("deer class handles proper input correctly", {
  test_deer <- deer(name = "Dieter", weight = 15, female = TRUE, hide = 0.4)
  expect_true(class(test_deer) == "deer")
  expect_true(test_deer@name == "Dieter")
  expect_true(test_deer@female == TRUE)
  expect_true(test_deer@weight == 15)
  expect_true(test_deer@hide == 0.4)
})

test_that("hawk class handles proper input correctly", {
  test_hawk <- hawk(name = "Dieter", weight = 7, female = TRUE, seek = 0.8)
  expect_true(class(test_hawk) == "hawk")
  expect_true(test_hawk@name == "Dieter")
  expect_true(test_hawk@female == TRUE)
  expect_true(test_hawk@weight == 7)
  expect_true(test_hawk@seek == 0.8)
})

test_that("lynx class handles proper input correctly", {
  test_lynx <- lynx(name = "Dieter", weight = 37, female = TRUE, seek = 0.8)
  expect_true(class(test_lynx) == "lynx")
  expect_true(test_lynx@name == "Dieter")
  expect_true(test_lynx@female == TRUE)
  expect_true(test_lynx@weight == 37)
  expect_true(test_lynx@seek == 0.8)
})
