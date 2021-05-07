# devtools::load_all()

library(testthat)
library(recipes)
library(modeldata)
data(biomass)


rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

test_that('step_earth works', {
  with_earth <- rec %>%
    step_earth(all_numeric(), -all_outcomes(),
               outcome = "HHV", drop = FALSE)
  with_earth <- prep(with_earth, training = biomass, verbose = FALSE)

  with_earth_pred <- bake(with_earth, new_data = biomass)

  expect_true(any(grepl("^EARTH_h\\(.+\\)$", colnames(with_earth_pred))))
})

test_that('options = list(degree = 2) works', {
  with_earth <- rec %>%
    step_earth(all_numeric(), -all_outcomes(),
               outcome = "HHV", drop = FALSE,
               options = list(degree = 2))
  with_earth <- prep(with_earth, training = biomass, verbose = FALSE)

  with_earth_pred <- bake(with_earth, new_data = biomass)

  expect_true(any(grepl("^EARTH_h\\(.+\\)\\*h\\(.+\\)$", colnames(with_earth_pred))))
})
