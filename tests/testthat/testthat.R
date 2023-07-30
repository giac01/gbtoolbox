library(testthat)
library(devtools)
devtools::load_all()

# Create missing dataset

set.seed(10)

mtcars_missing = as.matrix(mtcars)

mtcars_missing[,1:5][runif(n = length(c(t(mtcars_missing[,1:5]))))>.7] <- NA

mtcars_missing = data.frame(mtcars_missing)

# This needs to be updated!

test_that("Check .R_ConInt function ", {
  expect_error(gbtools:::.R_ConInt(rnorm(10), rnorm(10)), NA)
})

test_that("Check apa_num ", {
  expect_error(gbtools::apa_num(rnorm(10)), NA)
})

test_that("Check plotcor",{

  data("mtcars")
  #Get base R result

  #Get package R result
  expect_error(
    gbtools::plot_correlations(mtcars, suppress_warning_message = TRUE)

    , NA
  )

})

test_that("Check plotcor clustering",{

  data("mtcars")
  #Get base R result

  #Get package R result
  expect_error(
    gbtools::plot_correlations(mtcars, cluster_variables = TRUE, suppress_warning_message = TRUE)

    , NA
  )

})

test_that("Check plot_pairwise_missing on mtcars_missing",{

  data("mtcars")
  #Get base R result

  #Get package R result
  expect_error(
    gbtools::plot_pairwise_missing(mtcars_missing, suppress_warning_message = TRUE)

    , NA
  )

})

test_that("Check plot_missing_correlations on mtcars_missing",{

  data("mtcars")
  #Get base R result

  #Get package R result
  expect_error(
    gbtools::plot_missing_correlations(mtcars_missing, suppress_warning_message = TRUE)

    , NA
  )

})