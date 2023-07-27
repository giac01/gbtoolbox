library(testthat)
library(devtools)
devtools::load_all()

# devtools::test_active_file()


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
    gbtools::plot_correlations(mtcars)

    , NA
  )

})

test_that("Check plotcor clustering",{

  data("mtcars")
  #Get base R result

  #Get package R result
  expect_error(
    gbtools::plot_correlations(mtcars, cluster_variables = TRUE)

    , NA
  )

})
