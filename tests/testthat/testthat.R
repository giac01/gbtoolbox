library(testthat)
library(devtools)
devtools::load_all()
library(giaco_tools)

# devtools::test_active_file()


# This needs to be updated!

test_that("Check .R_ConInt function ", {
  expect_error(gbtools:::.R_ConInt(rnorm(10), rnorm(10)), NA)
})


test_that("Check plotcor",{

  data("mtcars")
  #Get base R result

  #Get package R result
  expect_error(
    gbtools::plotcor(mtcars)

    , NA
  )

})

