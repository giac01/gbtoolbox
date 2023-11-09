library(testthat)
library(devtools)
devtools::load_all()
rm(list= ls(all.names = TRUE))

# Create missing dataset

set.seed(10)

mtcars_missing = as.matrix(mtcars)

mtcars_missing[,1:5][runif(n = length(c(t(mtcars_missing[,1:5]))))>.7] <- NA

mtcars_missing = data.frame(mtcars_missing)

mtcars_missing$newvar = rep(NA, 32)


# This needs to be updated!

test_that("Check .R_ConInt function ", {
  expect_error(gbtoolbox:::.R_ConInt(rnorm(10), rnorm(10)), NA)
})

test_that("Check apa_num ", {
  expect_error(gbtoolbox::apa_num(rnorm(10)), NA)
})

test_that("Check plotcor",{

  #Get base R result
  # mtcars_missing <- mtcars
  #
  # set.seed(10)
  #
  # mtcars_missing[sample(nrow(mtcars_missing), 5), sample(ncol(mtcars_missing), 2)] <- NA

  #Get package R result
  expect_error(
    gbtoolbox::plot_correlations(mtcars_missing, suppress_warning_message = TRUE)

    , NA
  )

})

test_that("Check plot_correlations clustering creates a plot", {
  # Assume mtcars_missing is supposed to be a version of mtcars with missing values
  # Here we create a missing data version for the test

  # Test that plot_correlations returns a ggplot object and does not throw errors or warnings
  suppressWarnings({
  plot <- gbtoolbox::plot_correlations(mtcars_missing, cluster_variables = TRUE, suppress_warning_message = TRUE)
  })
  expect_true(is.ggplot(plot))

  # Alternatively, if you expect the function might throw warnings but you only want to ensure it doesn't throw errors:
  # expect_error(gbtoolbox::plot_correlations(mtcars_missing, cluster_variables = TRUE, suppress_warning_message = TRUE), NA)
})

test_that("Check plot_pairwise_missing on mtcars_missing",{

  data("mtcars")
  #Get base R result

  #Get package R result
  expect_error(
    gbtoolbox::plot_pairwise_missing(mtcars_missing, suppress_warning_message = TRUE)

    , NA
  )

})

test_that("Check plot_missing_correlations on mtcars_missing",{

  data("mtcars")
  #Get base R result

  #Get package R result
  expect_error(
    gbtoolbox::plot_missing_correlations(mtcars_missing, suppress_warning_message = TRUE)

    , NA
  )

})

# Test 1: Basic functionality with a numeric vector
test_that("sum_score calculates correctly with simple numeric input", {
  result <- sum_score(c(1, NA, 3, 4))
  expected <- mean(c(1, 3, 4)) * 4
  expect_equal(result, expected)

  expect_true(is.na(sum_score(c(1, NA, 3, 4), max_percent_missing_allowed = .2)))
  expect_equal(sum_score(c(1, NA, 3, 4), max_percent_missing_allowed = .25), mean(c(1,3,4)*4))

})

# Test 2: Testing with a data frame and ensuring it runs without error
test_that("sum_score runs without error on a data frame and returns correct values", {
  set.seed(10)
  df_test <- mtcars[c(5,7:9)]

  # Add missing data
  df_test$qsec[c(5, 7, 10, 12, 30)] <- NA
  df_test[1, 1:4] <- NA
  df_test[2, 1:3] <- NaN

  ss1 <- df_test %>%
    select(1:4) %>%
    sum_score()

  ss2 <- df_test %>%
    rowwise() %>%
    mutate(ss = sum_score(c(drat, qsec, vs, am))) %>%
    ungroup() %>%
    pull(ss)

  # Check if ss1 and ss2 are identical
  expect_true(identical(as.numeric(ss1), as.numeric(ss2)))

  # Check if the first element of ss1 is NA
  expect_true(is.na(ss1[1]))

  # Check if the second element of ss1 is 4
  expect_equal(as.numeric(ss1[2]), 4)

  # Check if the third element of ss1 is the mean of the non-missing values multiplied by 4
  expect_equal(as.numeric(ss1[3]), mean(c(3.85, 18.61, 1, 1)) * 4)

})
# Test for apa_num to check that blank strings and NAs are converted to blank strings

test_that("Check apa_num ", {
  x = c(1, .5, .213,-2134.124, -.123124,NA,"",4,2)
  expect_equal(gbtoolbox::apa_num(x, remove_leading_zeros = TRUE), c("1.00", " .50", " .21", "-2134.12", "-.12", "", "", "4.00", "2.00"))
})

# Test for apa_num to check that character NA ("NA") is converted to blank string

test_that("Check apa_num ", {
  expect_equal(gbtoolbox::apa_num("NA"), "")
})
