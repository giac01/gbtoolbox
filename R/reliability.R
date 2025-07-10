#' Estimate reliability from Bayesian statistical models
#'
#' This function measures internal consistency reliability using posterior draws from a Bayesian model.
#'
#' @param input_draws A matrix or data frame of posterior draws. Rows represent subjects and columns represent draws.
#' @param verbose Logical. Print detailed information about the input data. Default is TRUE.
#' @param level Numeric. Credibility level for the highest density continuous interval. Default is 0.95.
#'
#' @return A list containing:
#' - hdci: A data frame with the highest density continuous interval for the reliability posterior
#' - pd: A numeric value representing the posterior probability of the direction of the reliability
#' - reliability_posterior: A numeric vector of reliability values for each posterior draw
#'
#' @examples
#' \dontrun{
#' # Generate some example data
#' set.seed(123)
#' example_draws <- matrix(rnorm(1000), nrow = 50, ncol = 20)
#'
#' # Calculate reliability
#' result <- reliability(example_draws, verbose = TRUE)
#' print(result)
#' }
#'
#' @export
reliability <- function(
    input_draws,
    verbose = TRUE,
    level   = .95
) {
  if (!is.numeric(level) || level <= 0 || level >= 1) stop("level must be a numeric value between 0 and 1")

  input_draws <- as.matrix(input_draws)

  # Check for columns with zero SD
  sds <- apply(input_draws, 2, stats::sd, na.rm = TRUE)
  zero_sd_cols <- which(sds == 0)
  if (length(zero_sd_cols) > 0) {
    warning(sprintf("Found %d column(s) with zero standard deviation (columns: %s)",
                   length(zero_sd_cols),
                   paste(zero_sd_cols, collapse = ", ")))
  }

  # Check for NAs in input
  na_count <- base::sum(base::is.na(input_draws))
  if (na_count > 0) {
    warning(sprintf("Found %d NA value(s) in input_draws", na_count))
  }

  if (verbose) {
    base::print(paste0("Number of subjects: ", nrow(input_draws)))
    base::print(paste0("Number of posterior draws: ", ncol(input_draws)))
  }

  col_select <- base::sample(1:ncol(input_draws), replace = FALSE)
  input_draws_1 <- input_draws[, col_select[1:floor(length(col_select) / 2)]]
  input_draws_2 <- input_draws[, col_select[(floor(length(col_select) / 2) + 1):length(col_select)]]

   # Calculate correlations and handle NAs
  reliability_posterior <- sapply(1:ncol(input_draws_1), function(i) {
    x <- input_draws_1[, i]
    y <- input_draws_2[, i]

   # Return 0 if either column has zero variance
    if (stats::var(x, na.rm = TRUE) == 0 || stats::var(y, na.rm = TRUE) == 0) {
      return(0)
   }

   # Calculate correlation and handle NA
   cor_val <- stats::cor(x, y, method = "pearson")
   return(cor_val)
  })

  hdci <- ggdist::mean_hdci(reliability_posterior, .width = level)
  pd <- bayestestR::p_direction(reliability_posterior)$pd

  return(list(
    hdci = hdci,
    pd = pd,
    reliability_posterior = reliability_posterior))
}
