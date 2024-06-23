#' Estimate internal consistency teliability from Bayesian statistical models
#'
#' This function measures internal consistency reliability using posterior draws from a Bayesian model.
#'
#' For more detail, see our pre-print: ADD LINK
#'
#' @param input_draws A matrix or data frame of posterior draws. Rows represent subjects and columns represent draws.
#' @param verbose A logical value indicating whether to print detailed information about the input data. Default is TRUE.
#' @param level A numeric value indicating the credibility level for the highest density continuous interval (HDCI). Default is 0.95 (95%).

#'
#' @return A list with the following components:
#' \describe{
#'   \item{hdci}{A data frame with the highest density continuous interval (HDCI) for the reliability posterior.}
#'   \item{pd}{A numeric value representing the posterior probability of the direction of the reliability.}
#'   \item{reliability_posterior}{A numeric vector of reliability values for each posterior draw.}
#' }
#'
#' @details
#' The function calculates the reliability by randomly splitting the posterior draws into two halves and then computing
#' the Pearson correlation between corresponding columns of the two halves. This process is repeated for each pair
#' of columns, and the resulting correlations form the reliability posterior distribution. The function then calculates
#' the 95% highest density continuous interval (HDCI) and the posterior probability of the direction (pd) of the reliability.
#'
#' @references
#' ADD REF
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

  if (verbose) {
    print(paste0("Number of subjects: ", nrow(input_draws)))
    print(paste0("Number of posterior draws: ", ncol(input_draws)))
  }

  col_select <- sample(1:ncol(input_draws), replace = FALSE)
  input_draws_1 <- input_draws[, col_select[1:floor(length(col_select) / 2)]]
  input_draws_2 <- input_draws[, col_select[(floor(length(col_select) / 2) + 1):length(col_select)]]
  reliability_posterior <- sapply(1:ncol(input_draws_1), function(i) cor(input_draws_1[, i], input_draws_2[, i], method = "pearson"))
  hdci <- ggdist::mean_hdci(reliability_posterior, .width = level)
  pd <- bayestestR::p_direction(reliability_posterior)$pd

  return(list(
    hdci = hdci,
    pd = pd,
    reliability_posterior = reliability_posterior))
}
