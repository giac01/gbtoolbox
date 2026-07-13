#' Estimate reliability (Relative Measurement Uncertainty) from Bayesian measurement models
#'
#' This function measures reliability using posterior draws from a fitted Bayesian model.
#' \code{reliability} is a generic function: the default method takes a matrix of posterior
#' draws directly (see below), while \code{\link{reliability.hBayesDM}} accepts a fitted
#' \pkg{hBayesDM} model object.
#'
#' To use the default method, you will need to provide a matrix (x) that contains the posterior draws for the parameter you wish to calculate reliability. The function assumes that rows of x represent subjects and columns represent posterior draws.
#'
#' For an example of how to apply this function to calculate mean score reliability using brms, see \href{https://www.bignardi.co.uk/8_bayes_reliability/tutorial_rmu_sum_score_reliability.html}{this tutorial}.
#'
#' For an example of how to apply this function to go/go-no task data using brms, see \href{https://www.bignardi.co.uk/8_bayes_reliability/tutorial_calculating_rmu_gonogo.html}{this tutorial}.
#'
#' @param x A matrix or data frame of posterior draws (rows = subjects, columns = draws), or an
#'   object with a specific \code{reliability} method, such as a fitted \pkg{hBayesDM} model
#'   (see \code{\link{reliability.hBayesDM}}).
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class \code{gbt_reliability}. Printing the object shows a short summary; the underlying object is a plain list that can be accessed as usual:
#' \itemize{
#'   \item hdci: A data frame with a point-estimate (posterior mean) and highest density continuous interval for reliability, calculated using the ggdist::mean_hdci function
#'   \item reliability_posterior_draws: A numeric vector of posterior draws for reliability, of length K/2 (K = number of columns/draws in your x matrix)
#' }
#'
#' @references
#' Bignardi, G., Kievit, R., & Bürkner, P. C. (2025). A general method for estimating reliability using Bayesian Measurement Uncertainty. PsyArXiv. \href{https://osf.io/preprints/psyarxiv/h54k8}{doi:10.31234/osf.io/h54k8}
#'
#' @examples
#' \dontrun{
#' # See https://www.bignardi.co.uk/8_bayes_reliability/tutorial_rmu_sum_score_reliability.html
#' # for more details on this example
#'
#' # Simulate data
#'
#' set.seed(1)
#' N                   = 5000 # number of subjects (mice)
#' J                   = 3    # number of measurements per subject
#' true_score_variance = 1
#' error_variance      = 10
#'
#' df = expand.grid(j = 1:J, mouse = 1:N)
#'
#' true_scores       = rnorm(N, mean = 10, sd = sqrt(true_score_variance))
#' measurement_error = rnorm(N*J, mean = 0, sd = sqrt(error_variance))
#'
#' df$measurement = true_scores[df$mouse] + measurement_error
#'
#' df_average_lengths = df %>%
#'   group_by(mouse) %>%
#'   summarise(average_measurement = mean(measurement))
#'
#' # Reliability should equal this:
#'
#' true_score_variance/(true_score_variance+error_variance/J)
#'
#' # Approximately the same as:
#'
#' cor(df_average_lengths$average_measurement, true_scores)^2
#'
#' # Fit model and calculate RMU
#'
#' brms_model = brm(
#'   measurement ~ 1 + (1 | mouse),
#'   data    = df
#' )
#'
#' # Extract posterior draws from brms model
#'
#' posterior_draws = brms_model %>%
#'   as_draws_df() %>%
#'   select(starts_with("r_mouse")) %>%
#'   t()
#'
#' # Calculate RMU
#'
#' reliability(posterior_draws)$hdci
#' }
#'
#'
#' @export
reliability <- function(x, ...) {
  UseMethod("reliability")
}

#' @param level Numeric. Credibility level for the highest density continuous interval. Default is 0.95.
#' @rdname reliability
#' @export
reliability.default <- function(
    x,
    level   = .95,
    ...
) {
  if (!is.numeric(level) || level <= 0 || level >= 1) stop("level must be a numeric value between 0 and 1")

  input_draws <- as.matrix(x)

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

  col_select <- base::sample(1:ncol(input_draws), replace = FALSE)
  input_draws_1 <- input_draws[, col_select[1:floor(length(col_select) / 2)]]
  input_draws_2 <- input_draws[, col_select[(floor(length(col_select) / 2) + 1):length(col_select)]]

   # Calculate correlations and handle NAs
  reliability_posterior_draws <- sapply(1:ncol(input_draws_1), function(i) {
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

  hdci <- ggdist::mean_hdci(reliability_posterior_draws, .width = level)

  colnames(hdci)[1] = "rmu_estimate"
  colnames(hdci)[2] = "hdci_lowerbound"
  colnames(hdci)[3] = "hdci_upperbound"

  output <- list(
    hdci = hdci,
    reliability_posterior_draws = reliability_posterior_draws)

  attr(output, "n_subjects") <- nrow(input_draws)
  attr(output, "n_draws")    <- ncol(input_draws)
  attr(output, "level")      <- level
  class(output) <- "gbt_reliability"

  return(output)
}

#' Print method for gbt_reliability objects
#'
#' Prints a short summary of the reliability estimate instead of the full list of posterior draws. The full results remain accessible via \code{$hdci} and \code{$reliability_posterior_draws}.
#'
#' @param x An object of class \code{gbt_reliability}, as returned by \code{\link{reliability}}.
#' @param digits Number of decimal places to display. Default is 3.
#' @param ... Further arguments (ignored).
#'
#' @return \code{x}, invisibly.
#'
#' @export
print.gbt_reliability <- function(x, digits = 3, ...) {
  cat("Reliability (Relative Measurement Uncertainty)\n")
  cat(sprintf("  Number of subjects:        %d\n", attr(x, "n_subjects")))
  cat(sprintf("  Number of posterior draws: %d\n", attr(x, "n_draws")))
  cat("\n")
  cat(sprintf("  RMU estimate: %s\n", formatC(x$hdci$rmu_estimate, digits = digits, format = "f")))
  cat(sprintf("  Posterior SD: %s\n", formatC(stats::sd(x$reliability_posterior_draws), digits = digits, format = "f")))
  cat(sprintf("  %s%% HDCI:     [%s, %s]\n",
              format(100 * attr(x, "level")),
              formatC(x$hdci$hdci_lowerbound, digits = digits, format = "f"),
              formatC(x$hdci$hdci_upperbound, digits = digits, format = "f")))
  cat("\nAccess full results with $hdci and $reliability_posterior_draws\n")
  invisible(x)
}
