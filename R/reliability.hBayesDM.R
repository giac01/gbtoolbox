#' @section hBayesDM models:
#' The \code{hBayesDM} method dispatches automatically when \code{reliability()} is called
#' on a fitted \pkg{hBayesDM} model object (an object of class \code{"hBayesDM"}), and computes
#' reliability for one or more individual-level parameters extracted from the model's posterior
#' draws.
#'
#' \code{x} must be an \pkg{hBayesDM} model object fitted with \pkg{hBayesDM} >= 2.0 (the
#' \pkg{cmdstanr} backend), containing \code{$par_vals} (posterior draws) and
#' \code{$all_ind_pars} (per-subject estimates, including a \code{subjID} column) components.
#' Models fitted with older, \pkg{rstan}-based versions of \pkg{hBayesDM} are not supported.
#'
#' @param parameter Character vector of parameter names to analyze, or \code{"auto"} to
#'   automatically detect all individual-level parameters. Must match parameter names
#'   available in \code{model$par_vals}.
#' @param level Numeric. Credibility level for the highest density continuous interval.
#'   Default is \code{0.95}.
#'
#' @return An object of class \code{"reliability_hBayesDM"} containing a named list with one element
#'   per parameter. Each element contains the output from \code{\link{reliability}}, including:
#' \itemize{
#'   \item \code{hdci}: A data frame with point estimate (posterior mean) and highest density
#'     continuous interval for reliability
#'   \item \code{reliability_posterior_draws}: A numeric vector of posterior draws for reliability
#' }
#'
#' @references
#' Ahn, W.-Y., Haines, N., & Zhang, L. (2017). Revealing Neurocomputational Mechanisms
#' of Reinforcement Learning and Decision-Making With the hBayesDM Package.
#' Computational Psychiatry, 1, 24-57. \href{https://doi.org/10.1162/CPSY_a_00002}{doi:10.1162/CPSY_a_00002}
#'
#' @rdname reliability
#'
#' @examples
#' \dontrun{
#' # Fit an hBayesDM model
#' library(hBayesDM)
#' fit <- dd_hyperbolic(data = "example")
#'
#' # Calculate reliability for discount rate and inverse temperature parameters
# rel_results <- reliability.hBayesDM(
#   x = fit,
#   parameter = c("k", "beta")
# )
#'
#' # View results
#' print(rel_results)
#'
#' # Access individual parameter results
#' rel_results$k$hdci
#' rel_results$beta$reliability_posterior_draws
#' }
#'
#' @export
reliability.hBayesDM = function(
    x,
    parameter = "auto",
    level = .95,
    ...
){
  # browser()

  model = x

  model_name = model$model

  if (grepl("_single$", model_name)) {
    stop("reliability can only be calculated for hierarchical, multi-participant models ",
         "(reliability is not definable on the individual-participant level).")
  }

  # Requires hBayesDM >= 2.0 (cmdstanr backend), which stores posterior draws in
  # `$par_vals` and per-subject estimates in `$all_ind_pars`. Older, rstan-based fits
  # (`$parVals`/`$allIndPars`) are not supported.
  
  # Note: 
  #  all_ind_pars = data frame of per-subject point estimates for each individual-level (subject-varying) parameter in the model — one row per subject
  #  par_vals     = list of all MCMC draws for all model parameters. Each ROW is a specific MCMC draw (parameters on columns - can be subects on columns!).

  
  if (is.null(model$all_ind_pars) || is.null(model$par_vals)) {
    stop("`model` does not contain `$all_ind_pars`/`$par_vals` components. ",
         "reliability.hBayesDM() requires an hBayesDM model fitted with hBayesDM >= 2.0 ",
         "(cmdstanr backend); older rstan-based fits are not supported.")
  }

  sample_size  = length(model$all_ind_pars$subjID)
  model_draws0 = model$par_vals

  if (identical(parameter, "auto")){
    dims = lapply(model_draws0, dim)

    matching_params = names(dims[sapply(dims, function(d) {
      length(d) == 2 && d[2] == sample_size
    })])

    matching_params = matching_params[matching_params != "log_lik"]

    parameter = matching_params

  }

  if (!all(parameter %in% names(model_draws0))) stop("values entered for argument `parameter` not found among the model's posterior draws (model$par_vals)")

  model_draws = model_draws0[parameter]
  
  rmu_results = lapply(names(model_draws), function(param_name) {
    # cat("\nProcessing reliability of parameter:", param_name, "\n")
    reliability(t(model_draws[[param_name]]), level = level)
  })
  
  names(rmu_results) = names(model_draws)

  class(rmu_results) = "reliability_hBayesDM"
  attr(rmu_results, "n_subjects") = sample_size
  attr(rmu_results, "n_draws")    = nrow(model_draws[[1]])
  
  print(rmu_results)
  invisible(rmu_results)
  
}

#' Print method for reliability_hBayesDM objects
#'
#' Displays reliability estimates in a formatted table showing point estimates
#' and credible intervals for each parameter.
#'
#' @param x An object of class "reliability_hBayesDM" from \code{\link{reliability.hBayesDM}}
#' @param digits Integer. Number of decimal places to display.
#' @param ... Further arguments (ignored).
#'
#' @return Returns \code{x} invisibly
#'
#' @export
print.reliability_hBayesDM = function(
    x,
    digits = 3,
    ...
){
  # browser()
  results_matrix = sapply(x, function(item) (item$hdci))
  results_matrix = t(results_matrix)
  
  # Extract CI level
  ci_level = as.numeric(results_matrix[1, ".width"]) * 100
  
  # Extract only numeric columns and convert to numeric matrix
  remove_cols    = colnames(results_matrix) %in% c(".point",".interval",".width")
  results_matrix = results_matrix[,!remove_cols]
  results_matrix = round(apply(results_matrix, 2, as.numeric), digits)
  rownames(results_matrix) = names(x)
  
  cat("\nReliability (Relative Measurement Uncertainty) Estimates\n")
  cat(sprintf("Posterior Mean & %g%% HDCI\n", ci_level))
  cat(sprintf("  Number of subjects:        %d\n", attr(x, "n_subjects")))
  cat(sprintf("  Number of posterior draws: %d\n\n", attr(x, "n_draws")))
  print(results_matrix)
  cat("\n")
  
  invisible(x)
}
