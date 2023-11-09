#' Calculate Sum Scores with Optional Mean Imputation
#'
#' Calculates a sum score for each observation in a dataset, with handling for missing values.
#' The sum score is the product of the mean of non-missing values and the number of variables.
#' The function allows specifying the tolerance for missing data and provides additional diagnostic outputs.
#'
#' The function operates row-wise. For rows with missing data within the permissible limit,
#' it replaces missing values with the mean of the available values in that row before calculating
#' the sum score. It can also perform a loading check of the first principal component,
#' plot the distribution of the scores, and provide detailed missing data diagnostics.
#'
#'
#' @param input A data frame or a numeric vector containing the variables for sum score calculation.
#'              Ensure that only the relevant variables are included.
#' @param max_percent_missing_allowed Maximum allowed proportion of missing values for each observation.
#'                             Observations with missing data beyond this threshold are assigned `NA`.
#' @param check_loadings If `TRUE`, checks the loadings of the first principal component for
#'                       negative values, which may indicate data issues. For psychological questionnaire data,
#'                       this indicates that an item needs to be reverse coded.
#' @param plot_scores If `TRUE`, plots a histogram of the calculated sum scores.
#' @param print_missing_table If `TRUE`, prints a table summarizing the percentage of missing
#'                            data across all observations.
#' @param print_missing_each_input_var If `TRUE`, prints a table of missing data counts for each input variable.
#' @param custom_score_calc A custom function to calculate the sum score, defaults to mean imputation.
#'
#' @return A numeric vector of sum scores for each observation.
#'
#' @examples
#' # Assuming iris_test is a modified iris dataset with intentional NAs
#' iris_test = iris
#' iris_test[1,1:3] = NA
#' iris_test[c(4,15,16),2] = NA
#' iris_test %>%
#'   select(1:4) %>%
#'   sum_score(max_percent_missing_allowed = .2, print_missing_table = TRUE)
#'
#' # More complex usage with dplyr's rowwise operation
#' # Warnings and messages are printed to the console may not be visible when using pipes.
#' iris_test %>%
#'   rowwise() %>%
#'   mutate(ss = sum_score(c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width), max_percent_missing_allowed = .2)) %>%
#'   ungroup()
#'
#' @export
# note : documentation partly created by chatgpt4 (checked by the author)

sum_score = function(input=NULL,
                     max_percent_missing_allowed=1,
                     check_loadings = TRUE,
                     # verbosity
                     plot_scores = FALSE,
                     print_missing_table = FALSE,
                     print_missing_each_input_var = FALSE,
                     custom_score_calc = function(x) mean(x, na.rm = TRUE) * length(x)

){
  # browser()

  if (!is.data.frame(input) && !is.numeric(input)) {
    stop("Input must be a data frame or a numeric vector.")
  }

  if (class(input)=="numeric" ){
    input = data.frame(t(input))

    check_loadings = FALSE
  }

  perc_missing = rowSums(is.na(input)) / ncol(input)

  if (print_missing_table){
    perc_missing_table =  table(perc_missing)
    names(perc_missing_table) = gbtoolbox:::apa_num(as.numeric(names(perc_missing_table)))
    names(perc_missing_table) = paste0(round(as.numeric(names(perc_missing_table))*100), "%")
    cat("Percentage of missing data for each row of input data:\n")
    print(perc_missing_table)
    cat("\n")
  }

  if (print_missing_each_input_var) print(apply(input, 2,table))

  if (check_loadings){
    item_loadings = stats::loadings(stats::princomp(covmat = stats::cov(input, use = "pairwise.complete.obs"), fix_sign = TRUE, cor = TRUE, scores = FALSE) )[,1] # Need to check this again later!
    if (length(which(item_loadings<0))>0) {
      warning_message = paste0(
            c("PCA Warning: Negative loadings detected for items: ", which(item_loadings<0),
              "\nNegative loadings can indicate a reversal in the expected relationship between items and the principal component. ",
              "Review the loadings to ensure they meet expectations for your analysis."),
             collapse = " ")
      message(warning_message)
    }
  }

  scores = apply(input, 1, custom_score_calc)

  scores[perc_missing>max_percent_missing_allowed] = NA

  scores[perc_missing==1] = NA                                                  # Prevents NaNs from mean(c(NA,NA,NA))

  if (plot_scores) {
    hist(scores, main = "Distribution of Sum Scores", xlab = "Sum Scores")
  }

  return(scores)
}



