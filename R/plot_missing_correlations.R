#' Plot Missing Correlations
#'
#' This function plots how missingness in variables is correlated with other variables in the dataset.
#'
#' This is a very early version of the function and requires additional testing and verification.
#'
#' @param dat A dataframe with missing data that you want to analyze.
#' @param textadjust Adjust text size by a magnification factor.
#' @param p_threshold_col A numeric value indicating the p-value threshold for colouring.
#' @param low_colour Hex colour code for the lowest correlation.
#' @param high_colour Hex colour code for the highest correlation.
#' @param mid_colour Hex colour code for the midpoint of the correlation.
#' @param abs_colour Logical. If TRUE, will use the absolute correlation for determining square colour.
#' @param set_colour_to_p_value Logical. If TRUE, will set the colour to p-value.
#' @param cluster_variables Logical. If TRUE, clusters the variables on the plot.
#' This also moves data with no missing observations to the bottom.
#' @param n_decimal_places Number of decimal places to use for plotting.
#' @param suppress_warning_message Logical. If TRUE, suppresses warning message about the function being in beta.
#' @return A ggplot object showing how missingness in variables is correlated with other variables.
#'
#' @examples
#' data <- data.frame(A = rnorm(100), B = rnorm(100), C = rnorm(100))
#' data$A[sample(1:100, 20)] <- NA
#' data$B[sample(1:100, 30)] <- NA
#' plot_missing_correlations(data)
#'
#' @export


plot_missing_correlations = function(dat,
                             textadjust=2,
                             # sample_size=TRUE,
                             p_threshold_col = .05,
                             low_colour="#0072B2",
                             high_colour="#D55E00",
                             mid_colour="white",
                             abs_colour=TRUE,
                             set_colour_to_p_value = FALSE,
                             cluster_variables = TRUE,
                             n_decimal_places = 3,
                             suppress_warning_message = FALSE
){
  if (!suppress_warning_message) {warning("This function is in early beta, and not yet ready for widespread use. \n  Proceed with caution")}

  if (!base::is.data.frame(dat)) {dat=base::as.data.frame(dat)}

  # Estimate Missingness correlations and p-values
  missingness_results      = gbtools:::missingness_correlations(dat)
  correlation_matrix_miss  = missingness_results$cor
  correlation_matrix_pvals = missingness_results$p.value

  # Cluster Variables
  if (cluster_variables) {

    correlation_matrix_miss2 = correlation_matrix_miss
    correlation_matrix_miss2[is.na(correlation_matrix_miss2)] = 0

    cordist = stats::as.dist(correlation_matrix_miss2)
    clustres = stats::hclust(cordist)

    new_order = clustres$order

    correlation_matrix_miss = correlation_matrix_miss[new_order,new_order]
    correlation_matrix_pvals = correlation_matrix_pvals[new_order,new_order]

    # Move rows with no missing data to the top

    exclude_var = apply(correlation_matrix_miss, 1, function(x) length(which(!is.na(x)))!=0)
    # exclude_var = names(which(exclude_var))

    new_order = order(exclude_var)

    correlation_matrix_miss = correlation_matrix_miss[new_order,new_order]
    correlation_matrix_pvals = correlation_matrix_pvals[new_order,new_order]

  }

  # Calculate Fill colors
  if(abs_colour){
    correlation_matrix_fill = base::abs(correlation_matrix_miss)  #Correlation matrix for table fill
  }
  if(set_colour_to_p_value ){
    correlation_matrix_fill = correlation_matrix_pvals
  }
  else {
    correlation_matrix_fill = (correlation_matrix_miss)  #Correlation matrix for table fill
  }

  data_to_plot = apply(correlation_matrix_miss, 2, function(x) apa_num(x, n_decimal_places = n_decimal_places))

  base::rownames(data_to_plot) =  colnames(data_to_plot)

  data_to_plot = base::data.frame(reshape2::melt(data_to_plot), stringsAsFactors = FALSE)

  data_to_plot$value = (base::as.character(data_to_plot$value))
  data_to_plot$ValueFill = base::as.numeric(t(c(correlation_matrix_fill)))
  data_to_plot$Var2 = base::factor(data_to_plot$Var2, levels=rev(base::levels(data_to_plot$Var2)))

  if (!is.null(p_threshold_col)){
    data_to_plot_pval = base::data.frame(reshape2::melt(correlation_matrix_pvals), stringsAsFactors = FALSE)
    data_to_plot$ValueFill[data_to_plot_pval$value>p_threshold_col] = NA
  }

  # data_to_plot = data_to_plot[!(data_to_plot$Var1 %in% exclude_var),]

  output_plot =
    ggplot2::ggplot(data = data_to_plot, ggplot2::aes(x=Var2, y=Var1,fill=ValueFill))+# + geom_point(aes(size=value^2,alpha=value^4))+
    ggplot2::geom_tile() + ggplot2::labs(x=NULL, y=NULL) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=5*textadjust)) +
    ggplot2::geom_text(ggplot2::aes(label=value), size=1.4*textadjust) +
    jtools::theme_apa() +
    ggplot2::scale_fill_gradient2(low = low_colour, mid = mid_colour, high = high_colour, midpoint = 0, na.value = "#F5F5F5", guide = "colourbar", aesthetics = "fill") +
    ggplot2::theme(legend.position = "#DAECED") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,hjust=1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(y = "Outcome (is is.na(y))", x = "Predictor of Missingness")

  output_plot

  return(output_plot)
}
