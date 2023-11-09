
#' Create a ggplot2 Correlation Matrix
#'
#' @description
#'
#' The function provides a wrapper around ggplot2 to easily create a correlation matrix.
#'
#' @param dat A dataframe or matrix for which to compute correlations. Note: A correlation matrix should not be provided as input.
#' @param variable_labels A character vector of labels for each variable (i.e., each column in `dat`). If missing (NULL), column names from `dat` are used.
#' @param textadjust A scalar that adjusts text size by a specific magnification factor.
#' @param sample_size Logical. If TRUE, the function includes sample sizes on the upper diagonal. If FALSE, these are left blank.
#' @param p_threshold_col A numeric value indicating the p-value threshold for colouring.
#' @param confidence_interval Logical. If TRUE, the function includes confidence intervals on the upper diagonal. If FALSE, these are left blank.
#' @param low_colour Hex colour code for the lowest correlation value.
#' @param high_colour Hex colour code for the highest correlation value.
#' @param mid_colour Hex colour code for the middle point in the correlation scale.
#' @param abs_colour Logical. If TRUE, absolute correlation values (i.e., ignoring positive/negative status) are used for determining square colours.
#' @param cluster_variables Logical. If TRUE, variables are clustered in the plot. Default is FALSE.
#' @param n_decimal_places The number of decimal places to use for plot text.
#' @param suppress_warning_message Logical. If TRUE, suppresses warning message about the function being in beta.
#'
#' @return A ggplot object of the correlation matrix.
#'
#' @examples
#' X = sapply(1:10, function(i) rnorm(100))
#' X = as.data.frame(X)
#' My_Labels = c(paste0("Predictor ",1:5), paste0("Outcome ",1:5))
#'
#' plot_correlations(X, variable_labels = My_Labels, sample_size = TRUE, confidence_interval = FALSE)
#'
#' plot_correlations(X, variable_labels = My_Labels, sample_size = TRUE, confidence_interval = FALSE) +
#' ggplot2::labs(title = "My Title")
#' @export
#'
plot_correlations = function(dat,
                   variable_labels=NULL,
                   textadjust=2,
                   sample_size=TRUE,
                   p_threshold_col = NULL,
                   confidence_interval=TRUE,
                   low_colour="#0072B2",
                   high_colour="#D55E00",
                   mid_colour="white",
                   abs_colour=TRUE,
                   cluster_variables = FALSE,
                   n_decimal_places = 2,
                   suppress_warning_message = FALSE
){
  if (!suppress_warning_message) {warning("This function is in development, and not yet ready for widespread use. \n  Proceed with caution")}


  if (!base::is.data.frame(dat)) {dat=base::as.data.frame(dat)}

  # Cluster Variables
  if (cluster_variables) {
    new_order = gbtoolbox:::.sortVar(dat)
    dat = dat[,new_order]
    if (!base::is.null(variable_labels)){
      variable_labels = variable_labels[new_order]
    }
  }

  # Set Variable Names
  Variables = base::colnames(dat)
  if(is.null(variable_labels)){
    variable_labels = base::colnames(dat)
  }

  # Estimate Correlation Matrix
  correlation_matrix_vals = stats::cor(dat, use="pairwise.complete.obs")
  correlation_matrix_text_vals = apply(correlation_matrix_vals, 2, gbtoolbox::apa_num)

  #Matrix on Ns per comparison - lower triag
  sample_size_matrix = sapply(Variables, function(x)
    sapply(Variables, function(y)
      base::nrow(stats::na.omit(base::data.frame(dat[,base::unique(c(x,y))])))
    ))

  # Estimate P-values
  pvals = sapply(seq_along(Variables) ,function(c)
    sapply(seq_along(Variables), function(r)
      .correlation_pvalue(r = correlation_matrix_vals[r,c], n = sample_size_matrix[r,c])
    ))

  # Calculate Fill colors
  if(abs_colour){
    correlation_matrix_fill = base::abs(correlation_matrix_vals)  #Correlation matrix for table fill
  } else {
    correlation_matrix_fill = (correlation_matrix_vals)  #Correlation matrix for table fill
  }
  if(!is.null(p_threshold_col)) {
    correlation_matrix_fill[pvals > p_threshold_col] = NA
  }
  correlation_matrix_fill[base::lower.tri(correlation_matrix_fill,diag = TRUE)]=NA

  #Confidence interval information
  if(confidence_interval){
    correlation_matrix_conf_int = sapply(Variables, function(x)
      sapply(Variables, function(y)
        gbtoolbox:::.R_ConInt(as.numeric(dat[,x]),base::as.numeric(dat[,y]), n_decimal_places = n_decimal_places)$CI
      ))
    base::diag(correlation_matrix_conf_int) = base::diag(sample_size_matrix)
    sample_size_matrix = correlation_matrix_conf_int
  }

  #Create Dataframe For ggplot to Read

  data_to_plot = correlation_matrix_text_vals
  if(sample_size){
    data_to_plot[base::lower.tri(data_to_plot, diag=TRUE)]=sample_size_matrix[base::lower.tri(sample_size_matrix, diag=TRUE)]
  }
  if(!sample_size){
    data_to_plot[base::lower.tri(data_to_plot, diag=TRUE)]=""
  }

  base::colnames(data_to_plot) = variable_labels ;  base::rownames(data_to_plot) = variable_labels

  data_to_plot = base::data.frame(reshape2::melt(data_to_plot), stringsAsFactors = FALSE)

  data_to_plot$value = (base::as.character(data_to_plot$value))
  data_to_plot$ValueFill = base::as.numeric(t(c(correlation_matrix_fill)))
  data_to_plot$Var2 = base::factor(data_to_plot$Var2, levels=rev(base::levels(data_to_plot$Var2)))

  output_plot =
    ggplot2::ggplot(data = data_to_plot, ggplot2::aes(x=Var1, y=Var2,fill=ValueFill))+# + geom_point(aes(size=value^2,alpha=value^4))+
    ggplot2::geom_tile() + ggplot2::labs(x=NULL, y=NULL) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=5*textadjust)) +
    ggplot2::geom_text(ggplot2::aes(label=value), size=1.4*textadjust) +
    jtools::theme_apa() +
    #scale_fill_brewer(palette=1,na.value="grey")+
    # ggplot2::scale_fill_continuous(na.value="gray94",low=low_colour, high=high_colour) +
    ggplot2::scale_fill_gradient2(low = low_colour, mid = mid_colour, high = high_colour, midpoint = 0, na.value = "#F5F5F5", guide = "colourbar", aesthetics = "fill") +
    ggplot2::theme(legend.position = "#DAECED") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,hjust=1)) +
    ggplot2::coord_fixed()

  output_plot

  return(output_plot)
}
