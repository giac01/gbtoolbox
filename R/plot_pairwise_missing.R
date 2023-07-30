
#' Create a ggplot2 plot of pairwise missingness
#'
#' @description
#'
#' The function provides a wrapper around ggplot2 to easily create a paired sample size matrix
#'
#' @param dat A dataframe or matrix for which to compute pairwise sample sizes.
#' @param textadjust A scalar that adjusts text size by a specific magnification factor.
#' @param low_colour Hex colour code for the lowest correlation value.
#' @param high_colour Hex colour code for the highest correlation value.
#' @param cluster_variables Logical. If TRUE, variables are clustered in the plot. Default is FALSE.
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
plot_pairwise_missing = function(dat,
                             textadjust=4,
                             low_colour="white",
                             high_colour="#0072B2",
                             cluster_variables = FALSE,
                             suppress_warning_message = FALSE
){

  if (!suppress_warning_message) {warning("This function is in early beta, and not yet ready for widespread use. \n  Proceed with caution")}


  if (!base::is.data.frame(dat)) {dat=base::as.data.frame(dat)}

  # Cluster Variables
  if (cluster_variables) {
    new_order = gbtools:::.sortVar(dat)
    dat = dat[,new_order]
  }

  Variables = base::colnames(dat)

  #Matrix on Ns per comparison - lower triangle
  # Could be made more efficient in future as calculations are repeated.

  sample_size_matrix = sapply(Variables, function(x)
    sapply(Variables, function(y)
      base::nrow(stats::na.omit(base::data.frame(dat[,base::unique(c(x,y))])))
    ))

  sample_size_matrix[base::lower.tri(sample_size_matrix, diag = FALSE)] = ""

  # Calculate Fill Colors

  matrix_fill = sample_size_matrix

  matrix_fill[base::lower.tri(matrix_fill,diag = FALSE)]=NA

  #Create Dataframe For ggplot to Read

  data_to_plot = sample_size_matrix

  base::colnames(data_to_plot) = Variables ;  base::rownames(data_to_plot) = Variables

  data_to_plot = base::data.frame(reshape2::melt(data_to_plot), stringsAsFactors = FALSE)

  data_to_plot$value = (base::as.character(data_to_plot$value))
  data_to_plot$ValueFill = base::as.numeric(t(c(matrix_fill)))
  data_to_plot$Var2 = base::factor(data_to_plot$Var2, levels=rev(base::levels(data_to_plot$Var2)))

  output_plot =
    ggplot2::ggplot(data = data_to_plot, ggplot2::aes(x=Var1, y=Var2,fill=ValueFill))+
    ggplot2::geom_tile() + ggplot2::labs(x=NULL, y=NULL) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=5*textadjust)) +
    ggplot2::geom_text(ggplot2::aes(label=value), size=1.4*textadjust) +
    jtools::theme_apa() +
    ggplot2::scale_fill_gradient2(low = low_colour, high = high_colour, midpoint = 0, na.value = "#F5F5F5", guide = "colourbar", aesthetics = "fill") +
    ggplot2::theme(legend.position = "#DAECED") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,hjust=1)) +
    ggplot2::coord_fixed()

  output_plot

  return(output_plot)
}
