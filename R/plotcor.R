
#' Create a ggplot2 Correlation Matrix
#'
#' @description
#'
#' The function provides a wrapper around ggplot2 to quickly create a correlation matrix.
#'
#' @param dat Input dataframe or matrix (*do not input a correlation matrix*).
#' @param Variables_Labels Character vector of variable Labels, corresponding to each column in dat. If missing (NULL) then colnames(dat) will be used.
#' @param textadjust Scalar. Adjust text size by a magnification factor.
#' @param includeN Logical. Include sample size on upper diagononal (TRUE) or leave blank (FALSE).
#' @param reportCI Logical. Include confidence interval on upper diagonal (TRUE) or leave blank (FALSE).
#' @param low_colour Logical. Hex colour code for the lowest correlation.
#' @param high_colour Logical. Hex colour code for the highest correlation.
#' @param abs_colour Logical. If TRUE, will use the absolute correlation (i.e. ignoring whether the correlation is positive or negative) for determining square colour.
#'
#' @return ggplot
#'
#' @examples
#' X = sapply(1:10, function(i) rnorm(100))
#' X = as.data.frame(X)
#' My_Labels = c(paste0("Predictor ",1:5), paste0("Outcome ",1:5))
#'
#' plotcor(X, Variables_Labels = My_Labels, includeN = TRUE, reportCI = FALSE)
#'
#' @export
#'
plotcor = function(dat,
                   Variables_Labels=NULL, textadjust=2, includeN=TRUE, reportCI=TRUE,
                   low_colour="#0072B2", high_colour="#D55E00", mid_colour="white", abs_colour=TRUE,
                   cluster_variables = FALSE, ndigits = 2
){
  if (!base::is.data.frame(dat)) {dat=base::as.data.frame(dat)}

  if (cluster_variables) {
    new_order = giacotools:::.sortVar(dat)
    dat = dat[,new_order]
    if (!base::is.null(Variables_Labels)){
      Variables_Labels = Variables_Labels[new_order]
    }
  }

  Variables = base::colnames(dat)
  if(is.null(Variables_Labels)){
    Variables_Labels = base::colnames(dat)
  }

  matrix_scores = dat
  Mat_Cor = cor(matrix_scores, use="pairwise.complete.obs")
  Mat_Cor = matrix(sprintf(paste0("%.",ndigits,"f"), Mat_Cor), ncol = ncol(Mat_Cor))
  Mat_Cor = apply(Mat_Cor,2, function(x) gsub("^(-?)0+\\.", "\\1.",x))

  if(abs_colour){
    Mat_Cor_fill = base::abs(stats::cor(matrix_scores, use="pairwise.complete.obs"))  #Correlation matrix for table fill
  } else {
    Mat_Cor_fill = (stats::cor(matrix_scores, use="pairwise.complete.obs"))  #Correlation matrix for table fill
  }

  Mat_Cor_fill[base::lower.tri(Mat_Cor_fill,diag = TRUE)]=NA

  #Matrix on Ns per comparison - lower triag
  Mat_N = sapply(Variables, function(x)
    sapply(Variables, function(y)
      base::nrow(stats::na.omit(base::data.frame(dat[,base::unique(c(x,y))])))
    ))

  #Confidence interval information
  if(reportCI){
    Mat_CI = sapply(Variables, function(x)
      sapply(Variables, function(y)
        gbtools:::.R_ConInt(as.numeric(dat[,x]),base::as.numeric(dat[,y]), ndigits = ndigits)$CI
      ))
    base::diag(Mat_CI) = base::diag(Mat_N)
    Mat_N = Mat_CI
  }

  #Create Dataframe For ggplot to Read
  PlotMat = Mat_Cor
  if(includeN){
    PlotMat[base::lower.tri(PlotMat, diag=TRUE)]=Mat_N[base::lower.tri(Mat_N, diag=TRUE)]
  }
  if(!includeN){
    PlotMat[base::lower.tri(PlotMat, diag=TRUE)]=""
  }
  base::colnames(PlotMat) = Variables_Labels ;  base::rownames(PlotMat) = Variables_Labels

  PlotMat = base::data.frame(reshape2::melt(PlotMat), stringsAsFactors = FALSE)
  head(PlotMat)

  PlotMat$value = (base::as.character(PlotMat$value))
  PlotMat$ValueFill = base::as.numeric(t(c(Mat_Cor_fill)))
  PlotMat$Var2 = base::factor(PlotMat$Var2, levels=rev(base::levels(PlotMat$Var2)))


  OutPlot =
    ggplot2::ggplot(data = PlotMat, ggplot2::aes(x=Var1, y=Var2,fill=ValueFill))+# + geom_point(aes(size=value^2,alpha=value^4))+
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

  OutPlot

  return(OutPlot)
}
