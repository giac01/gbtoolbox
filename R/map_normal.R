
#' map_normal - Map any continuous variable onto a Normal Distribution
#'
#' Transforms any numeric vector onto a continuous distribution.
#'
#' The function preserves the original order of values, but shifts the location of values so that the overall distribution maps perfectly onto a normal distribution.
#'
#' This works by first finding the percentile ranks (i.e. rank individuals between 0 and 1), and then use the inverse cumulative distribution function for a standard normal distribution to convert those ranks to the equivalent position on a normal distribution.
#'
#' Two additional tricks are required.
#'
#' First, after ranking values from 1 to N (N = the number of values), we minus 1/2 then divide by N to get percentile ranks.
#' This is to avoid having percentile ranks of 0% or 100% - as they map onto -/+ infinity!
#'
#' Second problem is dealing with tied values. The approach here is to initially sort tied values randomly, assign them all a unique normal score, then average across the normal scores for each tied value.
#' This yields a different result than first averaging the percentile rank, and finding a normal score from that.
#'
#' After transforming percentile ranks to the normal distribution, additional scaling and centering is performed to make sure the vector mean is 0 with unit variance.
#'
#' Warning 1: The default behavior is that the function will not transform dichotomous variables. It's generally only advisable to use this function for continuous variables with minimal repeated values.
#'
#' Warning 2: For linear regression your outcome variable does not need to be normally distributed! It may be helpful to use this function for predictor variables, but transforming outcomes may be inadvisable....
#'
#' @param x Numeric Vector that your want to map onto a Normal distribution.
#' @param MinDim Numeric Scalar (default = 2). If the number of unique values in a vector is equal to or less than MinDim, then no the function will not transform the variable. This is because it is inadvisable to use this function on dichotmous variables.
#' @param rescale Logical (default = TRUE). If true, will scale() the output to ensure a 0 mean and unit variance.
#'
#' @return Numeric Vector
#' @export
#'
map_normal = function(x, MinDim=2, rescale=TRUE){
  if (!is.numeric(x)) stop("Uh oh, make sure your input is a numeric vector!")

  #Ranks the kids on unique integers (for ties, randomly order these),then finds a average normal score
  percentilerank = (base::rank(x, na.last="keep", ties.method = "random")-.5)/(base::length(stats::na.omit(x)))
  normalscore = stats::qnorm(percentilerank)
  #average over z-scores with the same value.
  for(i in base::unique(x)){
    if(base::length(base::which(x==i))>1){
      normalscore[base::which(x==i)]=mean(normalscore[x==i],na.rm = TRUE)
    }
  }
  out = base::as.numeric(normalscore)

  if (rescale==TRUE){
    out = base::as.numeric(base::scale(out,center = TRUE, scale=TRUE))
  }

  #Minimum number of unique values in data allowed. This is so that we can avoid transforming binary data types or likert scales if we don't want them to be transformed
  ObservedNumberUniqueValues = base::length(base::unique(stats::na.omit(x)))
  if (ObservedNumberUniqueValues <= MinDim){
    out=x
  }

  return(out)
}

