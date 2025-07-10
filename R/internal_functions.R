.isBinaryVariable = function(x){

  return(2==base::length(base::table(x,useNA="no")))

}

# Converts into binary variable (0, 1)

.makeBinary = function(x){

  return(base::as.numeric(x>base::min(x, na.rm = TRUE)))

}

.inverseLogit = function(x){
  return(1/(1+exp(-1*(x))))
}


# Coefficient of Determination Estimation
## https://en.wikipedia.org/wiki/Coefficient_of_determination

.R2 = function(X,Y){
  if (nrow(X)!=nrow(Y)) stop(".R2 Fail; nrow don't match!!")
  Res = Y - X #Matrix of residuals
  Yt = base::apply(base::as.matrix(Y), 2, function(x) x - mean(x, na.rm=TRUE) )

  SSres =  base::crossprod(Res, Res)        # Residual Sums of Squares
  SStot =  base::crossprod(Yt, Yt)          # Y (total) Sums of Squares

  R2 = base::diag(1 - (SSres/SStot))

  return(R2)
}


# Add intercept to matrix
.addintercept = function(matrix){
  intercept = rep(1, nrow(matrix)) # add intercept column to training data

  return(
    (
      cbind(
        intercept,
        matrix
      )
    )
  )

}

#Slower version, but does not require full rank matrices?
.R2quickcalc_v2 = function(X,Y){
  # browser()
  intercept = base::rep(1,nrow(X))
  X = base::cbind(intercept, X)
  out1= stats::.lm.fit(x=X,y=Y)
  Y = as.numeric(Y)
  return(1- sum(out1$residuals^2)/sum((Y-mean(Y))^2))
}

# Estimate p=value from Bootstrap distribution ---------------------------------
.boot_pval = function(x, null_val=0){
  x = stats::na.omit(x)
  perc = length(which(x<null_val))/length(x)
  p_val = 1-abs(.50-perc)*2
  return(p_val)
}


# Pearson Correlation Confidence Interval Calculator  --------------------------

.CorrelationCIEstimator = function(r, n, alpha=0.05){
  Fr = base::atanh(r)                 # Fisher Z Transform
  SE = 1/((n-3)^.5)             # Standard Error
  CI = stats::qnorm(c(alpha/2,1-alpha/2), mean=Fr, sd=SE)
  CI = base::tanh(CI)
  # p  = (1-pnorm(abs(Fr), mean=0, sd=SE))*2    # Fisher Z P value
  t = r*base::sqrt((n-2)/(1-r^2))       # P-value estimated from t-distribution
  p  = (1-stats::pt(base::abs(t), df=n-2))*2
  return(base::list(CI=CI,p=p))
}

# Pearson Correlation P-value Calculator  --------------------------

.correlation_pvalue = function(r, n, alpha=0.05){
  Fr = base::atanh(r)                 # Fisher Z Transform
  SE = 1/((n-3)^.5)             # Standard Error
  t = r*base::sqrt((n-2)/(1-r^2))       # P-value estimated from t-distribution
  p  = (1-stats::pt(base::abs(t), df=n-2))*2
  return(p)
}


# Sort variables ---------------------------------------------------------------


.sortVar = function(dat){

  cormat = 1 - base::abs(stats::cor(dat, use="pairwise.complete.obs"))
  cormat = stats::as.dist(cormat)

  if(length(which(is.na(c(t(cormat)))))>0){
    warning("Clustering failed due to an NA in the correlation matrix. Clustering not used. \nCheck stats::cor(your_data, use=`pairwise.complete.obs`)\nOr set cluster_variables = FALSE")
    clustres_order = 1:ncol(dat)
  } else {
    clustres = stats::hclust(cormat)
    clustres_order = clustres$order
  }

  return(clustres_order)
}


# Confidence Interval Function Required for PlotCorrelationMatrix --------------

.R_ConInt = function(
    x,
    y,
    n_decimal_places = 2){
  # browser()
  if(base::is.na(stats::cor(x,y, use="pairwise.complete.obs"))){
    # return(list(CI="0", N=nrow(na.omit(cbind.data.frame(x,y))), CI_l="0"))
    return(base::list(CI="", N="", CI_l=""))

  } else {
    CI = stats::cor.test(x,y)$conf.int[1:2]
    # CI = base::gsub("^\\s","",gsub("0\\.", "\\.",round(CI, digits=n_decimal_places)))
    CI = gbtoolbox::apa_num(CI, n_decimal_places = n_decimal_places)
    N = base::as.numeric(stats::cor.test(x,y)$parameter + 2)
    CI_l =     base::paste0( "[",CI[1],", ",CI[2],"]", sep="")
    out =    base::paste0(N,"\n[",CI[1],", ",CI[2],"]", sep="")
    return(list(CI=out, N=N, CI_l=CI_l))
  }
}


.swapsies = function(x, input, replacement){
  return(replacement[match(x, input)])
}

recode_check = function(x, input, output, verbose = TRUE){

  before=c(t(x))
  after = .swapsies(before, input, output)

  if (verbose){
    print(table(before))
    print(table(after))
  }

  return(after)

}

# Correlation Missingness Calculaton -------------------------------------------

missingness_correlations = function(dat){
  # input data

  # matrix of missingness (1 = missing, 0 = not missing)
  dat_missing = apply(dat, 2, function(x) as.numeric(is.na(x)))

  vars = colnames(dat)

  missingness_cors =
  sapply(vars, function(v_1)
    sapply(vars, function(v_2)
      suppressWarnings(
        stats::cor(
            dat_missing[,v_2],
            dat[,v_1],
            use = "pairwise.complete.obs"))
      )
    )

  sample_size_matrix = sapply(vars, function(x)
    sapply(vars, function(y)
      base::nrow(stats::na.omit(base::data.frame(dat[,base::unique(c(x,y))])))
    ))


  missingness_cors_pvals = sapply(seq_along(vars) ,function(c)
    sapply(seq_along(vars), function(r)
      .correlation_pvalue(r = missingness_cors[r,c], n = sample_size_matrix[c,c])
    ))

  output = list("cor" = missingness_cors, "p.value" = missingness_cors_pvals,
                "sample.size" = sample_size_matrix)
  return(output)
}


sapply(paste(1:10), function(x)
   sapply( paste(letters[1:4]), function (y) paste(x,y)
  ))
