.sortVar = function(dat){

  cormat = 1 - base::abs(stats::cor(dat, use="pairwise.complete.obs"))
  cormat = stats::as.dist(cormat)
  clustres = stats::hclust(cormat)

  return(clustres$order)
}


# Confidence Interval Function Required for PlotCorrelationMatrix
.R_ConInt = function(x,y, ndigits = 2){
  # browser()
  if(base::is.na(stats::cor(x,y, use="pairwise.complete.obs"))){
    # return(list(CI="0", N=nrow(na.omit(cbind.data.frame(x,y))), CI_l="0"))
    return(base::list(CI="0", N="0", CI_l="0"))

  } else {
    CI = stats::cor.test(x,y)$conf.int[1:2]
    CI = base::gsub("^\\s","",gsub("0\\.", "\\.",round(CI, digits=ndigits)))
    N = base::as.numeric(cor.test(x,y)$parameter + 2)
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




