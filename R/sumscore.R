
sumscore = function(df_input=df, vars = NULL, time = NULL, perc_missing_allowed=.2, recode_items = NULL, recode_function = NULL, plot_scores = TRUE){

  perc_missing = apply(df, 1, function(x) length(which(is.na(x)))/length(x))

  print(table(perc_missing))
  print(apply(df, 2,table))

  if (!is.null(recode_items)){
    df[recode_items] = apply(df[recode_items], 2, recode_function)
  }

  item_loadings = stats::princomp(stats::cor(df, use = "pairwise.complete.obs"))$loadings[,1]

  if (length(which(item_loadings<0))>0){cat(c("\n\nError, negative items loadings:", which(item_loadings<0)))}

  scores = apply(df, 1, function(x) mean(x, na.rm=TRUE)*ncol(df))

  scores[perc_missing>perc_missing_allowed] = NA

  if (plot_scores) hist(scores)

  return(scores)
}



