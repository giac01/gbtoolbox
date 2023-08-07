
sumscore = function(df_input=NULL,
                    vars = NULL,
                    time = NULL,
                    perc_missing_allowed=.2,
                    recode_items = NULL,
                    recode_function = NULL,
                    # verbosity
                    plot_scores = TRUE,
                    print_missing_table = TRUE,
                    print_missing_each_input_var = FALSE
                    ){

  perc_missing = apply(df_input, 1, function(x) length(which(is.na(x)))/length(x))

  if (print_missing_table){
    perc_missing_table =  table(perc_missing)
    names(perc_missing_table) = gbtoolbox:::apa_num(as.numeric(names(perc_missing_table)))
    cat("Percent missing on the calculated sumscore variable:\n")
    print(perc_missing_table)
    cat("\n")
  }


  if (print_missing_each_input_var) print(apply(df_input, 2,table))


  if (!is.null(recode_items)){
    df_input[recode_items] = apply(df_input[recode_items], 2, recode_function)
  }

  # item_loadings = stats::princomp(stats::cor(df_input, use = "pairwise.complete.obs"))$loadings[,1]
  item_loadings = stats::loadings(stats::princomp(covmat = stats::cov(df_input, use = "pairwise.complete.obs"), fix_sign = TRUE, cor = TRUE, scores = FALSE) )[,1] # Need to check this later!

  if (length(which(item_loadings<0))>0){cat(c("\n\nError, negative items loadings:", which(item_loadings<0),"\n\n"))}

  scores = apply(df_input, 1, function(x) mean(x, na.rm=TRUE)*ncol(df_input))

  scores[perc_missing>perc_missing_allowed] = NA

  if (plot_scores) hist(scores)

  return(scores)
}



