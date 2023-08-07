
iris_test = iris

iris_test$Sepal.Length[sample(150, 20, replace = FALSE)] = NA

ability = rnorm(100)
df_test = sapply(1:10, function(x) ability + rnorm(100, mean = 10)) %>% as.matrix()
df_test[,6] = df_test[,6]*-1
# df_test[sample(nrow(df_test)*ncol(df_test), 100, replace = FALSE)] = NA

devtools::load_all()

gbtoolbox:::sumscore(df_test, print_tables = FALSE, perc_missing_allowed = 1)


gbtoolbox:::sumscore(df_test, print_tables = FALSE, perc_missing_allowed = 1)

# function defaults

df_input = df_test
vars = NULL
time = NULL
perc_missing_allowed=.2
recode_items = NULL
recode_function = NULL
plot_scores = TRUE
print_tables = TRUE


x = prcomp(cov(df_input, use = "pairwise.complete.obs"), rank. =10, scale. = TRUE)


summary(x)
x$rotation

psych::pca(df_input, nfactors = 1, rotate = "none")

stats::princomp(df_input)$loadings


psych::fa.parallel(df_test)


psych::pca(df_input, nfactors = 1, rotate = "none")


princomp(cor(df_test), fix_sign = TRUE) %>% loadings()
