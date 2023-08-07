
iris_test = iris

iris_test$Sepal.Length[sample(150, 20, replace = FALSE)] = NA


devtools::load_all()

gbtoolbox:::sumscore(iris[1:2], print_tables = FALSE)




# function defaults

df_input=df
vars = NULL
time = NULL
perc_missing_allowed=.2
recode_items = NULL
recode_function = NULL
plot_scores = TRUE
print_tables = TRUE
