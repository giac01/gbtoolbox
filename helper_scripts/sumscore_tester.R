iris_test = iris

iris_test$Sepal.Length[sample(150, 20, replace = FALSE)] = NA


iris_test %>%
  mutate()

devtools::load_all()

gbtoolbox:::sumscore(iris[1:2])




# function defaults

df_input=df
vars = NULL
time = NULL
perc_missing_allowed=.2
recode_items = NULL
recode_function = NULL
plot_scores = TRUE
