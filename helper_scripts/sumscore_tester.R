rm(list=ls(all.names = TRUE))
devtools::load_all()
library(tidyverse)

# Test 1

gbtoolbox::sum_score(c(1,NA,3,4)) == mean(c(1,3,4))*4

# Test 2
set.seed(10)
df_test = mtcars[c(5,7:9)]

# Add missing data
df_test$qsec[c(5,7,10,12,30)] = NA
df_test[1,1:4] = NA
df_test[2,1:3] = NaN

# test 0 - should run without error
ss1=
df_test %>%
  select(1:4) %>%
  gbtoolbox::sum_score()

ss2=
df_test %>%
  rowwise() %>%
  mutate(ss = gbtoolbox::sum_score(c(drat, qsec, vs, am)))

identical(as.numeric(ss1), as.numeric(ss2$ss)) # first test (should be true)

is.na(ss1[1]) # second test (should be true)

ss1[2] == 4  # should also be true
ss1[3] == mean(c(3.85,18.61,1,1))*4  # should also be true



data.frame(ss2)


ability = rnorm(100)
df_test = sapply(1:10, function(x) ability + rnorm(100, mean = 10)) %>% as.matrix()
df_test[,6] = df_test[,6]*-1
df_test[sample(nrow(df_test)*ncol(df_test), 100, replace = FALSE)] = NA



gbtoolbox:::sum_score(df_test,  perc_missing_allowed = 1)




# function defaults

rm(list = ls(all.names = TRUE))
input=c(1,NA,3,5)
perc_missing_allowed=1
check_loadings = TRUE
# verbosity
plot_scores = FALSE
print_missing_table = FALSE
print_missing_each_input_var = FALSE




perc_missing <- rowSums(is.na(input)) / ncol(input)
