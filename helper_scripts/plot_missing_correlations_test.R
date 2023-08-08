rm(list=ls(all.names = TRUE))
devtools::load_all()

# Test 1 -------------------------------------------------------------------------

set.seed(10)
n <- 1000
df <- data.frame(
  age = rnorm(n, 50, 10),
  income = rnorm(n, 50000, 15000),
  satisfaction = runif(n, 1, 5),
  education = rnorm(n, 16, 2),
  health = runif(n, 0, 1),
  employment = rep(NA, n)
)

# Define a logistic function to create a probability of missingness
# We use the sigmoid function here (logistic function) to map income to a probability between 0 and 1
logistic <- function(x) 1 / (1 + exp(-x))

# Generate missingness in the "age" variable based on "income"
# We scale income to avoid too large or too small values in the logistic function
scaled_income <- scale(df$income)
prob_missing <- logistic(scaled_income)
df$age[runif(n) < prob_missing] <- NA

# Repeat similar process for other variables
# For "satisfaction", we make missingness depend on "education"
scaled_education <- scale(df$education)
prob_missing <- logistic(scaled_education)
df$satisfaction[runif(n) < prob_missing] <- NA

# For "health", we make missingness depend on "age" (including its missingness)
age_without_na <- ifelse(is.na(df$age), 0, df$age)
scaled_age <- scale(age_without_na)
prob_missing <- logistic(scaled_age)
df$health[runif(n) < prob_missing] <- NA

rm(logistic)

dat = df

gbtoolbox::plot_missing_correlations(dat)

# Test 2 -----------------------------------------------------------------------


iris_test = iris

iris_test$Sepal.Length[sample(150, 20, replace = FALSE)] = NA
iris_test$Sepal.Width[sample(150, 20, replace = FALSE)] = NA
iris_test$Petal.Width[sample(150, 100, replace = FALSE)] = NA

dat = iris_test[1:4]

gbtoolbox::plot_missing_correlations(iris_test[1:4], p_threshold_col = .06)

cor.test(as.numeric(is.na(iris_test$Sepal.Length)), iris_test$Sepal.Width)

cor.test(as.numeric(is.na(iris_test$Sepal.Width)), iris_test$Petal.Width)



# Other ------------------------------------------------------------------------



missingness_correlations(dat)$cor


cor(is.na(dat[,"age"]),dat[,"income"], use = "pairwise.complete.obs")
cor(is.na(dat[,"satisfaction"]),dat[,"age"], use = "pairwise.complete.obs")



variable_labels=NULL
textadjust=2
sample_size=TRUE
confidence_interval=TRUE
low_colour="#0072B2"
high_colour="#D55E00"
mid_colour="white"
abs_colour=TRUE
set_colour_to_p_value = FALSE
cluster_variables = FALSE
n_decimal_places = 3
p_threshold_col = .05
suppress_warning_message = FALSE


dat <- data.frame(A = rnorm(100), B = rnorm(100), C = rnorm(100))
dat$A[sample(1:100, 20)] <- NA
dat$B[sample(1:100, 30)] <- NA
dat$C[sample(1:100, 30)] <- NA

plot_missing_correlations(data.frame(dat))
#'
