rm(list=ls(all.names = TRUE))
devtools::load_all()

dat = mtcars[,c(1,2,7:11)]
# gbtoolbox:::plot_correlations(dat, abs_colour = FALSE)


variable_labels=NULL
textadjust=2
sample_size=TRUE
confidence_interval=TRUE
low_colour="#0072B2"
high_colour="#D55E00"
mid_colour="white"
abs_colour=FALSE
cluster_variables = FALSE
n_decimal_places = 2
suppress_warning_message = FALSE
