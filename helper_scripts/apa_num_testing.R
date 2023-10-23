
Mat_Cor = matrix(sprintf(paste0("%.",n_decimal_places,"f"), Mat_Cor), ncol = ncol(Mat_Cor))
Mat_Cor = apply(Mat_Cor,2, function(x) gsub("^(-?)0+\\.", "\\1.",x))
devtools::load_all()

x = c(1, .5, .213,-2134.124, -.123124,NA,"",4,2)
apa_num(x, remove_leading_zeros = TRUE)

