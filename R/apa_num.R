#' Format Numbers to APA Style
#'
#' This function formats a given numeric input to APA style. This involves rounding
#' the number to a specified number of decimal places. Any leading zeros before
#' the decimal point are also removed. It can optionally add a space before the decimal point for numbers
#' less than 1 and greater than -1, without a leading 0 or - sign.
#'
#' @param x A numeric vector. These are the numbers to be formatted.
#' @param n_decimal_places An integer specifying the number of decimal places to round to. Default is 2.
#' @param remove_leading_zeros A logical indicating whether to remove leading zeros. Default is TRUE.
#' @param add_space_start A logical indicating whether to add a space before the decimal point for numbers
#' less than 1 and greater than -1, without a leading 0 or - sign. Default is TRUE.
#' @param na_to_blank A logical indicating whether to convert NA values to blank strings. Default is TRUE.
#' @param blank_to_blank A logical indicating whether to convert blank strings to blank strings. Default is TRUE.
#'
#' @return A character vector of APA formatted numbers corresponding to 'x'.
#'
#' @export
#'
#' @examples
#' apa_num(-0.1) # returns "-.10"
#' apa_num(-100.12314) # returns "-100.12"
#' apa_num(0.123456) # returns " .12"
#' apa_num(0.123456, add_space_start = FALSE) # returns ".12"
#' apa_num(0.987654, n_decimal_places = 3) # returns " .988"
#' apa_num(c(-0.123456, 0.987654), n_decimal_places = 2) # returns "-.12" " .99"
#'
apa_num = function(x,
                   n_decimal_places = 2,
                   remove_leading_zeros = TRUE,
                   add_space_start = TRUE,
                   na_to_blank = TRUE,
                   blank_to_blank = TRUE
                   ){
  if (na_to_blank){ na_locations = which(is.na(x))}
  if (blank_to_blank){ blank_locations = which(x == "")}

  x = as.character(sprintf(paste0("%.",n_decimal_places,"f"), as.numeric(x)))
  if (remove_leading_zeros){ x = gsub("^(-?)0+\\.", "\\1.",x)}
  if (add_space_start){ x = gsub("^(\\.)", " \\1", x)}

  if (na_to_blank){ x[na_locations] = ""}
  if (blank_to_blank){ x[blank_locations] = ""}

  return(x)
}


