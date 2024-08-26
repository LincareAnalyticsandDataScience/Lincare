#' Number_Format
#'
#' This function converts a Number to a character string with leading zeros
#'
#' @param x Numeric or field that be coerced to Numeric
#' @param digits How many digits should the field contain?
#'
#' @return A Vector containing the original number with leading zeros (nchar of output values should always be equal to the digits parameter)
#'
#' @examples Number_Format(x = 8, digits = 2)
#' [1] "08"
#'
#' @export
Number_Format <- function(x, digits){
  x <- as.numeric(x)
  x_len <- nchar(x)

  if(x_len == digits) return(as.character(x))

  return(as.character(paste0(paste0(rep(x = "0",
                                        digits - x_len),
                                    collapse = ""),
                             x)))

}
