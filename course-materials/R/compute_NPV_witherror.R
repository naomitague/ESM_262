#' compute_NPV
#'
#' compute net present value
#' @param value/cost ($)
#' @param time in the future that cost/value occurs (years)
#' @param discount rate
#' @return value in $


compute_NPV = function(value, time, discount) {
  
  # check for positive discount
  if (discount < 0) {
    stop("Discount rate must be positive")
  }
  
  if (time < 0) {
   stop("Time must be positive")
  }
  
  if (value < 0) {
    warning("Value must be positive")
  }  
  
  # use value to compute NPV
  result = value / (1 + discount)**time
  return(result)
}

