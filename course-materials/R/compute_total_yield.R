#' Compute_total_yield
#'
#' Computed the total yield from a vectors of fertilizer inputs and precipitation
#' for each yeild
#'
#' @param fert fertilizer kg/ha
#' @param TP total precipitation (cm)
#' @param coeff1 coefficient on fertilizer
#' @param coeff2 coefficient 2 on fertilizer equation
#' @param pcoeff coeffcient on precipitation
#
#' @return total yield (kg)

compute_total_yield = function(fert, TP, coeff1=1.8, coeff2=-0.5, pcoeff=0.1) {

  # for negative fertilizer we assume this is an error an stop the function
  fert = ifelse(fert < 0, stop("fertilizer cannot be negative"), fert)

  # if TP is negative return an error
  if (TP < 0) stop("Total Precipitation (TP) cannot be negative")
  # if its zero we still will computed yield but warn the user
  if (TP == 0) warning("Total Precipitation is zero")
  yield = coeff1*fert^2-coeff2* fert + pcoeff*TP
  ty = sum(yield)

  return(ty)
}
