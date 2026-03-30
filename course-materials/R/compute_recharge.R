#' effective_rain
#'
#' compute recharge from precipitation inputs
#' @param rainfall (mm/day)
#' @param interception_loss (mm) (default 1)
#' @return totalrecharge totalprecipitation
#'

effective_rain=function(rain, interception_loss=1) {

# easy part - get total rain
totalrain = sum(rain)

# interception happens everyday so we need a new vector
recharge = rain-interception_loss
recharge = pmax(recharge, 0)
totalrecharge = sum(recharge)

return(list(totalrain=totalrain, totalrecharge=totalrecharge))}


