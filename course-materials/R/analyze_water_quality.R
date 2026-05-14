#' analyze_water_quality
#'
#' Analyze water quality measurements against safety thresholds
#' and compute an overall water quality index
#' @param ph pH measurement (unitless, typically 0-14)
#' @param dissolved_oxygen dissolved oxygen concentration (mg/L)
#' @param turbidity water clarity measurement (NTU)
#' @param ph_min minimum acceptable pH (unitless, default 6.5)
#' @param ph_max maximum acceptable pH (unitless, default 8.5)
#' @param do_min minimum acceptable dissolved oxygen (mg/L, default 6.0)
#' @param turbidity_max maximum acceptable turbidity (NTU, default 4.0)
#' @return list containing ph_status, do_status, turbidity_status,
#'         water_quality_index (0-100), and overall_rating (Good/Moderate/Poor)

analyze_water_quality <- function(
    ph,
    dissolved_oxygen,
    turbidity,
    ph_min        = 6.5,
    ph_max        = 8.5,
    do_min        = 6.0,
    turbidity_max = 4.0
) {
  
  ph_status        <- ifelse(ph >= ph_min & ph <= ph_max, "Safe", "Unsafe")
  do_status        <- ifelse(dissolved_oxygen >= do_min, "Adequate", "Hypoxic")
  turbidity_status <- ifelse(turbidity <= turbidity_max, "Clear", "Turbid")
  
  water_quality_index <- round(
    (ph / ph_max * 40) + (dissolved_oxygen / 10 * 40) + ((10 - turbidity) / 10 * 20),
    1
  )
  
  overall <- ifelse(water_quality_index >= 70, "Good",
                    ifelse(water_quality_index >= 50, "Moderate", "Poor"))
  
  return(list(
    ph_status           = ph_status,
    do_status           = do_status,
    turbidity_status    = turbidity_status,
    water_quality_index = water_quality_index,
    overall_rating      = overall
  ))
}