#' Compute a color gradient
#' 
#' Given a vector of scores, compute a color gradient representing the
#' range of values.
#' 
#' @details 
#' ComplexHeatmap provides a nice approach to color gradient for heatmaps and
#' annotations. It selects colors somewhat randomly, so in order to get a 
#' fixed color gradient a color function must be defined. This function
#' provides a color function (from colorRamp2) that represents that color
#' gradient. Currently, the idea is to provide the `score` vector, a 
#' `min` color and a `max` color. 
#' 
#' The actual computation is 1% and 99% values, with the intermediate value (white)
#' being interpolated using seq(). This is how ComplexHeatmap computes the gradient.
#' 
#' @param score A vector of scores to represent.
#' @param min Color for minimum value
#' @param mid Color for middle value
#' @param max Color for maximum value
color_gradient <- function(score, min = "blue", mid="#EEEEEE", max = "red") {
  circlize::colorRamp2(
    breaks =
      seq(quantile(score, 0.01),
          quantile(score, 0.99),
          length=3),
    colors = c(min, mid, max)
  )
}
