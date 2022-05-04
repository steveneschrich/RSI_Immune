#' Draw a half-circle.
#'
#' Function to create a half-circle.
#' @details
#' Rather than plot an entire circle, this function will plot a half-circle in a manner
#' similar to grid.circle. You can specify which half to show through the normal specifications
#' of left, right, top, bottom. 
#' 
#' The gp parameter requires a gpar and can be used to set typical graphical components
#' such as color and fill. 
#'
#' This function was inspired by
#'     vp <- viewport(width=0.5, height=0.5, clip  = "on")
#'     grid.circle(0.5,0,r=0.5, gp = gpar(fill = 'red'), vp = vp)
#'  at https://stackoverflow.com/questions/31538534/plotting-half-circles-in-r
#'
#'
#' The goal of this code is to draw two half-circles colored differently
#' (according to data). We use grid graphics for this purpose.
#'
#' The first step is to define a new "viewport" which is the extents of
#' the cell given to us in the function (e.g., x,y,width,height). These
#' are always the same. What changes is the viewport relative to the coordinates.
#'
#' So for instance, in the first case we put the coordinates of x,y at the
#' left, center of the viewport. 
#' |---|
#' |*--|
#' |---|
#' Then plot a circle centered at x=0, y=0.5 (relative to viewport) with
#' clipping on. It will only get the right half of the circle, centered in the
#' middle of the area.
#'
#' Likewise, if we define right, center
#' |---|
#' |--*|
#' |---|
#' Then plot a circle at x=1, y=0.5 (relative to viewport) with clipping on,
#' we get a left-half of the circle.
#'
#' @param x A numeric vector or unit object specifying x-location.
#' @param y A numeric vector or unit object specifying y-location.
#' @param r A numeric vector or unit object specifying radius.
#' @param r An object of class gpar, typically the output from a call to 
#'     the function gpar. This is basically a list of graphical parameter settings.
#' @param ... Any other parameters to pass to grid.circle
#'
#' @return
#' @export
#'
grid.half_circle <- function(x, y, r, which="left", gp=gpar(fill="white"), ...) {
  
  circle_settings <- tibble::tribble(
    ~align, ~x_just, ~y_just, ~x_center, ~y_center,
    "left", "right", "center", 1, 0.5,
    "right", "left", "center", 0, 0.5,
    "top", "center","bottom", 0.5, 0,
    "bottom", "center", "top", 0.5, 1,
  )
  cparam<-circle_settings %>% dplyr::filter(align==which)
  
  # Not sure this is ok to do, since it is an object (but really a list)
  gp$lwd <- 0.5
  
  # The trick is to create a viewport to plot onto, where clipping is enabled.
  # We draw the entire circle, but half of it is clipped.
  vp<-viewport(x, y, width = 2*r, height = 2*r,
               just = c(cparam$x_just, cparam$y_just),
               clip="on"
  )
  grid.circle(cparam$x_center, cparam$y_center, r = 0.45, gp = gp, vp = vp, ...)
  
}
