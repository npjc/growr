#' @export
dy <- function(x) UseMethod("dy")

#' @export
dy.smooth.spline <- function(x) {
    dy <- stats::predict(x, deriv = 1)
    dy$y
}

#' @export
dy.nls <- function(x) {
    stats::predict(x, deriv = 1)
}
