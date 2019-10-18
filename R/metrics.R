#' Maximum value reached
#'
#' x and y coordinates of first point of maximum y
#'
#' @export
metric_ymax <- function(x, y, ...) {
    y_max_i <- which.max(y)
    tibble::tibble(
        y_max_x = x[y_max_i],
        y_max_y = y[y_max_i]
    )
}

#' Maximum slope reached
#'
#' x,y coordinates of first point of maximum dy. slope (m), y intercept (b),
#' x intercept (y0_x) of tangent line
#'
#' @export
metric_dymax <- function(x, y, dy, ...) {
    dy_max_i <- which.max(dy)
    tibble::tibble(
        dy_max_x = x[dy_max_i],
        dy_max_y = y[dy_max_i],
        dy_max_m = dy[dy_max_i],
        #  y = mx + b for max slope line...
        # y intercept (y @ x = 0); b = y - mx
        dy_max_b = dy_max_y - (dy_max_m * dy_max_x),
        # x intecept (x @ y = 0); x = y - b / m
        dy_max_y0_x = 0 - dy_max_b / dy_max_m
    )
}

#' Area under the curve (integral of fit over range)
#'
#' @param fit model object with fitted values
#' @param limits <num> vector of length; lower and upper limits of integration
#'
#' @export
metric_auc <- function(fit, x, ...) UseMethod("metric_auc")

#' @export
metric_auc.smooth.spline <- function(fit, x, ...) {
    auc <- integrate(
        f = function(x) {p <- predict(object = fit, x = x); p$y},
        lower = min(x),
        upper = max(x)
    )
    tibble::tibble(fit_int = auc$value)
}

#' @export
metric_auc.nls <- function(fit, x, ...) {
    auc <- integrate(
        f = function(x) {
            p <- predict(fit, newdata = data.frame(x = x))
            p
        },
        lower = min(x),
        upper = max(x)
    )
    tibble::tibble(fit_int = auc$value)
}


#' coefficients of a model fit
#'
#' @param fit <obj> model object from which coefficients are extracted
#'
#' @export
metric_coefs <- function(fit, ...) {
    tibble::as_tibble(as.list(stats::coef(fit)))
}
