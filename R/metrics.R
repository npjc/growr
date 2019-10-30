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

#' Minimum Doubling or Generation Time
#'
#' Finds the point of maximum growth rate then treating that as the midpoint
#' of a doubling period, computes the doubling time at point.
#'
#' @export
metric_min_dbl <- function(x, y, dy, ...) {
    i <- which.max(dy)
    m <- dy[i]
    midpoint_x = x[i]

    midpoint <- y[i]
    y2 <- midpoint * sqrt(2)
    y1 <- midpoint * sqrt(.5)

    # m = ∆y / ∆x; ∆x = ∆y / m = (y2 - y1) / m
    xdbl <- (y2 - y1) / m

    tibble::tibble(
        min_dbl = xdbl,
        min_dbl_midpoint_y = midpoint,
        min_dbl_midpoint_x = midpoint_x,
        min_dbl_y1 = y1,
        min_dbl_y2 = y2
    )
}

#' Average Doubling or Generation Time from start until max rate.
#'
#' x,y coordinates of first point of maximum dy. slope (m), y intercept (b),
#' x intercept (y0_x) of tangent line
#'
#' Finds the point of maximum growth rate then treating that as the end point,
#' computes the average
#'
#' @keywords internal
metric_avg_dbl <- function(x, y, dy, ...) {
    i <- which.max(dy)
    m <- dy[i]
    dy_max_x <- x[i]
    dy_max_y <- y[i]
    y0 <- min(y)
    x0 <- min(x)

    avg_m = (dy_max_y - y0) / (dy_max_x - x0)
    n_dbl = log2(dy_max_y / y0)
    avg_dbl = n_dbl / avg_m

    tibble::tibble(
        avg_dbl = avg_dbl,
        avg_dbl_y1 = y0,
        avg_dbl_y2 = dy_max_y,
        avg_dbl_x1 = x0,
        avg_dbl_x2 = dy_max_x,
        avg_dbl_n = n_dbl
    )
}

