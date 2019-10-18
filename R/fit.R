#' @export
fit_smooth_spline <- function(x, y, metric_funs = list(metric_ymax, metric_dymax, metric_auc), ...) {
    fit <- smooth.spline(x, y, cv = TRUE, ...)

    md <- broom::glance(fit)

    od <- broom::augment(fit)
    od$.dy <- dy(fit)

    cd <- purrr::map_dfc(
        metric_funs,
        ~.x(x = od$x, y = od$.fitted, dy = od$.dy, fit = fit)
        )

    list(
        model = md,
        components = cd,
        observations = od
        )
}

#' @export
fit_nls <- function(x, y, formula, start, metric_funs = list(metric_coefs, metric_ymax, metric_dymax, metric_auc), ...) {
    data <- data.frame(x = x, y = y)
    fit <- nls(formula = formula, start = start, data = data)

    md <- broom::glance(fit)

    od <- broom::augment(fit)
    od$.dy <- dy(fit)


    cd <- purrr::map_dfc(
        metric_funs,
        ~.x(x = od$x, y = od$.fitted, dy = od$.dy, fit = fit)
    )

    list(
        model = md,
        components = cd,
        observations = od
    )

}

#' @export
fit_nls_gompertz <- function(x, y, metric_funs = list(metric_coefs, metric_ymax, metric_dymax, metric_auc), ...) {

    formula <- y ~ A * exp(-exp(mu * exp(1) * (lambda - x) / A + 1.0))

    start <- nls_start_gompertz(x, y)

    fit_nls(x = x, y = y, formula = formula, start = start, metric_funs = metric_funs, ...)
}

nls_start_gompertz <- function(x, y) {
    ss <- fit_smooth_spline(x, y)$components
    list(
        A = ss$y_max_y,
        mu = ss$dy_max_m,
        lambda = ss$dy_max_y0_x)
}

#' @export
fit_nls_richards <- function(x, y, metric_funs = list(metric_coefs, metric_ymax, metric_dymax, metric_auc), ...) {

    formula <- y ~ A*(1.0 + nu*exp(1 + nu)*exp(mu*(1 + nu)^(1 + 1/nu)*(lambda - x)/A))^(-1/nu)

    start <- nls_start_richards(x, y)

    fit_nls(x = x, y = y, formula = formula, start = start, metric_funs = metric_funs, ...)
}

nls_start_richards <- function(x, y) {
    gompertz_start <- nls_start_gompertz(x, y)
    c(gompertz_start, list(nu = 0.1))
}


# nls: logistic -----------------------------------------------------------

#' @export
fit_nls_logistic <- function(x, y, metric_funs = list(metric_coefs, metric_ymax, metric_dymax, metric_auc), ...) {

    formula <- y ~ A / (1 + exp(4 * mu * (lambda - x) / A + 2))

    start <- nls_start_logistic(x, y)

    fit_nls(x = x, y = y, formula = formula, start = start, metric_funs = metric_funs, ...)
}

nls_start_logistic <- function(x, y) {
    nls_start_gompertz(x, y)
}


