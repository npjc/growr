#' @export
summarise_fit <- function(x, y, method = "smooth.spline") {
    data <- tibble::tibble(time = x, y = y)
    f <- switch (method,
                 "smooth.spline" = fit_smooth_spline,
                 "gompertz" = fit_gompertz,
                 "richards" = fit_richards,
                 "logistic" = fit_logistic
    )
    res <- f(data)
    tibble::as_tibble(res[c("A", "mu", "lambda")])
}

