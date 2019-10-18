# init_smooth_spline <- function(x, y, ...) {
#     fit <- smooth.spline(x, y, cv = TRUE)
#     deriv1 <- predict(fit, deriv = 1)
#     slope_max_pos <- which.max(deriv1$y)
#     x_at_slope_max <- x[slope_max_pos]
#     y_at_slope_max <- y[slope_max_pos]
#     dy_at_slope_max <- deriv1$y[slope_max_pos]
#     # area under curve, aka efficiency/total growth:
#     f <- function(x){
#         p <- predict(fit, x)
#         p$y
#     }
#     mu = dy_at_slope_max
#     b = y_at_slope_max - (dy_at_slope_max * x_at_slope_max)
#     lambda = -b / mu
#     integral <- integrate(f,min(x),max(x))$value
#     list(
#         mu = mu,
#         b = b,
#         lambda = lambda,
#         A = max(y),
#         integral = integral,
#         fit = fit
#     )
# }
#
# fit_smooth_spline <- function(data) {
#     init_smooth_spline(data$time, data$y)
# }

# gompertz ----------------------------------------------------------------

# init_gf_gompertz <- function(mCall, data, LHS) {
#     pars <- init_smooth_spline(data$x, data$y)
#     setNames(pars[c("A", "mu", "lambda")], nm = mCall[c("A", "mu", "lambda")])
# }


# ss_gf_gompertz <- selfStart(
#     ~A * exp(-exp(mu * exp(1) * (lambda - x) / A + 1.0)),
#     initial = init_gf_gompertz,
#     parameters = c("A", "mu", "lambda")
# )

# fit_gompertz <- function(data) {
#     fit <- nls(y ~ ss_gf_gompertz(x, A, mu, lambda), data = data)
#     coefs <- stats::coef(fit)
#
#     list(
#         mu = coefs[['mu']],
#         lambda = coefs[['lambda']],
#         A = coefs[['A']],
#         fit = fit
#     )
# }

# richards ----------------------------------------------------------------

#' init_gf_richards <- function(mCall, data, LHS) {
#'     pars <- init_smooth_spline(data$time, data$y)
#'     pars <- c(pars[c("A", "mu", "lambda")], nu = 0.1)
#'     setNames(pars, nm = mCall[c("A", "mu", "lambda", "nu")])
#' }
#'
#' #' @export
#' ss_gf_richards <- selfStart(
#'     ~A*(1.0 + nu*exp(1 + nu)*exp(mu*(1 + nu)^(1 + 1/nu)*(lambda - time)/A))^(-1/nu),
#'     initial = init_gf_richards,
#'     parameters = c("A", "mu", "lambda", "nu")
#' )
#'
#' fit_richards <- function(data) {
#'     fit <- stats::nls(y ~ ss_gf_richards(time, A, mu, lambda, nu), data = data)
#'     coefs <- stats::coef(fit)
#'
#'     list(
#'         mu = coefs[['mu']],
#'         lambda = coefs[['lambda']],
#'         A = coefs[['A']],
#'         fit = fit
#'     )
#' }


# logistic ----------------------------------------------------------------

#' init_gf_logistic <- function(mCall, data, LHS) {
#'     pars <- init_smooth_spline(data$time, data$y)
#'     setNames(pars[c("A", "mu", "lambda")], nm = mCall[c("A", "mu", "lambda")])
#' }
#'
#' #' @export
#' ss_gf_logistic <- selfStart(
#'     ~A / (1 + exp(4 * mu * (lambda - time) / A + 2)),
#'     initial = init_gf_logistic,
#'     parameters = c("A", "mu", "lambda")
#' )
#'
#' fit_logistic <- function(data) {
#'     fit <- stats::nls(y ~ ss_gf_logistic(time, A, mu, lambda), data = data)
#'     coefs <- stats::coef(fit)
#'
#'     list(
#'         mu = coefs[['mu']],
#'         lambda = coefs[['lambda']],
#'         A = coefs[['A']],
#'         fit = fit
#'     )
#' }


# gompertz exp ------------------------------------------------------------

# init_gf_gompertz_exp <- function(mCall, data, LHS) {
#     pars <- init_smooth_spline(data$time, data$y)
#     pars <- pars[c("A", "mu", "lambda")]
#     pars <- c(pars, list(alpha = 0.001, tshift = 0))
#     pars <- setNames(pars, nm = mCall[c("A", "mu", "lambda", "alpha", "tshift")])
#     dput(pars)
#     pars
# }
#
# ss_gf_gompertz_exp <- selfStart(
#     ~A * exp(-exp(mu * exp(1) * (lambda - time) / A + 1.0)) + A * exp(alpha * (time - tshift)),
#     initial = init_gf_gompertz_exp,
#     parameters = c("A", "mu", "lambda", "alpha", "tshift")
# )
#
# fit_gompertz_exp <- function(data) {
#     nls(y ~ ss_gf_gompertz_exp(time, A, mu, lambda, alpha, tshift), data = data)
# }
