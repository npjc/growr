# Package: grofit ----------------------------------------------------------------

#' Compute the output of a model from the time and specified parameters.
#'
#' @details
#' Gompertz model parametrized by `A`, `mu`, `lambda`:
#'
#' @param time   `<num>` Time points (x-axes) for which the function values will be returned.
#' @param A      `<num>` Maximum of the curve.
#' @param mu     `<num>` Maximum slope.
#' @param lambda `<num>` Lag phase
#'
#' @return y-values of the specified model for given time points.
#'
#' @rdname grofit
#' @export
gf_gompertz <- function(time, A, mu, lambda) {
    A * exp(-exp(mu * exp(1) * (lambda - time) / A + 1.0))
}

#' @rdname grofit
#' @param nu `<num>` shape parameter, enables flexible adjustment that the point of inflexion can be at any value between zero and A.
#' @export
gf_richards <- function(time, A, mu, lambda, nu = 0.1){
    A*(1.0 + nu*exp(1 + nu)*exp(mu*(1 + nu)^(1 + 1/nu)*(lambda - time)/A))^(-1/nu)
}

#' @rdname grofit
#' @export
gf_logistic <- function(time, A, mu,lambda) {
    A / (1 + exp(4 * mu * (lambda - time) / A + 2))
}

#' @rdname grofit
#' @param alpha `<num>` control the strength (slope) of the second increase.
#' @param tshift `<num>` control the location (time) of the second increase after the function enters a first saturation plateau
#' @export
gf_gompertz_exp <- function(time, A, mu, lambda, alpha = 0.1, tshift = max(time) / 10) {
    A * exp(-exp(mu * exp(1) * (lambda - time) / A + 1.0)) + A * exp(alfa * (time - tshift))
}

#' @rdname grofit
#' @param n `<num>` number of cells or proxy for number of cells
#' @export
gf_spline <- function(time, n) {
    smooth.spline(x = time, y = n)
}


# Package: growthcurver ---------------------------------------------------

#' fit logistic with parameter description as per growthcurver package
#'
#' @param t time
#' @param k the maximum possible population size in a particular environment, or the carrying capacity
#' @param n0 the population size at the beginning of the growth curve
#' @param r  The intrinsic growth rate of the population, r, is the growth rate that would occur if there were no restrictions imposed on total population size.
#'
#' @return y-values of the specified model for given time points.
#' @export
#'
#' @examples
#' gc_logistic(1:95, 10, .1, .2)
gc_logistic <- function(t, k, n0, r) {
    k / (1 + ((k - n0) / n0) * exp(-r * t))
}


# Package: growthmodels ---------------------------------------------------

#' Compute model output given time and parameters.
#'
#' @details
#' Blumberg growth model:
#' \deqn{ y(t) = ...}
#'
#' @param t time
#' @param alpha upper asymptote
#' @param w0 a reference value at t = t0, the value at t = 0
#' @param m slope of growth
#' @param t0 time shift (default 0)
#'
#' @return y-values of the specified model for given time points.
#'
#' @examples
#' gm_blumberg(0:10, 10, 2, 0.5)
#'
#' @author Daniel Rodriguez
#'
#' @references
#' M. M. Kaps, W. O. W. Herring, and W. R. W. Lamberson, "Genetic and
#' environmental parameters for traits derived from the Brody growth curve and
#' their relationships with weaning weight in Angus cattle.," Journal of
#' Animal Science, vol. 78, no. 6, pp. 1436-1442, May 2000.
#'
#' D. Fekedulegn, M. Mac Siurtain, and J. Colbert, "Parameter estimation of
#' nonlinear growth models in forestry," Silva Fennica, vol. 33, no. 4, pp.
#' 327-336, 1999.
#'
#' Michael J. Panik, "Growth Curve Modeling: Theory and Applications",
#' John Wiley & Sons, December 2013.
#'
#' A. Khamiz, Z. Ismail, and A. T. Muhammad, "Nonlinear growth models for
#' modeling oil palm yield growth," Journal of Mathematics and Statistics,
#' vol. 1, no. 3, p. 225, 2005.
#'
#' @rdname growthmodels
#' @export
gm_blumberg <- function(t, alpha, w0, m, t0 = 0) {
    (alpha * (t + t0)^m) / (w0 + (t + t0)^m)
}

#' @details
#' Brody growth model:
#' \deqn{ y(t) = \alpha - (\alpha - w_0) exp(- k t) }{ y(t) = \alpha - (\alpha - w_0) * exp(- k * t) }
#'
#' @param k growth rate
#'
#' @examples
#' gm_brody(0:10, 10, 5, 0.3)
#'
#' @rdname growthmodels
#' @export
gm_brody <- function(t, alpha, w0, k) {
    alpha - (alpha - w0) * exp(-k * t)
}

#' @details
#' Chapman-Richards growth model:
#' \deqn{ y(t) = \alpha (1 - \beta exp(-k t)^{1/(1-m)}) }{ y(t) = \alpha * (1 - \beta * exp(-k * t)^{1/(1-m)}) }
#'
#' @param beta growth range
#'
#' @examples
#' gm_chapmanRichards(0:10, 10, 0.5, 0.3, 0.5)
#'
#' @rdname growthmodels
#' @export
gm_chapmanRichards <- function(t, alpha, beta, k, m) {
    alpha * (1 - beta * exp(-k * t))^(1 / (1 - m))
}

#' @details
#' Gompertz growth model:
#' \deqn{ y(t) = \alpha exp(-\beta exp(-k^t))}{ y(t) = \alpha * exp(-\beta * exp(-k^t))}
#'
#' @param beta growth displacement
#'
#' @examples
#' gm_gompertz(0:10, 10, 0.5, 0.3)
#'
#' @rdname growthmodels
#' @export
gm_gompertz <- function(t, alpha, beta, k) {
    alpha * exp(-beta * exp(-k * t))
}

#' @details
#' Janoschek growth model:
#' \deqn{ y(t) = \alpha *(\alpha - \beta) \exp(-b * t^c)) }
#'
#' @param b growth parameter
#' @param c shape parameter
#'
#' @examples
#' gm_janoschek(0:10, 10, 2, 0.5, 2)
#'
#' @rdname growthmodels
#' @export
gm_janoschek <- function(t, alpha, beta, b, c) {
    alpha - (alpha - beta) * exp(-b * t^c)
}

#' @details
#' Logistic growth model:
#' \deqn{ y(t) = \frac{\alpha}{1 + \beta exp(-k t)}}{ y(t) = \alpha/(1 + \beta * exp(-k * t))}
#'
#' @examples
#' gm_logistic(0:10, 10, 0.5, 0.3)
#'
#' @rdname growthmodels
#' @export
gm_logistic <- function(t, alpha, beta, k) {
    alpha / (1 + beta * exp(-k * t))
}

#' @details
#' Log-logistic growth model:
#' \deqn{ y(t) = \frac{\alpha}{1 + \beta exp(-k log(t)}}{ y(t) = \alpha/(1 + \beta * exp(-k * log(t))}
#'
#' @examples
#' gm_loglogistic(0:10, 10, 0.5, 0.3)
#'
#' @rdname growthmodels
#' @export
gm_loglogistic <- function(t, alpha, beta, k) {
    t[t < 0] <- NaN
    logistic(log(t), alpha, beta, k)
}

#' @details
#' Mitcherlich growth model:
#' \deqn{ y(t) = (\alpha - \beta k^t)}{ y(t) = \alpha - \beta * k^t}
#'
#' @examples
#' gm_mitcherlich(0:10, 10, 0.5, 0.3)
#'
#' @rdname growthmodels
#' @export
gm_mitcherlich <- function(t, alpha, beta, k) {
    alpha - beta * k^t
}

#' @details
#' Morgan-Mercer-Flodin growth model:
#' \deqn{ y(t) = \frac{(w_0 \gamma + \alpha t^m)}{\gamma} +t^m}{ y(t) = (w_0 * \gamma + \alpha * t^m) / (\gamma + t^m)}
#'
#' @param gamma parameter that controls the point of inflection
#' @param m growth rate
#'
#' @examples
#' gm_mmf(0:10, 10, 0.5, 4, 1)
#'
#' @rdname growthmodels
#' @export
gm_mmf <- function(t, alpha, w0, gamma, m) {
    (w0 * gamma + alpha * t^m) / (gamma + t^m)
}

#' @details
#' Monomolecular growth model:
#' \deqn{ y(t) = \alpha ( 1 - \beta exp(-k t))}{ y(t) = \alpha * ( 1 - \beta * exp(-k * t))}
#'
#' @examples
#' gm_monomolecular(0:10, 10, 0.5, 0.3)
#'
#' @rdname growthmodels
#' @export
gm_monomolecular <- function(t, alpha, beta, k) {
    alpha * (1.0 - beta * exp(-k * t))
}

#' @details
#' Negative exponential growth model:
#' \deqn{ y(t) = \alpha ( 1 - exp(-k t))}{ y(t) = \alpha * ( 1 - exp(-k * t))}
#'
#' @examples
#' gm_negativeExponential(0:10, 1, 0.3)
#'
#' @rdname growthmodels
#' @export
gm_negativeExponential <- function(t, alpha, k) {
    alpha * (1.0 - exp(-k * t))
}

#' @details
#' Richard growth model:
#' \deqn{ y(t) = \frac{\alpha}{(1 + \beta exp(-k t))^{(1/m)}}}{ y(t) = \alpha/((1 + \beta * exp(-k * t))^(1 / m))}
#'
#' @rdname growthmodels
#' @export
gm_richard <- function(t, alpha, beta, k, m) {
    alpha / ((1 + beta * exp(-k * t))^(1 / m))
}

#' @details
#' Schnute growth model:
#' \deqn{ y(t) =  \left[ r_0 + \beta exp(k t) \right]^m }{ y(t) = (r_0 + \beta * exp(k * t))^m }
#'
#' @param r0 reference value
#'
#' @examples
#' gm_schnute(0:10, 10, 5, .5, .5)
#'
#' @rdname growthmodels
#' @export
gm_schnute <- function(t, r0, beta, k, m) {
    (r0 + beta * exp(k * t))^m
}

#' @details
#' Stannard growth model:
#' \deqn{ y(t) = \alpha \left[ 1 + exp(-(\beta + k t)/m) \right]^{-m}}{ y(t) = \alpha *( 1 + exp(-(beta + k * t)/m))^(-m) }
#'
#' @examples
#' gm_stannard(0:10, 1, .2, .1, .5)
#'
#' @rdname growthmodels
#' @export
gm_stannard <- function(t, alpha, beta, k, m) {
    alpha * (1 + exp(-(beta + k * t)/m) )^(-m)
}

#' @details
#' von Bertalanffy growth model:
#' \deqn{ y(t) = (\alpha^(1-m) - \beta * exp(-k t))^(1/(1-m)) }{ y(t) = (\alpha^(1-m) - \beta * exp(-k * t))^(1/(1-m)) }
#'
#' @examples
#' gm_vonBertalanffy(0:10, 10, 0.5, 0.3, 0.5)
#'
#' @rdname growthmodels
#' @export
gm_vonBertalanffy <- function(t, alpha, beta, k, m) {
    (alpha^(1 - m) - beta * exp(-k * t)) ^ (1 / (1 - m))
}


# classic sigmoidal -------------------------------------------------------

# sourced from 'modeling the bacterial growth curve'

sig_gompterz <- function(a, b, c) {
    a * exp(-exp(b - c * t))
}




# Pkackage: pygrowthmodels ------------------------------------------------

# these are the models from pygrowthmodels, not all actually translated.

pg_blumberg <- function() {
    message('not implemented.')
}

pg_brody <- function() {
    message('not implemented.')
}

pg_chapman_richards <- function() {
    message('not implemented.')
}

#' @export
pg_gompertz <- function(time, alpha, beta, rate) {
    alpha * exp(-beta * exp(-rate * time))
}

#' @export
pg_gompertz_inv <- function(size, alpha, beta, rate) {
    if (size == 0)
        return(NaN)
    - log(-log(size / alpha) / beta) / rate
}

pg_janochek <- function() {
    message('not implemented.')
}

pg_logistic <- function() {
    message('not implemented.')
}

pg_mitcherlich <- function() {
    message('not implemented.')
}

pg_mmf <- function() {
    message('not implemented.')
}

pg_monomolecular <- function() {
    message('not implemented.')
}

pg_negative_exponential <- function() {
    message('not implemented.')
}

pg_richard <- function() {
    message('not implemented.')
}

#' Rosso Growth Model
#'
#' @param time	time
#' @param mu	maximal growth rate
#' @param lag	time lag
#' @param lower	lower asymptote
#' @param alpha	upper asymptote
#'
#' @references
#' T. Ross, "Indices for performance evaluation of predictive models in
#' food microbiology." J. Appl. Bacteriol. vol. 81, no. 5, pp. 501-508.
#' Nov. 1996.
#'
#' @export
pg_rosso <- function(time, mu, lag, lower, alpha) {
    if (time <= lag)
        return(lower)
    alpha - log(1 + (exp(alpha - lower) - 1) * exp(-mu * (time - lag)))
}

#' inverse of rosso model
#'
#' @param size size (y-value)
#'
#' @rdname pg_rosso
#' @export
pg_rosso_inv <- function(size, mu, lag, lower, alpha) {
    lag - log((exp(alpha - size) - 1) / (exp(alpha - lower) - 1)) / mu
}

pg_schnute <- function() {
    message('not implemented.')
}

pg_stannard <- function() {
    message('not implemented.')
}

pg_vonbertalanffy <- function() {
    message('not implemented.')
}

#' @export
pg_weibull <- function(time, alpha, beta, rate, slope) {
    alpha - beta * exp(-rate * time ^ slope)
}

#' @export
pg_weibull_inv <- function(size, alpha, beta, rate, slope) {
    if ((alpha - size) == 0)
        return(Inf)
    ((-1 / rate) * log((alpha - size) / beta)) ^ (1 / slope)
}
