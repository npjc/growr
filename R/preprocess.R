#' preprocess raw growth-curve measures
#'
#' @param y `<num>` numeric vector of values to preprocess
#' @param log_base `<num>|<lgl>` base of log for log(y / min(y)) transformation.
#'     Defaults to base `[exp(1)]`. set to [NA] or [FALSE] to skip.
#' @param runmed_k integer width of median window, must be odd. Passed to
#'     [stats::runmed()]. Defaults to `3`. set to [NA] or [FALSE] to skip.
#' @param runmav_n integer width of moving average filter. Defaults to 5. set
#'     to [NA] or [FALSE] to skip.
#' @param force_inc force y to be monotonically increasing?
#' @param bg_subtract numeric value to use for background substraction. Defaults
#'     to `min(y)`. set to [NA] or [FALSE] to skip.
#' @param calibrate_fxn function to calibrate y values. Usually for correcting
#'     for non-linear scattering at higher concentrations. Defaults to the
#'     identity function [I], that simply returns y un-transformed (as is).
#'
#' @return numeric vector of pre-processed values.
#' @export
#'
#' @examples
#' y <- c(1:10, 8,11)
#' preprocess(y)
#' preprocess(1:10, log_base = F, runmed_k = F, runmav_n = F, force_inc = F, bg_subtract = F, calibrate_fxn = F)
preprocess <- function(y, log_base = exp(1), runmed_k = 3, runmav_n = 5,
                       force_inc = TRUE, bg_subtract = function(y) y - min(y),
                       calibrate_fxn = function(y) y) {
    if (is_todo(log_base))
        y <- log(y / min(y), base = log_base)
    if (is_todo(runmed_k))
        y <- runmed(y, k = runmed_k)
    if (is_todo(runmav_n))
        y <- runmav(y, n = runmav_n)
    if (is_todo(force_inc))
        y <- enforce_mono_inc(y)
    if (is_todo(bg_subtract))
        y <- bg_subtract(y)
    if (is_todo(calibrate_fxn))
        y <- calibrate_fxn(y)
    y
}

#' running mean smoothing of length n
#'
#' @param y values to process
#' @param n length of filter, width of window to apply mean smoothing to
#'
runmav <- function(y, n) {
    if (n %% 2 == 0)
        stop("n must be odd. n = ", n, call. = FALSE)
    if (length(n) > length(y))
        stop("length of filter n > length of vector y.", call. = FALSE)
    orig_len <- length(y)
    npad <- floor(n / 2)
    y <- c(rep(head(y, 1), npad), y, rep(tail(y, 1), npad))
    out <- stats::filter(y, filter = rep(1/n, n), method = "convolution",
                         sides = 2)
    out <- as.numeric(out)
    out[seq_len(orig_len) + npad]
}

#' enforce monotonically increasing
#'
#' @param y values to process
enforce_mono_inc <- function(y) {
    out <- y
    for (i in seq_along(out)[-1]) {
        comp <- out[i] < out[i - 1]
        if (comp && !is.na(comp))
            out[i] <- out[i - 1]
    }
    out
}

#' check if a value indicates there is something to do.
#'
#' Similar to a 'true-ish' type value. Everything but [FALSE] and [NA] should
#' return [TRUE].
#'
#' @param x value to check if is to do.
#'
#' @return [TRUE] or [FALSE] indicating if there is something to do or not.
#' @keywords internal
#'
#' @examples
#' is_todo(NA)
#' is_todo(3)
#' is_todo('a')
#' is_todo(TRUE)
#' is_todo(FALSE)
#' is_todo(I)
#' is_todo(function(y) {y})
is_todo <- function(x) {
    if (is.function(x))
        return(TRUE)
    !is.na(x) & x != FALSE
}
