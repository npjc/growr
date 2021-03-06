% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{gm_blumberg}
\alias{gm_blumberg}
\alias{gm_brody}
\alias{gm_chapmanRichards}
\alias{gm_gompertz}
\alias{gm_janoschek}
\alias{gm_logistic}
\alias{gm_loglogistic}
\alias{gm_mitcherlich}
\alias{gm_mmf}
\alias{gm_monomolecular}
\alias{gm_negativeExponential}
\alias{gm_richard}
\alias{gm_schnute}
\alias{gm_stannard}
\alias{gm_vonBertalanffy}
\title{Compute model output given time and parameters.}
\usage{
gm_blumberg(t, alpha, w0, m, t0 = 0)

gm_brody(t, alpha, w0, k)

gm_chapmanRichards(t, alpha, beta, k, m)

gm_gompertz(t, alpha, beta, k)

gm_janoschek(t, alpha, beta, b, c)

gm_logistic(t, alpha, beta, k)

gm_loglogistic(t, alpha, beta, k)

gm_mitcherlich(t, alpha, beta, k)

gm_mmf(t, alpha, w0, gamma, m)

gm_monomolecular(t, alpha, beta, k)

gm_negativeExponential(t, alpha, k)

gm_richard(t, alpha, beta, k, m)

gm_schnute(t, r0, beta, k, m)

gm_stannard(t, alpha, beta, k, m)

gm_vonBertalanffy(t, alpha, beta, k, m)
}
\arguments{
\item{t}{time}

\item{alpha}{upper asymptote}

\item{w0}{a reference value at t = t0, the value at t = 0}

\item{m}{slope of growth}

\item{t0}{time shift (default 0)}

\item{k}{growth rate}

\item{beta}{growth range}

\item{b}{growth parameter}

\item{c}{shape parameter}

\item{gamma}{parameter that controls the point of inflection}

\item{r0}{reference value}

\item{beta}{growth displacement}

\item{m}{growth rate}
}
\value{
y-values of the specified model for given time points.
}
\description{
Compute model output given time and parameters.
}
\details{
Blumberg growth model:
\deqn{ y(t) = ...}

Brody growth model:
\deqn{ y(t) = \alpha - (\alpha - w_0) exp(- k t) }{ y(t) = \alpha - (\alpha - w_0) * exp(- k * t) }

Chapman-Richards growth model:
\deqn{ y(t) = \alpha (1 - \beta exp(-k t)^{1/(1-m)}) }{ y(t) = \alpha * (1 - \beta * exp(-k * t)^{1/(1-m)}) }

Gompertz growth model:
\deqn{ y(t) = \alpha exp(-\beta exp(-k^t))}{ y(t) = \alpha * exp(-\beta * exp(-k^t))}

Janoschek growth model:
\deqn{ y(t) = \alpha *(\alpha - \beta) \exp(-b * t^c)) }

Logistic growth model:
\deqn{ y(t) = \frac{\alpha}{1 + \beta exp(-k t)}}{ y(t) = \alpha/(1 + \beta * exp(-k * t))}

Log-logistic growth model:
\deqn{ y(t) = \frac{\alpha}{1 + \beta exp(-k log(t)}}{ y(t) = \alpha/(1 + \beta * exp(-k * log(t))}

Mitcherlich growth model:
\deqn{ y(t) = (\alpha - \beta k^t)}{ y(t) = \alpha - \beta * k^t}

Morgan-Mercer-Flodin growth model:
\deqn{ y(t) = \frac{(w_0 \gamma + \alpha t^m)}{\gamma} +t^m}{ y(t) = (w_0 * \gamma + \alpha * t^m) / (\gamma + t^m)}

Monomolecular growth model:
\deqn{ y(t) = \alpha ( 1 - \beta exp(-k t))}{ y(t) = \alpha * ( 1 - \beta * exp(-k * t))}

Negative exponential growth model:
\deqn{ y(t) = \alpha ( 1 - exp(-k t))}{ y(t) = \alpha * ( 1 - exp(-k * t))}

Richard growth model:
\deqn{ y(t) = \frac{\alpha}{(1 + \beta exp(-k t))^{(1/m)}}}{ y(t) = \alpha/((1 + \beta * exp(-k * t))^(1 / m))}

Schnute growth model:
\deqn{ y(t) =  \left[ r_0 + \beta exp(k t) \right]^m }{ y(t) = (r_0 + \beta * exp(k * t))^m }

Stannard growth model:
\deqn{ y(t) = \alpha \left[ 1 + exp(-(\beta + k t)/m) \right]^{-m}}{ y(t) = \alpha *( 1 + exp(-(beta + k * t)/m))^(-m) }

von Bertalanffy growth model:
\deqn{ y(t) = (\alpha^(1-m) - \beta * exp(-k t))^(1/(1-m)) }{ y(t) = (\alpha^(1-m) - \beta * exp(-k * t))^(1/(1-m)) }
}
\examples{
gm_blumberg(0:10, 10, 2, 0.5)

gm_brody(0:10, 10, 5, 0.3)

gm_chapmanRichards(0:10, 10, 0.5, 0.3, 0.5)

gm_gompertz(0:10, 10, 0.5, 0.3)

gm_janoschek(0:10, 10, 2, 0.5, 2)

gm_logistic(0:10, 10, 0.5, 0.3)

gm_loglogistic(0:10, 10, 0.5, 0.3)

gm_mitcherlich(0:10, 10, 0.5, 0.3)

gm_mmf(0:10, 10, 0.5, 4, 1)

gm_monomolecular(0:10, 10, 0.5, 0.3)

gm_negativeExponential(0:10, 1, 0.3)

gm_schnute(0:10, 10, 5, .5, .5)

gm_stannard(0:10, 1, .2, .1, .5)

gm_vonBertalanffy(0:10, 10, 0.5, 0.3, 0.5)

}
\references{
M. M. Kaps, W. O. W. Herring, and W. R. W. Lamberson, "Genetic and
environmental parameters for traits derived from the Brody growth curve and
their relationships with weaning weight in Angus cattle.," Journal of
Animal Science, vol. 78, no. 6, pp. 1436-1442, May 2000.

D. Fekedulegn, M. Mac Siurtain, and J. Colbert, "Parameter estimation of
nonlinear growth models in forestry," Silva Fennica, vol. 33, no. 4, pp.
327-336, 1999.

Michael J. Panik, "Growth Curve Modeling: Theory and Applications",
John Wiley & Sons, December 2013.

A. Khamiz, Z. Ismail, and A. T. Muhammad, "Nonlinear growth models for
modeling oil palm yield growth," Journal of Mathematics and Statistics,
vol. 1, no. 3, p. 225, 2005.
}
\author{
Daniel Rodriguez
}
