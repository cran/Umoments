% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unbmom1.R
\name{uM2M4}
\alias{uM2M4}
\title{Unbiased central moment estimates}
\usage{
uM2M4(m2, m3, m4, m6, n)
}
\arguments{
\item{m2}{naive biased variance estimate \eqn{m_2 = 1/n \sum_{i = 1}^n ((X_i
- \bar{X})^2}{m[2] = mean((X - X-bar)^2)} for a vector \code{X}.}

\item{m3}{naive biased third central moment estimate \eqn{m_3 = 1/n \sum_{i =
1}^n ((X_i - \bar{X})^3}{m[3] = mean((X - X-bar)^3)} for a vector \code{X}.}

\item{m4}{naive biased fourth central moment estimate \eqn{m_4 = 1/n \sum_{i
= 1}^n ((X_i - \bar{X})^4}{m[4] = mean((X - X-bar)^4)} for a vector
\code{X}.}

\item{m6}{naive biased sixth central moment estimate \eqn{m_6 = 1/n \sum_{i =
1}^n ((X_i - \bar{X})^6}{m[6] = mean((X - X-bar)^6)} for a vector \code{X}.}

\item{n}{sample size.}
}
\value{
Unbiased estimate of a product of second and fourth central moments
  \eqn{\mu_2 \mu_4}{\mu[2] \mu[4]}, where \eqn{\mu_2}{\mu[2]} and
  \eqn{\mu_4}{\mu[4]} are second and fourth central moments respectively.
}
\description{
Calculate unbiased estimates of central moments and their powers and
products.
}
\examples{
n <- 10
smp <- rgamma(n, shape = 3)
m <- mean(smp)
for (j in 2:6) {
  m <- c(m, mean((smp - m[1])^j))
}
uM2M4(m[2], m[3], m[4], m[6], n)
}
\seealso{
Other unbiased estimates (one-sample): 
\code{\link{uM2}()},
\code{\link{uM2M3}()},
\code{\link{uM2pow2}()},
\code{\link{uM2pow3}()},
\code{\link{uM3}()},
\code{\link{uM3pow2}()},
\code{\link{uM4}()},
\code{\link{uM5}()},
\code{\link{uM6}()}
}
\concept{unbiased estimates (one-sample)}
