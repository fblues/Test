% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UHFuncs.R
\name{UHFuncs}
\alias{UHFuncs}
\title{UHFuncs}
\usage{
UHFuncs(th, xi, Y, X)
}
\arguments{
\item{th}{parameter1}

\item{xi}{target parameter}

\item{Y}{data}

\item{X}{data}
}
\value{
list
}
\description{
Estimation
}
\details{
Okay
}
\note{
%% ~~further notes~~
}
\examples{

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (th, xi, Y, X) 
{
    p = length(th)
    n = length(Y)
    U1 = rep(0, p)
    U2 = 0
    UUt = matrix(0, (p + 1), (p + 1))
    H11 = matrix(0, p, p)
    H12 = matrix(0, p, 1)
    H21 = matrix(0, 1, p)
    H22 = 0
    for (i in 1:n) {
        xtemp = as.vector(X[i, ])
        ytemp = Y[i]
        rho = as.numeric(exp(t(xtemp) \%*\% th))
        u1add = xtemp * (ytemp - rho)
        u2add = (ytemp - rho)^2 - rho * (1 + (1 + rho)/xi)
        u12add = c(u1add, u2add)
        U1 = U1 + u1add
        U2 = U2 + u2add
        UUt = UUt + u12add \%*\% t(u12add)
        H11 = H11 - (xtemp \%*\% t(xtemp)) * rho
        H21 = H21 - xtemp * rho * (2 * (ytemp - rho) + (1 + (1 + 
            rho)/xi) - rho/xi^2)
        H22 = H22 + (1/xi^2) * rho * (1 + rho)
    }
    U = c(U1, U2)/n
    H = rbind(cbind(H11, H12), cbind(H21, H22))/n
    UUt = UUt/n
    return(list(U = U, H = H, UUt = UUt))
  }

}
\references{
%% ~put references to the literature/web site here ~
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\author{
E. A. Pena
}
