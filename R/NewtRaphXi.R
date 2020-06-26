#' Newton-Raphson for Xi
#' 
#' Function for Newton-Raphson method for xi
#' 
#' Okay
#' 
#' @param xiseed Initial value
#' @param theta theta
#' @param Y data
#' @param X data
#' @param tol approximation tolerance
#' @return list
#' @note %% ~~further notes~~
#' @author E. A. Pena
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (xiseed, theta, Y, X, tol = 10^(-5)) 
#' {
#'     rho = exp(X %*% theta)
#'     n = length(Y)
#'     kiter = 0
#'     OK = FALSE
#'     xi0 = xiseed
#'     while (!OK) {
#'         kiter = kiter + 1
#'         g = sum((Y - rho)^2 - rho * (1 + (1 + rho)/xi0))
#'         gp = sum(rho * (1 + rho))/xi0^2
#'         xi1 = xi0 - g/gp
#'         if (abs(xi1 - xi0) < tol) {
#'             OK = TRUE
#'         }
#'         xi0 = xi1
#'         if (xi1 < 0) {
#'             OK = FALSE
#'             xiseed = xiseed/2
#'             xi0 = xiseed
#'             kiter = 0
#'         }
#'     }
#'     return(list(kiter = kiter, xi = xi1))
#'   }
#' 
NewtRaphXi <-
function(xiseed,theta,Y,X,tol=10^(-5))
{
rho = exp(X%*%theta)
n = length(Y)

kiter = 0
OK = FALSE
xi0 = xiseed

while(!OK) {
kiter = kiter + 1

g = sum((Y-rho)^2 - rho*(1+(1+rho)/xi0))
gp = sum(rho*(1+rho))/xi0^2

xi1 = xi0 - g/gp

if(abs(xi1-xi0) < tol) {
OK = TRUE
}
xi0 = xi1

if(xi1 < 0 ) {
OK = FALSE
xiseed = xiseed/2
xi0 = xiseed
kiter = 0
}
}

return(list(kiter=kiter,xi=xi1))

}
