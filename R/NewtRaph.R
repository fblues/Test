#' Newton-Raphson Method
#' 
#' Function for Newton-Raphson method
#' 
#' Okay
#' 
#' @param thseed Initial value for theta
#' @param xiseed Initial value for xi
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
#' function (thseed, xiseed, Y, X, tol = 10^(-5)) 
#' {
#'     p = length(thseed)
#'     kiter = 0
#'     th0 = thseed
#'     xi0 = xiseed
#'     OK = FALSE
#'     while (!OK) {
#'         kiter = kiter + 1
#'         est0 = c(th0, xi0)
#'         out = UHFuncs(th0, xi0, Y, X)
#'         U = out$U
#'         H = out$H
#'         UUt = out$UUt
#'         Hinv = solve(H)
#'         est1 = est0 - Hinv %*% U
#'         th0 = est1[-(p + 1)]
#'         xi0 = est1[(p + 1)]
#'         dist = sqrt(sum((est1 - est0)^2))
#'         if (dist < tol) {
#'             OK = TRUE
#'         }
#'     }
#'     XiCov = Hinv %*% UUt %*% t(Hinv)
#'     return(list(kiter = kiter, th = th0, xi = xi0, U = U, H = H, 
#'         Hinv = Hinv, UUt = UUt, XiCov = XiCov))
#'   }
#' 
#' @export
NewtRaph <-
function(thseed,xiseed,Y,X,tol=10^(-5))
{
p = length(thseed)
kiter = 0
th0 = thseed
xi0 = xiseed
OK = FALSE

while(!OK) {

kiter = kiter + 1
est0 = c(th0,xi0)

out = UHFuncs(th0,xi0,Y,X)

U = out$U
H = out$H
UUt = out$UUt

Hinv = solve(H)

est1 = est0 - Hinv%*%U
th0 = est1[-(p+1)]
xi0 = est1[(p+1)]
dist = sqrt(sum((est1-est0)^2))
if(dist < tol) {OK = TRUE}
}

XiCov = Hinv%*%UUt%*%t(Hinv)

return(list(kiter=kiter,th=th0,xi=xi0,U=U,H=H,Hinv=Hinv,UUt = UUt,XiCov=XiCov))
}
