#' Regression part
#' 
#' Function handles the regression part
#' 
#' Okay
#' 
#' @param Y data
#' @param X data
#' @param X0 prediction 
#' @param xival over-dispersion parameter, xi
#' @param alpha alpha
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
#' function (Y, X, X0 = NULL, xival = NULL, alpha = 0.05, tol = 10^(-5)) 
#' {
#'     p = dim(X)[2]
#'     n = dim(X)[1]
#'     if (is.null(X0)) {
#'         X0 = X
#'     }
#'     m = dim(X0)[1]
#'     Xmean = apply(X, 2, "mean")
#'     Xstd = apply(X, 2, "sd")
#'     XS = X
#'     X0S = X0
#'     for (j in 2:p) {
#'         XS[, j] = (X[, j] - Xmean[j])/Xstd[j]
#'         X0S[, j] = (X0[, j] - Xmean[j])/Xstd[j]
#'     }
#'     outglm = glm(Y ~ XS[, -1], family = poisson(link = log))
#'     outglmsumm = summary(outglm)
#'     theta = outglm$coefficients
#'     if (is.null(xival)) {
#'         xi = NewtRaphXi(1, theta, Y, XS, tol)$xi
#'     }
#'     else {
#'         xi = Inf
#'     }
#'     XiCov = NewtRaph(theta, xi, Y, XS, tol)$XiCov
#'     XiCov11 = XiCov[1:p, 1:p]
#'     rho0 = exp(X0S %*% theta)
#'     zcrit = qnorm(1 - alpha/2)
#'     PE = rep(0, m)
#'     PE2 = rep(0, m)
#'     for (i in 1:m) {
#'         PE[i] = zcrit * sqrt(rho0[i] + rho0[i] * (1 + rho0[i])/xi + 
#'             (1/n) * (rho0[i]^2) * (t(X0S[i, ]) %*% XiCov11) %*% 
#'                 X0S[i, ])
#'     }
#'     PILow = rho0 - PE
#'     PIHig = rho0 + PE
#'     for (i in 1:m) {
#'         PILow[i] = max(0, PILow[i])
#'     }
#'     return(list(outglm = outglm, outglmsumm = outglmsumm, theta = theta, 
#'         xi = xi, XiCov = XiCov, Xmean = Xmean, Xstd = Xstd, FitPred = data.frame(rho0, 
#'             PILow, PIHig, X0)))
#'   }
#' 
OveDisPoiReg <-
function(Y,X,X0=NULL,xival=NULL,alpha=.05,tol=10^(-5))
{
p = dim(X)[2]
n = dim(X)[1]

if(is.null(X0)) {X0 = X}
m = dim(X0)[1]

Xmean = apply(X,2,"mean")
Xstd = apply(X,2,"sd")

XS = X
X0S = X0
for(j in 2:p) {
XS[,j] = (X[,j]-Xmean[j])/Xstd[j]
X0S[,j] = (X0[,j]-Xmean[j])/Xstd[j]
}

outglm = glm(Y~XS[,-1],family=poisson(link=log))

outglmsumm = summary(outglm)
theta = outglm$coefficients

if(is.null(xival)) {
xi = NewtRaphXi(1,theta,Y,XS,tol)$xi
}
else {xi = Inf}

XiCov = NewtRaph(theta,xi,Y,XS,tol)$XiCov
XiCov11 = XiCov[1:p,1:p]

##print(XiCov11)
##scan()

##Sigma = matrix(0,p,p)
##rho = exp(XS%*%theta)
##for(i in 1:n) {
##Sigma = Sigma + (XS[i,]%*%t(XS[i,]))*rho[i]*(1+(1+rho[i])/xi)
##}
##Sigma = Sigma/n
##SigmaInv = solve(Sigma)

##print(SigmaInv)
##scan()

rho0 = exp(X0S%*%theta)

zcrit = qnorm(1-alpha/2)
PE = rep(0,m)
PE2 = rep(0,m)
for(i in 1:m) {
PE[i] = zcrit*sqrt(rho0[i]+rho0[i]*(1+rho0[i])/xi + (1/n)*(rho0[i]^2)*(t(X0S[i,])%*%XiCov11)%*%X0S[i,])
##PE2[i] = zcrit*sqrt(rho0[i]+rho0[i]*(1+rho0[i])/xi + (1/n)*(rho0[i]^2)*(t(X0S[i,])%*%SigmaInv)%*%X0S[i,])
}

##print(data.frame(1:m,PE,PE2))
##scan()

PILow = rho0 - PE
PIHig = rho0 + PE
for(i in 1:m) {PILow[i] = max(0,PILow[i])}

##close.screen(all.screens=T)
##matplot(1:m,cbind(Y,rho0,PILow,PIHig),type="plll",lwd=3,col=c("black","blue",rep("red",2)),lty=1,pch=1)

return(list(outglm=outglm,outglmsumm=outglmsumm,theta=theta,xi=xi,XiCov=XiCov,Xmean=Xmean,Xstd=Xstd,FitPred=data.frame(rho0,PILow,PIHig,X0)))
}
