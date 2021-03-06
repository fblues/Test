#' Main Function on Over-dispersed Poisson Regression
#' 
#' This is the main file which incorporates other functions.
#' 
#' Okay
#' 
#' @param daycutpred threshold for prediction
#' @param maxday maximum day for the prediction
#' @param ordfit the order of polynomial of Poisson regression
#' @param DayEffect day effect
#' @param Country country name
#' @param xival over-dispersion parameter, xi  
#' @param dat data
#' @param alpha alpha 
#' @param yhighval graphical parameter
#' @param targetval graphical parameter
#' @param LegPos graphical parameter
#' @param ToPlot plot or not
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
#' function (daycutpred = 137, maxday = 154, ordfit = 5, DayEffect = TRUE, 
#'     Country = "USA", xival = NULL, dat = NULL, alpha = 0.05, 
#'     yhighval = NULL, targetval = 1e+05, LegPos = c("topright", 
#'         "left"), ToPlot = TRUE) 
#' {
#'     if (is.null(dat)) {
#'         countrydata = UpdateToday()
#'     }
#'     else {
#'         countrydata = dat
#'     }
#'     dat = AnalysisByCountry2(Country, alldata = countrydata)
#'     daynum = dat$daynum
#'     deaths = dat$dailydeaths
#'     cumdeaths = dat$cumdeaths
#'     DateRep = dat$DateRep
#'     close.screen(all.screens = T)
#'     daynumc = daynum - daynum[1]
#'     X = NULL
#'     collab = NULL
#'     for (j in 0:ordfit) {
#'         X = cbind(X, daynumc^j)
#'         collab = c(collab, paste("DayNum", j))
#'     }
#'     colnames(X) = collab
#'     if (DayEffect) {
#'         dayclass = daynum%%7
#'         dayclass = factor(dayclass, levels = as.character(0:6), 
#'             labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
#'                 "Friday", "Saturday", "Sunday"))
#'         ll = length(dayclass)
#'         desmat = model.matrix(~dayclass)[, -1]
#'         daylab = c("Tuesday", "Wednesday", "Thursday", "Friday", 
#'             "Saturday", "Sunday")
#'         X = cbind(X, desmat)
#'         colnames(X) = c(collab, daylab)
#'     }
#'     inpred = (daynum <= daycutpred)
#'     numdaysused = sum(inpred)
#'     Y = deaths[inpred]
#'     X = X[inpred, ]
#'     X0 = NULL
#'     dayspred = daynum[1]:maxday
#'     dayspredc = dayspred - daynum[1]
#'     for (j in 0:ordfit) {
#'         X0 = cbind(X0, dayspredc^j)
#'     }
#'     if (DayEffect) {
#'         dayspredclass = as.factor(dayspred%%7)
#'         dayspredclass = factor(dayspredclass, levels = 0:6, labels = c("Monday", 
#'             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", 
#'             "Sunday"))
#'         ll = length(dayspredclass)
#'         desmat = model.matrix(~dayspredclass)[, -1]
#'         X0 = cbind(X0, desmat)
#'     }
#'     numdaystopred = maxday - daycutpred
#'     if (numdaystopred == 0) {
#'         alphaeff = alpha
#'     }
#'     else {
#'         alphaeff = 1 - (1 - alpha)^(1/numdaystopred)
#'     }
#'     out = OveDisPoiReg(Y, X, X0, alpha = alphaeff, xival = xival)
#'     outglm = out$outglm
#'     outglmsumm = out$outglmsumm
#'     thetahat = out$theta
#'     xihat = out$xi
#'     XiCov = out$XiCov
#'     Xmean = out$Xmean
#'     Xstd = out$Xstd
#'     FitPred = out$FitPred[, 1:3]
#'     if (is.null(yhighval)) {
#'         yhighval = max(c(deaths, FitPred[, 3]))
#'     }
#'     if (ToPlot) {
#'         plot(NULL, NULL, xlim = c(daynum[1], maxday), ylim = c(0, 
#'             yhighval), xlab = "Day Number Since 12/31/2019", 
#'             ylab = "Daily Deaths", main = paste("Daily Deaths due to Covid-19 in ", 
#'                 Country))
#'         points(daynum[inpred], Y, col = "black", lwd = 3)
#'         matlines(dayspred, FitPred, col = c("blue", rep("red", 
#'             2)), lty = 1, pch = 1, lwd = 3)
#'         points(daynum[(!inpred) & (daynum <= maxday)], deaths[(!inpred) & 
#'             (daynum <= maxday)], col = "red", lwd = 3)
#'         abline(v = daycutpred, col = "black", lwd = 2)
#'         abline(v = maxday, col = "green", lwd = 2)
#'     }
#'     lendayspred = length(dayspred)
#'     cumdeathspred = rep(0, lendayspred)
#'     cumdeathspredlow = rep(0, lendayspred)
#'     cumdeathspredhig = rep(0, lendayspred)
#'     for (i in 1:lendayspred) {
#'         cumdeathspred[i] = sum(FitPred[1:i, 1])
#'         cumdeathspredlow[i] = sum(FitPred[1:i, 2])
#'         cumdeathspredhig[i] = sum(FitPred[1:i, 3])
#'     }
#'     if (daycutpred < maxday) {
#'         daysahead = (daycutpred + 1):maxday
#'         lendaysahead = length(daysahead)
#'         lennow = length(daynum[daynum <= daycutpred])
#'         currentval = cumdeaths[lennow]
#'         cumdeathspredahead = rep(currentval, lendaysahead)
#'         cumdeathspredaheadlow = rep(currentval, lendaysahead)
#'         cumdeathspredaheadhig = rep(currentval, lendaysahead)
#'         for (i in 1:lendaysahead) {
#'             cumdeathspredahead[i] = cumdeathspredahead[i] + sum(FitPred[(lennow + 
#'                 1):(lennow + i), 1])
#'             cumdeathspredaheadlow[i] = cumdeathspredaheadlow[i] + 
#'                 sum(FitPred[(lennow + 1):(lennow + i), 2])
#'             cumdeathspredaheadhig[i] = cumdeathspredaheadhig[i] + 
#'                 sum(FitPred[(lennow + 1):(lennow + i), 3])
#'         }
#'         maxy = max(cumdeathspredaheadhig)
#'     }
#'     else {
#'         maxy = max(cumdeaths[inpred])
#'     }
#'     if (ToPlot) {
#'         scan()
#'         plot(NULL, NULL, xlim = range(dayspred), ylim = c(0, 
#'             maxy), xlab = "Day Number Since 12/31/2019", ylab = "Cumulative Deaths", 
#'             main = paste("Cumulative Deaths Due to Covid-19 in ", 
#'                 Country))
#'         lines(dayspred, cumdeathspred, type = "l", col = c("blue"), 
#'             lty = 1, pch = 1, lwd = 3)
#'         points(daynum[inpred], cumdeaths[inpred], col = "black", 
#'             lwd = 3)
#'         points(daynum[(!inpred) & (daynum <= maxday)], cumdeaths[(!inpred) & 
#'             (daynum <= maxday)], col = "red", lwd = 3)
#'         if (daycutpred < maxday) {
#'             matlines(daysahead, cbind(cumdeathspredaheadlow, 
#'                 cumdeathspredaheadhig), col = "magenta", lty = 1, 
#'                 pch = 2, lwd = 3)
#'         }
#'         abline(v = daycutpred, col = "black", lwd = 2)
#'         abline(v = maxday, col = "green", lwd = 2)
#'     }
#'     summpreds = NULL
#'     if (daycutpred < maxday) {
#'         summpreds = data.frame(daynum = daysahead, Pred = cumdeathspredahead, 
#'             PILow = cumdeathspredaheadlow, PIHig = cumdeathspredaheadhig)
#'     }
#'     return(list(Country = Country, dat = dat, outglm = outglm, 
#'         outglmsumm = outglmsumm, daycutpred = daycutpred, numdaysused = numdaysused, 
#'         maxday = maxday, numdaystopred = numdaystopred, ordfit = ordfit, 
#'         alpha = alpha, alphaeff = alphaeff, thetahat = thetahat, 
#'         xihat = xihat, XiCov = XiCov, Xmean = Xmean, Xstd = Xstd, 
#'         summpreds = summpreds))
#'   }
#' 
#' @export
PoissonRegOveDis <-
function(daycutpred=137,maxday=154,ordfit=5,DayEffect=TRUE,Country="USA",xival=NULL,dat=NULL,alpha=.05,yhighval=NULL,targetval=100000,LegPos=c("topright","left"),ToPlot=TRUE)
{
if(is.null(dat)) {
countrydata = UpdateToday()
}
else {countrydata=dat}

dat = AnalysisByCountry2(Country,alldata=countrydata)

daynum = dat$daynum
deaths = dat$dailydeaths
cumdeaths = dat$cumdeaths
DateRep = dat$DateRep

close.screen(all.screens=T)
##split.screen(c(1,2))

daynumc = daynum - daynum[1]
X = NULL
collab = NULL
for(j in 0:ordfit) {
X = cbind(X,daynumc^j)
collab = c(collab,paste("DayNum",j))
}
colnames(X) = collab

###includes DayEffect?
if(DayEffect) {
dayclass = daynum%%7
dayclass = factor(dayclass,levels=as.character(0:6),labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
##print(dayclass)
ll = length(dayclass)
##desmat = model.matrix(glm(rep(1,ll)~dayclass))[,-1]
desmat = model.matrix(~dayclass)[,-1]
##print(desmat)
##scan()

daylab = c("Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
X = cbind(X,desmat)
colnames(X) = c(collab,daylab)
}

inpred = (daynum <= daycutpred)
numdaysused = sum(inpred)
Y = deaths[inpred]
X = X[inpred,]


X0 = NULL
dayspred = daynum[1]:maxday
dayspredc = dayspred - daynum[1]
for(j in 0:ordfit) {X0 = cbind(X0,dayspredc^j)}

if(DayEffect) {
dayspredclass = as.factor(dayspred%%7)
dayspredclass = factor(dayspredclass,levels=0:6,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
ll = length(dayspredclass)
desmat = model.matrix(~dayspredclass)[,-1]
##print(desmat)
X0 = cbind(X0,desmat)
}

numdaystopred = maxday-daycutpred
if(numdaystopred==0) {alphaeff = alpha}
else {alphaeff = 1 - (1-alpha)^(1/numdaystopred)}

out = OveDisPoiReg(Y,X,X0,alpha=alphaeff,xival=xival)

outglm = out$outglm
outglmsumm = out$outglmsumm
thetahat = out$theta
xihat = out$xi
XiCov = out$XiCov
Xmean = out$Xmean
Xstd = out$Xstd
FitPred = out$FitPred[,1:3]

##screen(1)

if(is.null(yhighval)) {
yhighval = max(c(deaths,FitPred[,3]))
}

if(ToPlot) {
plot(NULL,NULL,xlim=c(daynum[1],maxday),ylim=c(0,yhighval),xlab="Day Number Since 12/31/2019",ylab="Daily Deaths",main=paste("Daily Deaths due to Covid-19 in ",Country))
points(daynum[inpred],Y,col="black",lwd=3)
matlines(dayspred,FitPred,col=c("blue",rep("red",2)),lty=1,pch=1,lwd=3)
points(daynum[(!inpred) & (daynum <= maxday)],deaths[(!inpred) & (daynum <= maxday)],col="red",lwd=3)
abline(v=daycutpred,col="black",lwd=2)
abline(v=maxday,col="green",lwd=2)
}

###cumulative deaths
lendayspred = length(dayspred)
cumdeathspred = rep(0,lendayspred)
cumdeathspredlow = rep(0,lendayspred)
cumdeathspredhig = rep(0,lendayspred)
for(i in 1:lendayspred) {
cumdeathspred[i] = sum(FitPred[1:i,1])
cumdeathspredlow[i] = sum(FitPred[1:i,2])
cumdeathspredhig[i] = sum(FitPred[1:i,3])
}

####predictions conditional on what has been observed so far
if(daycutpred < maxday) {### to predict past last day of data used
daysahead=(daycutpred+1):maxday
lendaysahead = length(daysahead)
lennow = length(daynum[daynum<=daycutpred])
currentval = cumdeaths[lennow]
#print(currentval)
cumdeathspredahead = rep(currentval,lendaysahead)
cumdeathspredaheadlow = rep(currentval,lendaysahead)
cumdeathspredaheadhig = rep(currentval,lendaysahead)
for(i in 1:lendaysahead) {
cumdeathspredahead[i] = cumdeathspredahead[i] + sum(FitPred[(lennow+1):(lennow+i),1])
cumdeathspredaheadlow[i] = cumdeathspredaheadlow[i] + sum(FitPred[(lennow+1):(lennow+i),2])
cumdeathspredaheadhig[i] = cumdeathspredaheadhig[i] + sum(FitPred[(lennow+1):(lennow+i),3])
}
maxy = max(cumdeathspredaheadhig)
}
else {
maxy = max(cumdeaths[inpred])
}

if(ToPlot) {
##screen(2)
scan()
##print(maxy)
plot(NULL,NULL,xlim=range(dayspred),ylim=c(0,maxy),xlab="Day Number Since 12/31/2019",ylab="Cumulative Deaths",main=paste("Cumulative Deaths Due to Covid-19 in ",Country))
lines(dayspred,cumdeathspred,type="l",col=c("blue"),lty=1,pch=1,lwd=3)
points(daynum[inpred],cumdeaths[inpred],col="black",lwd=3)
points(daynum[(!inpred) & (daynum <= maxday)],cumdeaths[(!inpred) & (daynum <= maxday)],col="red",lwd=3)
if(daycutpred < maxday) {
matlines(daysahead,cbind(cumdeathspredaheadlow,cumdeathspredaheadhig),col="magenta",lty=1,pch=2,lwd=3)
}
abline(v=daycutpred,col="black",lwd=2)
abline(v=maxday,col="green",lwd=2)
}

summpreds = NULL
if(daycutpred < maxday) {
summpreds = data.frame(daynum = daysahead,
Pred=cumdeathspredahead,
PILow=cumdeathspredaheadlow,PIHig=cumdeathspredaheadhig)
}

return(list(Country=Country,dat=dat,outglm=outglm,outglmsumm=outglmsumm,daycutpred=daycutpred,numdaysused=numdaysused,maxday=maxday,numdaystopred=numdaystopred,ordfit=ordfit,alpha=alpha,alphaeff=alphaeff,thetahat=thetahat,xihat=xihat,XiCov=XiCov,Xmean=Xmean,Xstd=Xstd,summpreds=summpreds))
}
