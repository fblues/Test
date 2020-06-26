#' Analysis by country
#' 
#' function for country-wise analysis
#' 
#' Okay
#' 
#' @param CountryAbbrev Country name
#' @param alldata data
#' @param fval graphical parameter
#' @param lwdval1 graphical parameter
#' @param lwdval2 graphical parameter
#' @param cexval graphical parameter
#' @param toplot plot or not
#' @param lowdeaths the lowest number for deaths
#' @return list
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (CountryAbbrev = "USA", alldata = countrydata, fval = 0.14, 
#'     lwdval1 = 5, lwdval2 = 3, cexval = 0.9, toplot = F, lowdeaths = 1) 
#' {
#'     countrycodes = alldata$CountryCode
#'     ccode = which(countrycodes == CountryAbbrev)
#'     cdata = alldata$Country_Data[[ccode]]
#'     CountryCode = as.character(cdata$code)
#'     CountryID = as.character(cdata$id)
#'     CountryName = as.character(cdata$name)
#'     CountryPopn = cdata$popn
#'     StartDate = as.character(cdata$dateRep[1])
#'     len = length(cdata$dateRep)
#'     EndDate = as.character(cdata$dateRep[len])
#'     ind = which(cdata$cumdeaths >= lowdeaths)
#'     dateFirstCase = as.character(cdata$dateRep[ind[1]])
#'     dates = as.character(cdata$dateRep[ind])
#'     daynum = cdata$dayNum[ind]
#'     dailycases = cdata$cases[ind]
#'     dailydeaths = cdata$deaths[ind]
#'     cumcases = cdata$cumcases[ind]
#'     cumdeaths = cdata$cumdeaths[ind]
#'     close.screen(all.screens = T)
#'     if (toplot) {
#'         plot(daynum, dailydeaths, type = "l", lwd = lwdval1, 
#'             cex = cexval, xlab = "Day Number", ylab = "Daily Deaths")
#'         scan()
#'         plot(daynum, cumdeaths, type = "l", lwd = lwdval1, cex = cexval, 
#'             xlab = "Day Number", ylab = "Cumulative Deaths")
#'     }
#'     return(list(CountryCode = CountryCode, CountryID = CountryID, 
#'         CountryName = CountryName, Population = CountryPopn, 
#'         StartDate = StartDate, dateFirstCase = dateFirstCase, 
#'         EndDate = EndDate, DateRep = dates, daynum = daynum, 
#'         dailydeaths = dailydeaths, cumdeaths = cumdeaths))
#'   }
#' 
AnalysisByCountry2 <-
function(CountryAbbrev="USA",alldata=countrydata,fval=.14,lwdval1=5,lwdval2=3,cexval=.9,toplot=F,lowdeaths=1)
{
countrycodes=alldata$CountryCode
ccode=which(countrycodes==CountryAbbrev)
cdata = alldata$Country_Data[[ccode]]
CountryCode = as.character(cdata$code)
CountryID = as.character(cdata$id)
CountryName = as.character(cdata$name)
CountryPopn = cdata$popn
StartDate = as.character(cdata$dateRep[1])
len = length(cdata$dateRep)
EndDate = as.character(cdata$dateRep[len])

####Plots will only start when first case is reported#####
ind = which(cdata$cumdeaths >= lowdeaths)
dateFirstCase = as.character(cdata$dateRep[ind[1]])

dates = as.character(cdata$dateRep[ind])
daynum = cdata$dayNum[ind]
dailycases = cdata$cases[ind]
dailydeaths = cdata$deaths[ind]
cumcases = cdata$cumcases[ind]
cumdeaths = cdata$cumdeaths[ind]

close.screen(all.screens=T)

if(toplot) {
plot(daynum,dailydeaths,type="l",lwd=lwdval1,cex=cexval,xlab="Day Number",ylab="Daily Deaths")

scan()
plot(daynum,cumdeaths,type="l",lwd=lwdval1,cex=cexval,xlab="Day Number",ylab="Cumulative Deaths")
}

return(list(CountryCode=CountryCode,CountryID=CountryID,CountryName=CountryName,Population=CountryPopn,StartDate=StartDate,dateFirstCase=dateFirstCase,EndDate=EndDate,DateRep=dates,daynum=daynum,dailydeaths=dailydeaths,cumdeaths=cumdeaths))
}
