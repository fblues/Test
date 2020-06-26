#' Update today
#' 
#' function for date updating
#' 
#' Okay
#' 
#' @return list
#' @note function for data updating
#' @author E. A. Pena
#' @seealso ###
#' @references No particular reference
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function () 
#' {
#'     library(utils)
#'     library(httr)
#'     GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
#'         authenticate(":", ":", type = "ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
#'     covid19 <- read.csv(tf)
#'     countrydata = ByCountryData(covid19)
#'     return(countrydata)
#'   }
#' 
#' @export
UpdateToday <-
function()
{
library(utils)
library(httr)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into ?R?. The dataset will be called "data".
#covid19 = read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = #"UTF-8-BOM")

covid19 <- read.csv(tf)

countrydata = ByCountryData(covid19)
return(countrydata)
}
