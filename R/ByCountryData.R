#' County data
#' 
#' Function to extract county data
#' 
#' Okay
#' 
#' @param alldat input data
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
#' function (alldat = covid19) 
#' {
#'     uniqueCode = unique(alldat$countryterritoryCode)
#'     uniqueID = unique(alldat$geoID)
#'     uniqueCountry = unique(alldat$countriesAndTerritories)
#'     lenCode = length(uniqueCode)
#'     Country_Data = vector("list", lenCode)
#'     n = 0
#'     for (uc in uniqueCode) {
#'         n = n + 1
#'         countrydat = alldat[alldat$countryterritoryCode == uc, 
#'             ]
#'         temp = ConvertToList(countrydat)
#'         Country_Data[[n]] = temp
#'     }
#'     return(list(CountryCode = uniqueCode, CountryID = uniqueID, 
#'         Country = uniqueCountry, Country_Data = Country_Data))
#'   }
#' 
#' @export
ByCountryData <-
function(alldat=covid19)
{
uniqueCode = unique(alldat$countryterritoryCode)
uniqueID = unique(alldat$geoID)
uniqueCountry = unique(alldat$countriesAndTerritories)
lenCode = length(uniqueCode)
Country_Data = vector("list",lenCode)
n =  0

for(uc in uniqueCode) {
n = n+1
countrydat = alldat[alldat$countryterritoryCode==uc,]
temp = ConvertToList(countrydat)
Country_Data[[n]] = temp
}

return(list(CountryCode = uniqueCode,CountryID=uniqueID,Country=uniqueCountry,Country_Data=Country_Data))

}
