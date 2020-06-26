#' Convert to list
#' 
#' Function for data conversion
#' 
#' Okay
#' 
#' @param countrydat input data
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
#' function (countrydat) 
#' {
#'     nrows = dim(countrydat)[1]
#'     seqrev = nrows:1
#'     dat = NULL
#'     dat$code = countrydat[1, 9]
#'     dat$id = countrydat[1, 8]
#'     dat$name = countrydat[1, 7]
#'     dat$popn = countrydat[1, 10]
#'     dat$dayNum = 1:nrows
#'     dat$dateRep = countrydat[seqrev, 1]
#'     dat$day = countrydat[seqrev, 2]
#'     dat$month = countrydat[seqrev, 3]
#'     dat$year = countrydat[seqrev, 4]
#'     dat$cases = countrydat[seqrev, 5]
#'     dat$deaths = countrydat[seqrev, 6]
#'     dat$cumcases = PartialSum(dat$cases)
#'     dat$cumdeaths = PartialSum(dat$deaths)
#'     return(dat)
#'   }
#' 
#' @export
ConvertToList <-
function(countrydat)
{
nrows = dim(countrydat)[1]
seqrev = nrows:1
dat = NULL
dat$code = countrydat[1,9]
dat$id = countrydat[1,8]
dat$name = countrydat[1,7]
dat$popn = countrydat[1,10]
dat$dayNum = 1:nrows
dat$dateRep = countrydat[seqrev,1]
dat$day = countrydat[seqrev,2]
dat$month = countrydat[seqrev,3]
dat$year = countrydat[seqrev,4]
dat$cases = countrydat[seqrev,5]
dat$deaths = countrydat[seqrev,6]
dat$cumcases = PartialSum(dat$cases)
dat$cumdeaths = PartialSum(dat$deaths)
return(dat)
}
