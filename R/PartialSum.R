#' Partial Sum
#' 
#' Function for data partial summation 
#' 
#' Okay
#' 
#' @param x data
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
#' function (x) 
#' {
#'     nlen = length(x)
#'     y = rep(0, nlen)
#'     for (i in 1:nlen) {
#'         y[i] = sum(x[1:i])
#'     }
#'     return(y)
#'   }
#' 
#' @export
PartialSum <-
function(x)
{
nlen = length(x)
y = rep(0,nlen)
for(i in 1:nlen) {
y[i] = sum(x[1:i])
}
return(y)
}
