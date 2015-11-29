#############  FROM fun.R 

#' Create a time-invariant variable by filling in NAs -- generic
#' 
#' @param x 
#' @param \dots 

#' Create a longitudinal file from a wide file
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases long tolong
#' @param data %% ~~Describe \code{data} here~~
#' @param sep (default "_" in 'tolong") the separator between the long variable
#' name and the time
#' @param expand (default FALSE in 'tolong') if TRUE and if there are missing
#' combinations of long variable names and times, the missing combinations are
#' created with NAs
#' @param varying %% ~~Describe \code{varying} here~~
#' @param sep.varying %% ~~Describe \code{sep.varying} here~~
#' @param v.names %% ~~Describe \code{v.names} here~~
#' @param timevar %% ~~Describe \code{timevar} here~~
#' @param idvar %% ~~Describe \code{idvar} here~~
#' @param ids %% ~~Describe \code{ids} here~~
#' @param times %% ~~Describe \code{times} here~~
#' @param drop %% ~~Describe \code{drop} here~~
#' @param new.row.names %% ~~Describe \code{new.row.names} here~~
#' @param split %% ~~Describe \code{split} here~~
#' @param debug %% ~~Describe \code{debug} here~~
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' \dontrun{
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (data, varying = NULL, sep.varying = "\.", v.names = names(Varying),
#'     timevar = "time", idvar = "id", ids = 1:NROW(data), times = seq(length = length(Varying[[1]])),
#'     drop = NULL, new.row.names = NULL, split = list(regexp = "\.",
#'         include = FALSE), debug = FALSE)
#' {
#'     help <- "\n    Example:   long(dwl, varying = c('wmrss', 'lm1tot','lm2tot'), sep='')\n    "
#'     nn <- names(data)
#'     if (debug)
#'         disp(nn)
#'     Varying <- list()
#'     for (ii in 1:length(varying)) {
#'         if (length(prefix <- varying[[ii]]) == 1) {
#'             ns <- grep(paste("^", prefix, sep.varying, sep = ""),
#'                 nn, value = T)
#'             if (debug)
#'                 disp(ns)
#'             Varying[[ii]] <- ns
#'             names(Varying)[ii] <- prefix
#'         }
#'         else {
#'             Varying[[ii]] <- varying[[ii]]
#'             names(Varying)[ii] <- names(varying)[ii]
#'         }
#'     }
#'     if (debug)
#'         disp(Varying)
#'     if (debug)
#'         disp(times)
#'     ret <- stats::reshape(data, Varying, v.names, timevar, idvar,
#'         ids, times, drop, direction = "long", new.row.names,
#'         split)
#'     ret[order(ret[[idvar]], ret[[timevar]]), ]
#'   }
#' 
#' 
#' 
#' tolong <- function(data, sep = "_",...){
#'   reshape(data, direction = 'long', sep = sep, varying = grep(sep, names(data)),...)
#' }
#' }
#' 