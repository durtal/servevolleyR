#' simPoint
#'
#' @description simulate probability of server winning a point
#'
#' @param p probability of server winning point on their first serve
#' @param p2 probability of server winning point on their second serve
#' @param firstServe probability of first serve being in
#'
#' @return 1 (if server wins), 0 (if returner wins)
#'
#' @export
simPoint <- function(p, p2 = NULL, firstServe = NULL) {

    # if firstServe and p2 params supplied, simulate first serve being out
    if(!is.null(p2) && !is.null(firstServe)) {
        # simulate if first serve was in/out
        i <- runif(n = 1, min = 0, max = 1)
        if(i > firstServe) {
            p <- p2
            i <- runif(n = 1, min = 0, max = 1)
            if(i <= p) {
                return(1)
            } else {
                return(0)
            }
        } else {
            i <- runif(n = 1, min = 0, max = 1)
            if(i <= p) {
                return(1)
            } else {
                return(0)
            }
        }
    } else {
        i <- runif(n = 1, min = 0, max = 1)
        if( i <= p ) {
            return(1)
        } else {
            return(0)
        }
    }
}
