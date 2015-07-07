#' simTiebreak
#'
#' @description simulate a tiebreak between two players
#'
#' @param pA probability of player A winning point on their serve
#' @param pB probability of player B winning point on their serve
#' @param p2A probability of player A winning point on their second serve
#' @param firstServeA probability of player A getting their first serve in
#' @param p2B probability of player B winning point on their second serve
#' @param firstServeB probability of player B getting their first serve in
#'
#' @return 1 (if player A wins), 0 (if player B wins)
#'
#' @export
simTiebreak <- function(pA, pB, p2A = NULL, firstServeA = NULL, p2B = NULL, firstServeB = NULL) {
    # player points
    a <- 0
    b <- 0

    # first point is one serve from Player A
    point <- simPoint(p = pA, p2 = p2A, firstServe = firstServeA)
    if(point == 1) {
        a <- a + 1
    } else {
        b <- b + 1
    }
    # Here on players serve two points each until a winner is decided
    # simulate a series of 2 serves each, logging results, inverting Player B's
    series <- replicate(50, {

        playerA <- replicate(2, simPoint(p = pA, p2 = p2A, firstServe = firstServeA))
        playerB <- replicate(2, simPoint(p = pB, p2 = p2B, firstServe = firstServeB))
        # switch player B's 1s to 0s
        playerB <- ifelse(playerB == 1, 0, 1)

        return(c(playerB, playerA))
    })
    # returns a matrix, convert to vector
    series <- as.vector(series)
    # set counter to track point in vector
    i <- 1
    while(TRUE) {
        # extract next point
        point <- series[i]
        if(point == 1) {
            a <- a + 1
        } else {
            b <- b + 1
        }
        # increase counter
        i <- i + 1

        if((a >= 7) && ((a - b) >= 2)) {
            return(1)
        } else if ((b >= 7) && ((b - a) >= 2)) {
            return(0)
        }
    }
}
