#' simDf
#'
#' @description convert simulated games, sets and matches into useful dataframes
#' for further analysis
#'
#' @param sim detailed list returned by one of the following functions \link{simGame}
#' \link{simGames}, \link{simSet}, \link{simSets}, \link{simMatch} or \link{simMatches}
#' @param ... recommend adding \link{plyr}'s .progress when using \link{simGames},
#' \link{simSets} or \link{simMatches} to view progress at processing results.
#'
#' @export
simDf <- function(x, ...) {
    UseMethod(generic = "simDf", x)
}

#' simDf method for svR_game
#' @export
simDf.svR_game <- function(x) {
    object <- x

    df <- data.frame(player = object$player,
                     object$probs,
                     result = object$result,
                     object$points,
                     stringsAsFactors = FALSE)
    return(df)
}

#' simDf method for svR_games
#' @export
simDf.svR_games <- function(x, ...) {
    object <- x
    df <- plyr::ldply(1:length(object$games), .fun = function(x, games) {

        game <- games[[x]]
        df <- simDf(game)
        df <- data.frame(simNo = x, df)

        return(df)
    }, games = object$games, ...)

    return(df)
}

#' simDf method for svR_set
#' @export
simDf.svR_set <- function(x) {
    object <- x

    games <- plyr::ldply(1:length(object$games), .fun = function(x, games) {

        game <- games[[x]]
        df <- simDf(game)
        df <- data.frame(gameNo = x, df)

        return(df)
    }, games = object$games)
    names(games)[c(2,6)] <- c("serving", "game_res")
    scores <- data.frame(pA = object$probs$playerA$player,
                         pB = object$probs$playerB$player,
                         setA = object$set$playerA,
                         setB = object$set$playerB,
                         set_res = object$result,
                         stringsAsFactors = FALSE)
    df <- cbind(scores, games)
    # this is a bit hacky, for sets that have a tiebreak
    # (simTiebreak only returns 1 or 0 at the moment)
    if(max(df$gameNo) == 12) {
        df <- rbind(df, df[1,])
        df$gameNo[13] <- "13"
        df[13, c(8, 9, 10, 11, 12, 13)] <- NA
        if(df$setA[13] > df$setB[13]) {
            df$game_res[13] <- 1
        } else {
            df$game_res[13] <- 0
        }
    }
    return(df)
}

#' simDf method for svR_sets
#' @export
simDf.svR_sets <- function(x, ...) {
    object <- x

    df <- plyr::ldply(1:length(object$sets), .fun = function(x, sets) {

        Set <- sets[[x]]
        df <- simDf(Set)
        df <- data.frame(simNo = x, df)

        return(df)
    }, sets = object$sets, ...)
    return(df)
}

#' simDf method for svR_match
#' @export
simDf.svR_match <- function(x) {
    object <- x

    sets <- plyr::ldply(1:length(object$sets), .fun = function(x, sets) {
        Set <- sets[[x]]
        df <- simDf(Set)
        df <- data.frame(setNo = x, df)
        return(df)

    }, sets = object$sets)

    df <- data.frame(playerA = object$probs$playerA$player,
                     playerB = object$probs$playerB$player,
                     mA = object$match$playerA,
                     mB = object$match$playerB,
                     result = object$result, stringsAsFactors = FALSE)
    df <- cbind(df, sets)
    return(df)
}

#' simDf method for svR_matches
#' @export
simDf.svR_matches <- function(x, ...) {
    object <- x

    df <- plyr::ldply(1:length(object$matches), .fun = function(x, matches) {

        simmatch <- matches[[x]]
        df <- simDf(simmatch)
        df <- data.frame(simNo = x, df)

        return(df)
    }, matches = object$matches, ...)
    return(df)
}
