#' simMatch
#'
#' @description simulate a match between two players
#'
#' @param pA probability of player A winning point on their serve
#' @param pB probability of player B winning point on their serve
#' @param sets number of sets to be played
#' @param tiebreaks play tie break at 6-6, or keep playing
#' @param finalSetTiebreak play tie break at 6-6 in final set
#' @param players player names, vector of length 2
#' @param detail return detailed data for the Match, default FALSE
#' @param p2A probability of player A winning point on their second serve
#' @param firstServeA probability of player A getting their first serve in
#' @param p2B probability of player B winning point on their second serve
#' @param firstServeB probability of player B getting their first serve in
#'
#' @export
simMatch <- function(pA, pB, sets = c(3, 5), tiebreaks = TRUE, finalSetTiebreak = FALSE,
                     players = c("A", "B"), detail = FALSE, p2A = NULL,
                     firstServeA = NULL, p2B = NULL, firstServeB = NULL) {

    # set scores
    a <- 0
    b <- 0
    # --------------------------------------------------------------------------
    # start list to return detailed data
    result <- list()
    class(result) <- c(class(result), "svR_match")
    result$probs$playerA <- list(player = players[1],
                                 p = pA,
                                 p2 = ifelse(is.null(p2A), NA, p2A),
                                 firstServe = ifelse(is.null(firstServeA), NA, firstServeA))
    result$probs$playerB <- list(player = players[2],
                                 p = pB,
                                 p2 = ifelse(is.null(p2B), NA, p2B),
                                 firstServe = ifelse(is.null(firstServeB), NA, firstServeB))

    setNo <- 1
    server <- players[1]

    while(TRUE) {

        if(setNo < sets) {
            # simulate set based on current server
            if(server == players[1]) {
                setResult <- simSet(pA = pA, pB = pB, playTiebreak = tiebreaks,
                                    players = players, detail = TRUE, p2A = p2A,
                                    firstServeA = firstServeA, p2B = p2B, firstServeB = firstServeB)
                # increment results
                if(setResult$result == 1) {
                    a <- a + 1
                } else {
                    b <- b + 1
                }
            } else {
                # if playerB is next server then their probs go in player A
                setResult <- simSet(pA = pB, pB = pA, playTiebreak = tiebreaks,
                                    players = rev(players), detail = TRUE, p2A = p2B,
                                    firstServeA = firstServeB, p2B = p2A, firstServeB = firstServeA)
                # increment results
                if(setResult$result == 1) {
                    b <- b + 1
                } else {
                    a <- a + 1
                }
            }
            # final set out come (conditioned on number of sets played being 1 less than sets)
        } else {
            # use finalSetTiebreak argument
            if(server == players[1]) {
                setResult <- simSet(pA = pA, pB = pB, playTiebreak = finalSetTiebreak,
                                    players = players, detail = TRUE, p2A = p2A,
                                    firstServeA = firstServeA, p2B = p2B, firstServeB = firstServeB)
                if(setResult$result == 1) {
                    a <- a + 1
                } else {
                    b <- b + 1
                }
            } else {
                setResult <- simSet(pA = pB, pB = pA, playTiebreak = finalSetTiebreak,
                                    players = rev(players), detail = TRUE, p2A = p2B,
                                    firstServeA = firstServeB, p2B = p2A, firstServeB = firstServeA)
                if(setResult$result == 1) {
                    b <- b + 1
                } else {
                    a <- a + 1
                }
            }
        }

        # add set result to detailedlist
        result$sets[[setNo]] <- setResult
        # update sets count
        result$match$playerA <- a
        result$match$playerB <- b

        # update server for next iteration
        lastserver <- setResult$server[length(setResult$server)]
        server <- players[players != lastserver]
        setNo <- setNo + 1


        if(a == ceiling(sets/2)) {
            if(detail) {
                result$result <- 1
                return(result)
            }
            return(1)
        } else if(b == ceiling(sets/2)) {
            if(detail) {
                result$result <- 0
                return(result)
            }
            return(0)
        }
    }
}

#' print method for detailed return of \link{simMatch}
#' @export
print.svR_match <- function(x) {
    object <- x

    result <- paste("Match Result:\n\t",
                    ifelse(object$result == 1, paste(object$probs$playerA$player, "won the match.\n\n"),
                           paste(object$probs$playerB$player, "won the match.\n\n")), sep = "")

    df <- data.frame(player = c(object$probs$playerA$player, object$probs$playerB$player),
                     sets = c(object$match$playerA, object$match$playerB))
    for(s in 1:length(object$sets)) {
        tmp <- data.frame(player = c(object$sets[[s]]$probs$playerA$player,
                                     object$sets[[s]]$probs$playerB$player),
                          set = c(object$sets[[s]]$set$playerA,
                                  object$sets[[s]]$set$playerB))
        names(tmp) <- c("player", paste0("set", s))
        df <- merge(df, tmp, by = "player")
    }

    cat(result)
    print(df, row.names = FALSE)
}

#' summary method for detailed return of \link{simMatch}
#' @export
summary.svR_match <- function(x) {
    object <- x

    result <- paste("Match Result:\n\t",
                    ifelse(object$result == 1, paste(object$probs$playerA$player, "won the match.\n\n"),
                           paste(object$probs$playerB$player, "won the match.\n\n")), sep = "")

    results <- data.frame(player = c(object$probs$playerA$player, object$probs$playerB$player),
                          sets = c(object$match$playerA, object$match$playerB))
    for(s in 1:length(object$sets)) {
        tmp <- data.frame(player = c(object$sets[[s]]$probs$playerA$player,
                                     object$sets[[s]]$probs$playerB$player),
                          set = c(object$sets[[s]]$set$playerA,
                                  object$sets[[s]]$set$playerB))
        names(tmp) <- c("player", paste0("set", s))
        df <- merge(df, tmp, by = "player")
    }

    probs <- rbind(data.frame(object$probs$playerA), data.frame(object$probs$playerB))

    cat(result)
    print(results, row.names = FALSE)
    cat("\nServer probabilities:\n\n")
    print(probs, row.names = FALSE)
}

#' simMatches
#'
#' @description simulate a set between two players
#'
#' @param n number of simulations, default 1000
#' @param pA probability of player A winning point on their serve
#' @param pB probability of player B winning point on their serve
#' @param sets number of sets to be played
#' @param tiebreaks play tie break at 6-6, or keep playing
#' @param finalSetTiebreak play tie break at 6-6 in final set
#' @param players player names, vector of length 2
#' @param detail return detailed data for the Match, default FALSE
#' @param p2A probability of player A winning point on their second serve
#' @param firstServeA probability of player A getting their first serve in
#' @param p2B probability of player B winning point on their second serve
#' @param firstServeB probability of player B getting their first serve in
#' @param .progress \link{plyr}'s progress bar
#'
#' @export
simMatches <- function(n = 1000, pA, pB, sets = c(3, 5), tiebreaks = TRUE, finalSetTiebreak = FALSE,
                       players = c("A", "B"), p2A = NULL, firstServeA = NULL,
                       p2B = NULL, firstServeB = NULL, .progress = "none") {

    # simulate many matches
    simulatedMatches <- plyr::rlply(.n = n, {

        simMatch(pA = pA, pB = pB, sets = sets, tiebreaks = tiebreaks,
                 finalSetTiebreak = finalSetTiebreak, players = players, detail = TRUE,
                 p2A = p2A, firstServeA = firstServeA, p2B = p2B, firstServeB = firstServeB)

    }, .progress = .progress)

    # --------------------------------------------------------------------------
    # start building list to return
    simMatches <- list()
    class(simMatches) <- c(class(simMatches), "svR_matches")
    # add list of simulated matches
    simMatches$matches <- simulatedMatches
    # add result
    res <- sapply(simMatches$matches, function(x) x$result)
    simMatches$results$playerA <- sum(res)
    simMatches$results$pct <- sum(res) / n
    simMatches$results$playerB <- n - simMatches$results$playerA
    # add details about simulation
    simMatches$sim$n <- n
    simMatches$sim$playerA <- list(player = players[1],
                                   p = pA, p2 = ifelse(is.null(p2A), NA, p2A),
                                   firstServe = ifelse(is.null(firstServeA), NA, firstServeA))
    simMatches$sim$playerB <- list(player = players[2],
                                   p = pB, p2 = ifelse(is.null(p2B), NA, p2B),
                                   firstServe = ifelse(is.null(firstServeB), NA, firstServeB))
    return(simMatches)
}

#' print method for detailed return of \link{simMatches}
#' @export
print.svR_matches <- function(x) {
    object <- x

    about <- paste("\nSimulation of ", object$sim$n, " matches:\n\n", sep = "")
    results <- paste("Player A (", object$sim$playerA$player, ") won ", object$results$pct,
                     " of matches.\n\nServer Probabilities:\n", sep = "")
    details <- rbind(data.frame(object$sim$playerA), data.frame(object$sim$playerB))

    cat(about)
    cat(results)
    print(details, row.names = FALSE)
}

#' summary method for detailed return of \link{simMatches}
#' @export
summary.svR_matches <- function(x) {
    object <- x

    about <- paste("\nSimulation of ", object$sim$n, " matches:\n\n", sep = "")
    result <- paste("Player A (", object$sim$playerA$player, ") won ", object$results$pct,
                    " of matches.\n\nServer Probabilities:\n", sep = "")
    details <- rbind(data.frame(object$sim$playerA), data.frame(object$sim$playerB))

    results <- plyr::ldply(1:length(object$matches), .fun = function(ind, matches) {
        x <- matches[[ind]]
        data.frame(simNo = ind,
                   playerA = x$probs$playerA$player,
                   playerB = x$probs$playerB$player,
                   result = x$result,
                   scoreA = x$match$playerA,
                   scoreB = x$match$playerB)

    }, matches = object$matches)
    results <- table(playerA = results$scoreA, playerB = results$scoreB) / sum(table(results$scoreA, results$scoreB))
    results[results == 0] <- NA
    cat(about)
    cat(result)
    print(details, row.names = FALSE)
    cat("\n\n")
    print(results, row.names = FALSE)
}

#' plot method for detailed return of \link{simMatches}
#' @export
plot.svR_matches <- function(x) {
    object <- x

    results <- plyr::ldply(1:length(object$matches), .fun = function(ind, matches) {
        x <- matches[[ind]]
        data.frame(simNo = ind,
                   playerA = x$probs$playerA$player,
                   playerB = x$probs$playerB$player,
                   result = x$result,
                   scoreA = x$match$playerA,
                   scoreB = x$match$playerB)

    }, matches = object$matches)
    results <- table(playerA = results$scoreA, playerB = results$scoreB) / sum(table(results$scoreA, results$scoreB))
    results[results == 0] <- NA
    results <- as.data.frame(results)
    names(results)[3] <- "Prob"
    # plot using ggplot2
    ggplot2::ggplot(results, ggplot2::aes(x = playerA, y = playerB)) +
        ggplot2::geom_tile(data = subset(results, !is.na(Prob)), ggplot2::aes(fill = Prob)) +
        ggplot2::scale_fill_continuous(low = "lightblue", high = "#E50023", guide = FALSE, na.value = "transparent") +
        ggplot2::geom_text(data = subset(results, !is.na(Prob)), ggplot2::aes(label = round(Prob, 3)), size = 4.5) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = paste0("Player A (", object$sim$playerA$player, ")"),
                      y = paste0("Player B (", object$sim$playerB$player, ")"),
                      title = paste(object$sim$n, " Match Simulations: Player A (",
                                    object$sim$playerA$player, ") wins ",
                                    round(object$results$pct, 3), sep = ""))

}
