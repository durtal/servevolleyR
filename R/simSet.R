#' simSet
#'
#' @description simulate a set between two players, using the probability each will
#' win a point on their serve
#'
#' @param pA probability of player A winning point on their first serve
#' @param pB probability of player B winning point on their first serve
#' @param playTiebreak logical, play tiebreak at 6 games all
#' @param players player names, vector of length 2
#' @param detail return detailed data for set, default FALSE
#' @param p2A probability of player A winning point on their second serve
#' @param firstServeA probability of player A getting their first serve in
#' @param p2B probability of player B winning point on their second serve
#' @param firstServeB probability of player B getting their first serve in
#'
#' @return 1 (if player A wins), 0 (if player B wins). If the parameter \strong{detail}
#' is set to TRUE, then the function will returna detailed list about the simulated
#' set, including data about the indvidual games in that set.  This list can be
#' converted to a dataframe using \link{simDf}. See \link{simSets} for simulating
#' many sets between the two players
#'
#' @details minimum input required is the probability that each player will win
#' a point on their serve, however additional parameters include probability
#' of each player winning a point on their second serve, the probability of a
#' players first serve being in, and whether they play a tiebreak
#'
#' @export
simSet <- function(pA, pB, playTiebreak = TRUE, players = c("A", "B"), detail = FALSE,
                   p2A = NULL, firstServeA = NULL, p2B = NULL, firstServeB = NULL) {
    # game scores
    a <- 0
    b <- 0
    # --------------------------------------------------------------------------
    # start list to return detailed data
    result <- list()
    class(result) <- c(class(result), "svR_set")
    result$probs$playerA <- list(player = players[1],
                                 p = pA,
                                 p2 = ifelse(is.null(p2A), NA, p2A),
                                 firstServe = ifelse(is.null(firstServeA), NA, firstServeA))
    result$probs$playerB <- list(player = players[2],
                                 p = pB,
                                 p2 = ifelse(is.null(p2B), NA, p2B),
                                 firstServe = ifelse(is.null(firstServeB), NA, firstServeB))

    # --------------------------------------------------------------------------
    # return custom list with detailed results from service games
    series <- plyr::rlply(.n = 6, {

        playerA <- simGame(p = pA, p2 = p2A, firstServe = firstServeA, detail = TRUE, player = players[1])
        playerB <- simGame(p = pB, p2 = p2B, firstServe = firstServeB, detail = TRUE, player = players[2])

        return(list(playerA, playerB))
    })
    # flatten list of length 6x2 into 12x1 list
    series <- do.call(c, series)

    # --------------------------------------------------------------------------
    # start counter to tick through the games in series, and log server
    gameNo <- 1
    server <- c()
    while(TRUE) {
        # extract current game, add current server
        game <- series[[gameNo]]
        # update server log, and scores
        if((gameNo %% 2) == 1) {
            server <- c(server, players[1])
            if(game$result == 1) {
                a <- a + 1
            } else {
                b <- b + 1
            }
        } else {
            server <- c(server, players[2])
            if(game$result == 1) {
                b <- b + 1
            } else {
                a <- a + 1
            }
        }
        # update results list
        result$server <- server
        result$set$playerA <- a
        result$set$playerB <- b
        result$games[[gameNo]] <- game

        # increment game counter
        gameNo <- gameNo + 1

        # ----------------------------------------------------------------------
        # check scores
        if((a >= 6) && ((a - b) >= 2)) {
            result$result <- 1
            if(detail) {
                return(result)
            } else {
                return(1)
            }
        } else if((b >= 6) && ((b - a) >= 2)) {
            result$result <- 0
            if(detail) {
                return(result)
            } else {
                return(0)
            }
        }

        # ----------------------------------------------------------------------
        # if score is 6 all, play a tiebreak or keep playing
        if(a == 6 && b == 6 && playTiebreak) {

            tiebreak <- simTiebreak(pA = pA, pB = pB, p2A = p2A,
                                    firstServeA = firstServeA, p2B = p2B,
                                    firstServeB = firstServeB)
            result$server <- c(result$server, players[1])

            if(tiebreak == 1) {
                a <- a + 1
                result$set$playerA <- a
                result$result <- 1
                if(detail) {
                    return(result)
                } else {
                    return(1)
                }
            } else {
                b <- b + 1
                result$set$playerB <- b
                result$result <- 0
                if(detail) {
                    return(result)
                } else {
                    return(0)
                }
            }
        } else if(a == 6 && b == 6 && !playTiebreak) {

            while(TRUE) {

                playerA <- simGame(p = pA, p2 = p2A, firstServe = firstServeA, detail = TRUE, player = players[1])
                result$server <- c(result$server, players[1])
                playerB <- simGame(p = pB, p2 = p2B, firstServe = firstServeB, detail = TRUE, player = players[2])
                result$server <- c(result$server, players[2])
                if(playerA$result == 1 && playerB$result == 0) {
                    a <- a + 2
                } else if(playerA$result == 1 && playerB$result == 1) {
                    a <- a + 1
                    b <- b + 1
                } else {
                    b <- b + 2
                }

                result$set$playerA <- a
                result$set$playerB <- b

                if((a >= 6) && ((a - b) >= 2)) {
                    result$result <- 1
                    if(detail) {
                        return(result)
                    } else {
                        return(1)
                    }
                } else if((b >= 6) && ((b - a) >= 2)) {
                    result$result <- 0
                    if(detail) {
                        return(result)
                    } else {
                        return(0)
                    }
                }
            }
        }
    }
}

#' print method for detailed return of \link{simSet}
#' @export
print.svR_set <- function(x) {
    object <- x

    result <- paste("Set Result:\n\t",
                    object$probs$playerA$player, ":\t\t", object$set$playerA, "\n\t",
                    object$probs$playerB$player, ":\t\t", object$set$playerB, "\n",
                    ifelse(object$result == 1, paste(object$probs$playerA, "won the set.\n"),
                           paste(object$probs$playerB, "won the set.\n")), sep = "")
    server <- paste("Server order: ", paste(object$server, sep = "", collapse = ", "), sep = "")

    cat(result)
    cat(server)
}

#' summary method for detailed return of \link{simSet}
#' @export
summary.svR_set <- function(x) {
    object <- x

    result <- paste("Set Result:\n\t",
                    object$probs$playerA$player, ":\t\t", object$set$playerA, "\n\t",
                    object$probs$playerB$player, ":\t\t", object$set$playerB, "\n",
                    ifelse(object$result == 1, paste(object$probs$playerA, "won the set.\n\n"),
                           paste(object$probs$playerB, "won the set.\n\n")), sep = "")

    server <- paste("Server order:\t", paste(object$server, sep = "", collapse = ", "), "\n\n", sep = "")

    probs <- rbind(data.frame(object$probs$playerA), data.frame(object$probs$playerB))
    cat(result)
    cat(server)
    print(probs, row.names = FALSE)
}

#' simSets
#'
#' @description simulate many sets between two players, given the probability of
#' each player winning a point on their serve
#'
#' @param n number of simulations, default of 1000
#' @param pA probability of player A winning point on their first serve
#' @param pB probability of player B winning point on their first serve
#' @param playTiebreak logical, play tiebreak at 6 games all
#' @param players player names, vector of length 2
#' @param p2A probability of player A winning point on their second serve
#' @param firstServeA probability of player A getting their first serve in
#' @param p2B probability of player B winning point on their second serve
#' @param firstServeB probability of player B getting their first serve in
#' @param .progress \link{plyr}'s progress bar
#'
#' @return The function returns a large list, which can be printed, summarised, or
#' plotted. It can also be converted to a dataframe using \link{simDf}, which contains
#' data about the simulated sets, and games, within each simulation.
#'
#' @details minimum input required is the probability that each player will win
#' a point on their serve, however additional parameters include probability
#' of each player winning a point on their second serve, the probability of a
#' players first serve being in, and whether they play a tiebreak.
#'
#' Adding a parameter ("text", or "time") to the .progress argument will
#' cause two progress bars to show up, one after another, the first charts the
#' simulation of many games, while the second processes these results into something
#' more useful for users.
#'
#' @export
simSets <- function(n = 1000, pA, pB, playTiebreak = TRUE, players = c("A", "B"),
                    p2A = NULL, firstServeA = NULL, p2B = NULL, firstServeB = NULL,
                    .progress = "none") {

    # simulate many sets
    simulatedSets <- plyr::rlply(.n = n, {

        simSet(pA = pA, pB = pB, playTiebreak = playTiebreak, players = players, detail = TRUE,
               p2A = p2A, firstServeA = firstServeA, p2B = p2B, firstServeB = firstServeB)
    }, .progress = .progress)

    # --------------------------------------------------------------------------
    # start building list to return
    simSets <- list()
    class(simSets) <- c(class(simSets), "svR_sets")
    # add dataframe and list (not ideal to have duplicate data, see above)
    # simSets$dataframe <- simulatedDf
    simSets$sets <- simulatedSets
    # add results
    res <- sapply(simSets$sets, function(x) x$result)
    simSets$results$playerA <- sum(res)
    simSets$results$pct <- sum(res) / n
    simSets$results$playerB <- n - simSets$results$playerA
    # add details about simulation
    simSets$sim$n <- n
    simSets$sim$playerA <- list(player = players[1],
                                p = pA, p2 = ifelse(is.null(p2A), NA, p2A),
                                firstServe = ifelse(is.null(firstServeA), NA, firstServeA))
    simSets$sim$playerB <- list(player = players[2],
                                p = pB, p2 = ifelse(is.null(p2B), NA, p2B),
                                firstServe = ifelse(is.null(firstServeB), NA, firstServeB))

    return(simSets)
}

#' print method for detailed return of \link{simSets}
#' @export
print.svR_sets <- function(x) {
    object <- x

    about <- paste("\nSimulation of ", object$sim$n, " sets:\n\nServer Probabilities:\n", sep = "")
    details <- rbind(data.frame(object$sim$playerA), data.frame(object$sim$playerB))
    results <- sapply(object$sets, function(x) x$result)
    results <- paste("\nPlayer A (", object$sim$playerA$player, ") won ",
                     (sum(results) / length(results)), " of sets.\n", sep = "")

    cat(about)
    print(details, row.names = FALSE)
    cat(results)
}

#' summary method for detailed return of \link{simSets}
#' @export
summary.svR_sets <- function(x) {
    object <- x

    about <- paste("\nSimulation of ", object$sim$n, " sets:\n\nServer Probabilities:\n", sep = "")
    details <- rbind(data.frame(object$sim$playerA), data.frame(object$sim$playerB))
    result <- sapply(object$sets, function(x) x$result)
    result <- paste("\nPlayer A (", object$sim$playerA$player, ") won ",
                    (sum(result) / length(result)), " of sets.\n", sep = "")

    results <- plyr::ldply(1:length(object$sets), .fun = function(ind, sets) {
        x <- sets[[ind]]
        data.frame(simNo = ind,
                   playerA = x$probs$playerA$player,
                   playerB = x$probs$playerB$player,
                   result = x$result,
                   scoreA = x$set$playerA,
                   scoreB = x$set$playerB
        )
    }, sets = object$sets)
    results$scoreA[which(results$result == 1 & results$scoreA > 10)] <- 10
    results$scoreB[which(results$result == 1 & results$scoreB > 8)] <- 8
    results$scoreA[which(results$result == 0 & results$scoreA > 8)] <- 8
    results$scoreB[which(results$result == 0 & results$scoreB > 10)] <- 10
    results <- table(playerA = results$scoreA, playerB = results$scoreB) / sum(table(results$scoreA, results$scoreB))
    results[results == 0] <- NA

    cat(about)
    print(details, row.names = FALSE)
    cat(result)
    print(results)
}

#' plot method for detailed return of \link{simSets}
#' @export
plot.svR_sets <- function(x) {
    object <- x

    results <- plyr::ldply(1:length(object$sets), .fun = function(ind, sets) {
        x <- sets[[ind]]
        data.frame(simNo = ind,
                   playerA = x$probs$playerA$player,
                   playerB = x$probs$playerB$player,
                   result = x$result,
                   scoreA = x$set$playerA,
                   scoreB = x$set$playerB
        )
    }, sets = object$sets)
    results$scoreA[which(results$result == 1 & results$scoreA > 10)] <- 10
    results$scoreB[which(results$result == 1 & results$scoreB > 8)] <- 8
    results$scoreA[which(results$result == 0 & results$scoreA > 8)] <- 8
    results$scoreB[which(results$result == 0 & results$scoreB > 10)] <- 10
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
                      title = paste(object$sim$n, " Set Simulations: Player A (",
                                    object$sim$playerA$player, ") wins ",
                                    round(object$results$pct, 3), sep = ""))

}
