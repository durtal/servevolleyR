#' Simulate a server winning a game
#'
#' @description simulate probability of a server winning a game given a number of
#' parameters, that include probability of server winning a point on their first
#' serve, their second serve, and probability of their first serve going in.
#'
#' @param p probability of server winning point on their first serve
#' @param p2 probability of server winning point on their second serve
#' @param firstServe probability of first serve being in
#' @param detail return detailed output, default of FALSE returns 1 or 0, 1 if
#' server wins, 0 otherwise
#' @param player player name, default is "A"
#'
#' @details if the parameter \strong{detail} is set to TRUE, then the function
#' will return a detailed list about the simulated game, see \link{simGames} for
#' more about simulating many games for a single server
#'
#' @export
simGame <- function(p, p2 = NULL, firstServe = NULL, detail = FALSE, player = "A") {
    # player points at start of game
    a <- 0
    b <- 0

    #---------------------------------------------------------------------------
    # return custom list with detailed results from service game (points won, etc)
    result <- list()
    class(result) <- c(class(result), "svR_game")
    result$player <- player
    result$probs <- list(p = p,
                         p2 = ifelse(is.null(p2), NA, p2),
                         firstServe = ifelse(is.null(firstServe), NA, firstServe))
    result$points$server <- a
    result$points$returner <- b

    while(TRUE) {

        point <- simPoint(p = p, p2 = p2, firstServe = firstServe)
        if(point == 1) {
            a <- a + 1
            result$points$server <- a
        } else {
            b <- b + 1
            result$points$returner <- b
        }

        if((a >= 4) && ((a - b) >= 2)) {

            if(detail) {
                result$result <- 1
                return(result)
            } else {
                return(1)
            }

        } else if ((b >= 4) && ((b - a) >= 2)) {

            if(detail) {
                result$result <- 0
                return(result)
            } else {
                return(0)
            }
        }
    }
}

#' print method for detailed return of \link{simGame}
#' @export
print.svR_game <- function(x, ...) {
    object <- x
    results <- data.frame(player = c("server", "returner"),
                          points = c(object$points$server, object$points$returner))

    result <- paste("Service Game Result\n\n",
                    ifelse(object$result == 1, "Server won the game.\n", "Returner won the game.\n"),
                    sep = "")
    cat(result)
    print(results, row.names = FALSE)
}

#' summary method for detailed return of \link{simGame}
#' @export
summary.svR_game <- function(x, ...) {
    object <- x

    results <- data.frame(player = c("server", "returner"),
                          points = c(object$points$server, object$points$returner))
    result <- paste("Service Game Result:\n",
                    ifelse(object$result == 1, "Server won the game.\n", "Returner won the game.\n"),
                    sep = "")
    statement <- paste("\nProbability of Server (", object$player, ") winning point:\n",
                       sep = "")
    server <- data.frame(object$probs)

    cat(result)
    print(results, row.names = FALSE)
    cat(statement)
    print(server, row.names = FALSE)
}

#' simulate many Games
#'
#' @description simulate many service games for a single server
#'
#' @param n number of games to simulate (default, 1000)
#' @param p probability of server winning a point on the first serve
#' @param p2 probability of server winning a point on their second serve
#' @param firstServe probability of first serve being in
#' @param player player name, default is "A"
#' @param .progress \link{plyr}'s progress bar
#'
#' @details adding a parameter ("text", or "time") to the .progress argument will
#' cause two progress bars to show up, one after another, the first charts the
#' simulation of many games, while the second processes these results into something
#' more useful for users.
#'
#' @export
simGames <- function(n = 1000, p, p2 = NULL, firstServe = NULL, player = "A",
                     .progress = "none") {

    simulatedgames <- plyr::rlply(.n = n, {

        simGame(p = p, p2 = p2, firstServe = firstServe, detail = TRUE)

    }, .progress = .progress)

    simGames <- list()
    class(simGames) <- c(class(simGames), "svR_games")
    # add simulated games
    simGames$games <- simulatedgames
    # add results of simulations
    simGames$results$n <- n
    res <- sapply(simGames$games, function(x) x$result)
    simGames$results$server <- sum(res)
    simGames$results$pct <- sum(res) / n
    simGames$results$returner <- n - simGames$results$server
    # add server probs
    simGames$probs$p <- p
    simGames$probs$p2 <- ifelse(is.null(p2), NA, p2)
    simGames$probs$firstServe <- ifelse(is.null(firstServe), NA, firstServe)

    return(simGames)
}

#' print method for detailed return of \link{simGames}
#' @export
print.svR_games <- function(x) {
    object <- x

    about <- paste("\nSimulation of ", object$results$n, " service games:\n", sep = "")
    details <- paste("\nServer probabilities:\n", sep = "")
    server <- data.frame(object$probs)
    results <- paste("Server won ", object$results$pct, " (", object$results$server, "/",
                     object$results$n, ") of games.\n",
                     sep = "")
    cat(about)
    cat(results)
    cat(details)
    print(server, row.names = FALSE)
}

#' summary method for detailed return of \link{simGames}
#' @export
summary.svR_games <- function(x) {
    object <- x

    about <- paste("\nSimulation of ", object$results$n, " service games:\n", sep = "")
    details <- paste("\nServer probabilities:\n", sep = "")
    server <- data.frame(object$probs)
    result <- paste("Server won ", object$results$pct, " (", object$results$server, "/",
                     object$results$n, ") of games.\n",
                     sep = "")

        # summarise results
    results <- plyr::ldply(1:length(object$games), .fun = function(ind, games) {
        x <- games[[ind]]
        data.frame(simNo = ind,
                   result = x$result,
                   p = x$probs$p,
                   p2 = ifelse(is.null(x$probs$p2), NA, x$probs$p2),
                   firstServe = ifelse(is.null(x$probs$firstServe), NA, x$probs$firstServe),
                   server = x$points$server,
                   returner = x$points$returner)
    }, games = object$games)
    # reduce high game scores such as 10-8 to 5-3 (effectively the same)
    results$server[which(results$result == 1 & results$server > 5)] <- 5
    results$returner[which(results$result == 1 & results$returner > 3)] <- 3
    results$server[which(results$result == 0 & results$server > 3)] <- 3
    results$returner[which(results$result == 0 & results$returner > 5)] <- 5
    # make table
    results <- table(server = results$server, returner = results$returner) / sum(table(results$server, results$returner))
    results[results == 0] <- NA

    cat(about)
    cat(result)
    cat(details)
    print(server, row.names = FALSE)
    cat("\nService Game scores:\n")
    print(results)
}

#' plot method for return of \link{simGames}
#' @export
plot.svR_games <- function(x) {
    object <- x

    # summarise results
    results <- plyr::ldply(1:length(object$games), .fun = function(ind, games) {
        x <- games[[ind]]
        data.frame(simNo = ind,
                   result = x$result,
                   p = x$probs$p,
                   p2 = ifelse(is.null(x$probs$p2), NA, x$probs$p2),
                   firstServe = ifelse(is.null(x$probs$firstServe), NA, x$probs$firstServe),
                   server = x$points$server,
                   returner = x$points$returner)
    }, games = object$games)
    # reduce high game scores such as 10-8 to 5-3 (effectively the same)
    results$server[which(results$result == 1 & results$server > 5)] <- 5
    results$returner[which(results$result == 1 & results$returner > 3)] <- 3
    results$server[which(results$result == 0 & results$server > 3)] <- 3
    results$returner[which(results$result == 0 & results$returner > 5)] <- 5
    # make table
    results <- table(server = results$server, returner = results$returner) / sum(table(results$server, results$returner))
    results[results == 0] <- NA
    results <- as.data.frame(results)
    names(results)[3] <- "Prob"

    # plot results
    ggplot2::ggplot(results, ggplot2::aes(x = server, y = returner)) +
        ggplot2::geom_tile(data = subset(results, !is.na(Prob)), ggplot2::aes(fill = Prob)) +
        ggplot2::scale_fill_continuous(low = "lightblue", high = "#E50023", guide = FALSE, na.value = "transparent") +
        ggplot2::geom_text(data = subset(results, !is.na(Prob)), ggplot2::aes(label = round(Prob, 3)), size = 4.5) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = paste0("Server"),
                      y = paste0("Returner"),
                      title = paste(object$sim$n, " Game Simulations: Server wins ",
                                    round(object$results$pct, 3), sep = ""))
}
