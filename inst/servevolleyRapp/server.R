library(shiny)
library(servevolleyR)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

    rv <- reactiveValues(matches = NULL,
                         totalSets = 3,
                         pAsets = 0,
                         pBsets = 0,
                         setScore = c(0, 0))

    observeEvent(input$Sets, {
        rv$totalSets <- as.numeric(input$Sets)
        tmp <- floor(rv$totalSets / 2)
        output$pASets <- renderUI({
            radioButtons(inputId = "pASets", label = "Player A Sets",
                         choices = 0:tmp, selected = 0)
        })
        output$pBSets <- renderUI({
            radioButtons(inputId = "pBSets", label = "Player B Sets",
                         choices = 0:tmp, selected = 0)
        })
    })
    observeEvent(input$pASets, {
        rv$pASets <- as.numeric(input$pASets)
        rv$setScore <- c(rv$pASets, rv$pBSets)
    })
    observeEvent(input$pBSets, {
        rv$pBSets <- as.numeric(input$pBSets)
        rv$setScore <- c(rv$pASets, rv$pBSets)
    })
    observeEvent(input$simulate,
                 {
                     rv$matches <- simMatches(500, sets = rv$totalSets, currentScore = rv$setScore,
                                        pA = input$pA, p2A = input$p2A, firstServeA = input$firstServeA,
                                        pB = input$pB, p2B = input$p2B, firstServeB = input$firstServeB)
                     rv$sets <- simSets(500,
                                        pA = input$pA, p2A = input$p2A, firstServeA = input$firstServeA,
                                        pB = input$pB, p2B = input$p2B, firstServeB = input$firstServeB)
                 })

    output$PlayerA <- renderPlot({

        games <- simGames(n = 1e3, p = input$pA,
                          p2 = input$p2A,
                          firstServe = input$firstServeA)
        plot(games)
    }, bg = "#fcfcfc")

    output$PlayerB <- renderPlot({

        games <- simGames(n = 1e3, p = input$pB,
                          p2 = input$p2B,
                          firstServe = input$firstServeB)
        plot(games)
    }, bg = "#fcfcfc")

    output$sets <- renderPlot({

        if(input$simulate) {
            plot(rv$sets)
        }
    }, bg = "#fcfcfc")

    output$matches <- renderPlot({

        if(input$simulate) {
            plot(rv$matches)
        }
    }, bg = "#fcfcfc")
})
