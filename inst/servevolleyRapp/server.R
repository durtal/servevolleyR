library(shiny)
library(servevolleyR)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

    rv <- reactiveValues(matches = NULL,
                         sets = NULL)
    observeEvent(input$simulate,
                 {
                     rv$matches <- simMatches(500, sets = as.numeric(input$Sets),
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
    })

    output$PlayerB <- renderPlot({

        games <- simGames(n = 1e3, p = input$pB,
                          p2 = input$p2B,
                          firstServe = input$firstServeB)
        plot(games)
    })

    output$sets <- renderPlot({

        if(input$simulate) {
            plot(rv$sets)
        }
    })

    output$matches <- renderPlot({

        if(input$simulate) {
            plot(rv$matches)
        }
    })
})
