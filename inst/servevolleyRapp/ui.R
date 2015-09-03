library(shiny)

# Define UI for application that simulates tennis games:sets:matches
shinyUI(fluidPage(
    theme = "styles.css",

    headerPanel("servevolleyR"),

    fluidRow(
        column(1),
        column(
            9, align = "center",
            h4("This app simulates tennis games, sets, and matches, between two players using the", a("servevolleyR", href = "https://github.com/durtal/servevolleyR"), "package."),
            p("The sliders under Players A and B are the probability that the player will win a point on their first Serve (top slider), the probability that the player will win a point on their second serve, and the probability that the players first serve will be in.  Changing these sliders wll automatically update the plots underneath, showing the probability that the player will win a game on their serve (and the score of each game, 5-3 is effectively winning after deuce)."),
            p("To simulate sets and matches, then the simulate Button in the middle panel needs to be clicked, simulations are limited to 500 (in order to keep the app relatively speedy), and are based on Player A opening the serving in both the set simulations and match simulations.")
        ),
        column(1)
    ),

    fluidRow(
        column(
            4, align="center",

            wellPanel(
                h4("Player A"),
                sliderInput("pA",
                            "P(win pt on 1st Serve)",
                            min = 0.01, max = 0.99, value = 0.5),
                sliderInput("p2A",
                            "P(win pt on 2nd serve)",
                            min = 0.01, max = 0.99, value = 0.5),
                sliderInput("firstServeA",
                            "P(1st serve in)",
                            min = 0.01, max = 0.99, value = 0.5)
            ),
            wellPanel(
                h4("Player A Service Games"),
                plotOutput("PlayerA")
            )
        ),
        column(
            4, align = "center",
            wellPanel(
                h4("Match and Set params", align = "center"),
                radioButtons("Sets", label = "No. of Sets",
                             choices = list("3 Sets" = 3, "5 Sets" = 5),
                             selected = 3),
                fluidRow(
                    column(6,
                           uiOutput("pASets")
                    ),
                    column(6,
                           uiOutput("pBSets")
                    )
                ),
                actionButton("simulate", label = "Simulate Match")
            ),
            wellPanel(
                h4("Set Simulations (Player A serving first)"),
                plotOutput("sets")
            ),
            wellPanel(
                h4("Match Simulations (Player A serving first)"),
                plotOutput("matches")
            )
        ),
        column(
            4, align = "center",
            wellPanel(
                h4("Player B"),
                sliderInput("pB",
                            "P(win pt on 1st Serve)",
                            min = 0.01, max = 0.99, value = 0.5),
                sliderInput("p2B",
                            "P(win pt on 2nd serve)",
                            min = 0.01, max = 0.99, value = 0.5),
                sliderInput("firstServeB",
                            "P(1st serve in)",
                            min = 0.01, max = 0.99, value = 0.5)
            ),
            wellPanel(
                h4("Player B Service Games"),
                plotOutput("PlayerB")
            )
        )
    )
))
