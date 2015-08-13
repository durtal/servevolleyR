library(shiny)

# Define UI for application that simulates tennis games:sets:matches
shinyUI(fluidPage(

    # Application title
    headerPanel("servevolleyR"),
    p("This app simulates tennis games, sets, and matches, between two players using the", a("servevolleyR", href = "https://github.com/durtal/servevolleyR"), "package."),
    p("The sliders under Players A and B are the probability that the player will win a point on their first Serve (top slider), the probability that the player will win a point on their second serve, and the probability that the players first serve will be in.  Changing these sliders wll automatically update the plots underneath, showing the probability that the player will win a game on their serve (and the score of each game, 5-3 is effectively winning after deuce).", style = "font-size: 11px"),
    p("To simulate sets and matches, then the simulate Button in the middle panel needs to be clicked, simulations are limited to 500 (in order to keep the app relatively speedy), and are based on Player A opening the serving in both the set simulations and match simulations.", style = "font-size: 11px"),

    fluidRow(

        column(4, align = "center",
               h3("Player A", align = "center"),
               sliderInput("pA",
                           "P(win pt on 1st serve)",
                           min = 0.01, max = 0.99, value = 0.5),
               sliderInput("p2A",
                           "P(win pt on 2nd serve)",
                           min = 0.01, max = 0.99, value = 0.5),
               sliderInput("firstServeA",
                           "P(1st serve in)",
                           min = 0.01, max = 0.99, value = 0.5),
               plotOutput("PlayerA")
               ),
        column(4, align = "center",
               h3("Match and Set params", align = "center"),
               radioButtons("Sets", label = "No. of Sets",
                            choices = list("3 Sets" = 3, "5 Sets" = 5),
                            selected = 3),
#                numericInput(inputId = "pAsets", label = "Player A Sets",
#                             value = 0, min = 0, max = 3),
#                numericInput(inputId = "pBsets", label = "Player B Sets",
#                             value = 0, min = 0, max = 3),
               actionButton("simulate", label = "Simulate Match"),
               plotOutput("sets"),
               plotOutput("matches")
               ),
        column(4, align = "center",
               h3("Player B", align = "center"),
               sliderInput("pB",
                           "P(win pt on 1st serve)",
                           min = 0.01, max = 0.99, value = 0.5),
               sliderInput("p2B",
                           "P(win pt on 2nd serve)",
                           min = 0.01, max = 0.99, value = 0.5),
               sliderInput("firstServeB",
                           "P(1st Serve In)",
                           min = 0.01, max = 0.99, value = 0.5),
               plotOutput("PlayerB")
               )
    )
))
