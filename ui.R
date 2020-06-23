# Load Required Packages
library(shiny)
library(shinydashboard)
library(png)
library(shinyBS)
library(V8)
library(shinyjs)
library(shinyWidgets)
library(boastUtils)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Bias and Reliability"
APP_DESCP  <<- paste(
  "The bias of a measurement describes to what degree it is systematically off 
  target from the true value. The reliability of a measurement describes how 
  consistent the measurement is when you repeat it. This app is designed to help 
  with understanding those concepts."
)
# End App Meta Data------------------------------------------------------------

#Use jscode to for reset button to reload the app
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"


ui <- list(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
    dashboardPage(
        skin = "red",
        dashboardHeader(
            title = "Bias & Reliability",
            titleWidth = 220,
            tags$li(
                class = "dropdown",
                tags$a(
                    href='https://shinyapps.science.psu.edu/',
                    icon("home")
                )
            )
        ),
        #adding prereq pages
        dashboardSidebar(
            width = 220,
            sidebarMenu(id='tabs',
                menuItem("Overview",tabName = "overview", icon = icon("dashboard")),
                menuItem("Prerequisites", tabName= "prerequisites", icon=icon("book")),
                menuItem("Game",tabName = "game", icon = icon("gamepad")),
                menuItem("References", tabName = "references", icon = icon("leanpub"))
            ),
            tags$div(
              class = "sidebar-logo",
              boastUtils::psu_eberly_logo("reversed")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "overview",
                    h1("Bias & Reliability"),
                    p("This app helps with understanding the concepts of bias and reliability."),
                    br(),
                    h2("Instructions"),
                    tags$ol(
                        tags$li("Click the target to put down at least 10 points."),
                        tags$li("Check the feedback and keep trying until you get it correct and go to the next question."),
                        tags$li('Note: There are only four different questions.')),
                    div(
                        style = "text-align: center",
                        bsButton(
                            inputId = "go",
                            label = "Go to Prerequisites",
                            icon = icon("bolt"),
                            style = "danger",
                            size = "large",
                            class="circle grow"
                        )
                    ),
                    br(),
                    br(),
                    h2("Acknowledgements"),
                    p("This app was developed and coded by Yuxin Zhang and updated by Luxin Wang, Thomas McIntyre and Chenese Gray.")
                ),
                        #Adding pre-requisites page to remove background from instructions page
                tabItem(
                    tabName = "prerequisites",
                    h3("Review the following concepts: "),
                    br(),
                    tags$ul(
                        tags$li("The bias of a measurement describes to what degree it is systematically off target from the true value.
                        While playing with the app you should think of what gives you a high or low bias."),
                        tags$li("The reliability of a measurement describes how consistent the measurement is when you repeat it 
                        (alternatively, an unreliable measurement is one that shows a lot of variability from value to value 
                        when the measurement is repeated independently). ")
                    ),
                    div(
                        style = "text-align: center",
                        bsButton(
                            inputId = "start",
                            label = "Go",
                            icon = icon("bolt"),
                            style = "danger",
                            size = "large",
                            class="circle grow"
                        )
                    )
                ),        
                tabItem(
                    tabName ="game",
                    div(
                        style="display: inline-block;vertical-align:top;",
                        circleButton(
                            inputId = "info",
                            icon = icon("info"), 
                            status = "myClass",
                            size = "xs"
                        )
                    ),
                    wellPanel(
                        fluidRow(uiOutput("question"))
                    ),
                    hr(),
                    fluidRow(
                        column(
                            width = 4,
                            plotOutput(
                                outputId = "target", 
                                click = 'Click'
                            ), 
                            style = "height: 320px;"
                        ),
                        column(
                            width = 8, 
                            conditionalPanel(
                                condition = 'input.submit != 0',
                                fluidRow(
                                    #change scroll over for bias and reliability
                                    column(
                                        width = 6,
                                        plotOutput("plota"), 
                                        style = "height: 320px;",
                                        bsPopover(
                                            id = "plota", 
                                            title = "Bias", 
                                            content = "How far the points from the center.",
                                            placement = "top"
                                        )
                                    ),
                                    column(
                                        width = 6,
                                        plotOutput("plotb"), 
                                        style = "height: 320px;",
                                        bsPopover(
                                            id = "plotb", 
                                            title = "Reliability", 
                                            content = "How far points are from each other",
                                            placement = "top"
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    br(),br(),
                    fluidRow(
                        column(
                            width = 4, 
                            offset = 4, 
                            bsButton(
                                inputId = "submit",
                                label = "Submit",
                                type = "toggle", 
                                size = "large", 
                                value = FALSE, 
                                disabled = TRUE
                            ),
                            bsButton(
                                inputId = "new",
                                label = "Next>>", 
                                style = "danger",
                                size = "large",
                                disabled = TRUE
                            ),
                            bsButton(
                              inputId = 'clear', 
                              label = "Try Again", 
                              style = 'danger', 
                              size = 'large'
                            )
                        ),
                        column(
                            width = 3, 
                            offset = 1,
                            conditionalPanel(
                                "input.submit != 0", 
                                wellPanel(
                                    div(
                                        style = "position: relative; top:0",
                                        print("Feedback")
                                    ),
                                    img(src = "arrow.gif", width = 40), 
                                    class = "arrow"
                                )
                            )
                        )
                    ),
                    fluidRow(
                        hr(),
                        conditionalPanel(
                            "input.submit != 0",
                            h3(uiOutput("answer")),
                            wellPanel(
                                uiOutput("feedback0"),
                                uiOutput("feedback1"),
                                uiOutput("feedback2"),
                                uiOutput("feedback3"), 
                                class = "wellfeedback"
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "references",
                    h2("References"),
                    p(
                      class = "hangingindent",
                      "Dean Attali (2020). shinyjs: Easily Improve the User Experience of Your 
                      Shiny Apps in Seconds. R package version 1.1. https://CRAN.R-project.org/package=shinyjs"
                    ),
                    p(
                      class = "hangingindent",
                      "Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61.
                      https://CRAN.R-project.org/package=shinyBS"
                    ),
                    p(
                      class = "hangingindent",
                      "Jeroen Ooms (2020). V8: Embedded JavaScript and WebAssembly Engine for R. R package 
                      version 3.0.2. https://CRAN.R-project.org/package=V8"
                    ),
                    p(
                      class = "hangingindent",
                      "Robert Carey and Neil Hatfield (2020). boastUtils: BOAST Utilities. R package version 0.1.4.
                      https://github.com/EducationShinyAppTeam/boastUtils"
                    ),
                    p(
                      class = "hangingindent",
                      "Simon Urbanek (2013). png: Read and write PNG images. R package version 0.1-7.
                      https://CRAN.R-project.org/package=png"
                    ),
                    p(
                      class = "hangingindent",
                      "Victor Perrier, Fanny Meyer and David Granjon (2020). shinyWidgets: Custom Inputs Widgets for Shiny. R
                      package version 0.5.2. https://CRAN.R-project.org/package=shinyWidgets"
                    ),
                    p(
                      class = "hangingindent",
                      "Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: Create Dashboards with 'Shiny'. R
                      package version 0.7.1. https://CRAN.R-project.org/package=shinydashboard"
                    ),
                    p(
                      class = "hangingindent",
                      "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web
                      Application Framework for R. R package version 1.4.0.2. https://CRAN.R-project.org/package=shiny"
                    )
                )
            )
        )
    )
)
