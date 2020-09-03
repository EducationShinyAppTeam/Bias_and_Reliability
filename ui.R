# Load Required Packages
library(shiny)
library(shinydashboard)
library(png)
library(shinyBS)
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

ui <- list(
  tags$head(
    tags$link(
      rel = "stylesheet", 
      type = "text/css",
      href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
    dashboardPage(
        skin = "red",
        dashboardHeader(
            title = "Bias & Reliability",
            titleWidth = 250,
            tags$li(class = "dropdown", actionLink("info", icon("info"))),
            tags$li(
                class = "dropdown",
                tags$a(
                    href='https://shinyapps.science.psu.edu/',
                    icon("home")
                )
            )
        ),
        dashboardSidebar(
            width = 250,
            sidebarMenu(id='tabs',
                menuItem("Overview",tabName = "overview", icon = icon("dashboard")),
                menuItem("Prerequisites", tabName= "prerequisites", icon=icon("book")),
                menuItem("Challenge",tabName = "challenge", icon = icon("cog")),
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
                    p("This app helps with understanding the concepts of 
                      bias and reliability."),
                    br(),
                    h2("Instructions"),
                    tags$ol(
                        tags$li("Read the challenge and think about how you might 
                        achieve it."),
                        tags$li("Click on the target to place points. You will 
                        need to put at least 10 points."),
                        tags$li("Examine and use the provided feedback to place 
                        points until you meet the challenge."),
                        tags$li("When you achieve the current challenge, press 
                        the Next button to try another. (Note: there are only 
                        four unique challenges.")),
                    div(
                        style = "text-align: center",
                        bsButton(
                            inputId = "go",
                            label = "Go to Prerequisites",
                            icon = icon("bolt"),
                            size = "large",
                        )
                    ),
                    br(),
                    br(),
                    h2("Acknowledgements"),
                    p("This app was developed and coded by Yuxin Zhang and 
                      updated by Luxin Wang and Thomas McIntyre. The most recent
                      version was updated by Chenese Gray."),
                    div(class = "updated", "Last Update: 6/25/2020 by CG.")
                ),
                #Adding pre-requisites page to remove background from instructions page
                tabItem(
                    tabName = "prerequisites",
                    h2("Prerequisites"),
                    br(),br(),
                    h3("Review the following concepts: "),
                    br(),
                    tags$ul(
                        tags$li("Measurement is one of the main components of 
                        Statistics. Whether we're measuring something about a 
                        single object (e.g., the mass of a cookie), a single 
                        living being (e.g., a person's height), or a collection 
                        through the use of a statistic (e.g., how many people 
                        prefer Meyer Dairy over Berkey Creamery),  measurements
                        might vary."),
                        
                        tags$li(strong('Bias'), "refers to the degree by which a
                        measurement systematically misses the true value of what
                        you're measuring. If the true value is the bull's eye of 
                        a target, how might high and low values of bias impact 
                        where you hit the target?"),
                        
                        tags$li(strong('Reliability'), "refers to how consistent 
                        the measurement process is. If you carry out the process 
                        in the exact same way on the exact same object and then 
                        compare all of your measurements,are they all the same 
                        (no variation), fairly close to each each other 
                        (low variation), or really far from each other 
                        (high variation)? Low variation indicates high reliability; 
                        high variation indicates low reliability. ")
                    ),
                    div(
                        style = "text-align: center",
                        bsButton(
                            inputId = "start",
                            label = "Go",
                            icon = icon("bolt"),
                            size = "large",
                        )
                    )
                ),        
                tabItem(
                    tabName ="challenge",
                    h2("Challenge Yourself!"),
                    br(),
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
                            width = 4, 
                            conditionalPanel(
                                condition = 'input.submit != 0',
                                plotOutput("plota"), 
                                style = "height: 320px;",
                                bsPopover(
                                    id = "plota", 
                                    title = "Bias", 
                                    content = "How far the points from the center.",
                                    placement = "top"
                                )
                            )
                        ),
                        column(
                            width = 4,
                            conditionalPanel(
                                condition = 'input.submit != 0',
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
                                label = "Next >>", 
                                size = "large",
                                disabled = TRUE
                            ),
                            bsButton(
                              inputId = 'clear', 
                              label = "Try Again", 
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
                            div(
                              style = "text-align: center",
                              uiOutput("answer")),
                            wellPanel(
                                uiOutput("feedback0"),
                                uiOutput("feedback1"),br(),
                                uiOutput("feedback2"),br(),
                                uiOutput("feedback3")
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "references",
                    h2("References"),
                    p(
                      class = "hangingindent",
                      "Attali, D. (2020). shinyjs: Easily Improve the User 
                      Experience of Your Shiny Apps in Seconds. R package 
                      version 1.1. Available from https://CRAN.R-project.org/package=shinyjs"
                    ),
                    p(
                      class = "hangingindent",
                      "Bailey, E. (2015). shinyBS: Twitter Bootstrap Components
                      for Shiny. R package version 0.61. Available from
                      https://CRAN.R-project.org/package=shinyBS"
                    ),
                    p(
                      class = "hangingindent",
                      "Carey, R. and Hatfield, N. (2020). boastUtils: 
                      BOAST Utilities. R package version 0.1.4. Available from
                      https://github.com/EducationShinyAppTeam/boastUtils"
                    ),
                    p(
                      class = "hangingindent",
                      "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard: 
                      Create Dashboards with 'Shiny'. R package version 0.7.1. 
                      Available from https://CRAN.R-project.org/package=shinydashboard"
                    ),
                    p(
                      class = "hangingindent",
                      "Chang, W., Cheng, J., Allaire, J., Xie, Y., and 
                      McPherson, J. (2020). shiny: Web Application Framework for 
                      R. R package version 1.4.0.2. Available from 
                      https://CRAN.R-project.org/package=shiny"
                    ),
                    p(
                      class = "hangingindent",
                      "Ooms, J. (2020). V8: Embedded JavaScript and WebAssembly 
                      Engine for R. R package version 3.0.2. Available from
                      https://CRAN.R-project.org/package=V8"
                    ),
                    p(
                      class = "hangingindent",
                      "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets: 
                      Custom Inputs Widgets for Shiny. R package version 0.5.2. 
                      Available from https://CRAN.R-project.org/package=shinyWidgets"
                    ),
                    p(
                      class = "hangingindent",
                      "Urbanek, S. (2013). png: Read and write PNG images. 
                      R package version 0.1-7. Available from https://CRAN.R-project.org/package=png"
                    )
                )
            )
        )
    )
)








