library(shiny)
library(shinydashboard)
library(png)
library(shinyBS)
library(V8)
library(shinyjs)
library(shinyWidgets)


##BIAS app

#Use jscode to for reset button to reload the app
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Bias & Reliability",
                                    titleWidth = 220),
                    #adding prereq pages
                    dashboardSidebar(
                      width = 220,
                      
                      sidebarMenu(id='tabs',
                                  menuItem("Prerequisites", tabName= "prereq", icon=icon("book")),
                                  menuItem("Overview",tabName = "instruction", icon = icon("dashboard")),
                                  menuItem("Game",tabName = "game", icon = icon("gamepad"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "navcolor.css") #customised style sheet
                      ),
                      
                      tabItems(
                        tabItem(tabName = "instruction",
                                tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                                br(),br(),br(),
                                h3(strong("About:")),
                                h4("This app helps with understanding the concepts of bias and reliability."),br(),
                                h3(strong("Instructions:")),
                                h4(tags$li("Step 1: Click the target to put down at least 10 points.")),
                                h4(tags$li("Step 2: Check the feedback and keep trying until you get it correct and go to the next question.")),
                                h4(tags$li('Note: There are only four different questions.')),
                                div(style = "text-align: center",bsButton("go","Go",icon("bolt"),style = "danger",size = "large",class="circle grow")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Yuxin Zhang and updated by Luxin Wang and Thomas McIntyre.")
                                
                                
                        ),
                        #Adding pre-requisites page to remove background from instructions page
                        tabItem(tabName="prereq",
                                
                                h3(strong("Background: Bias and Reliability")),br(),
                                h4(tags$li("The bias of a measurement describes to what degree it is systematically off target from the true value.
       While playing with the app you should think of what gives you a high or low bias.")),
                                h4(tags$li("The reliability of a measurement describes how consistent the measurement is when you repeat it 
       (alternatively, an unreliable measurement is one that shows a lot of variability from value to value 
       when the measurement is repeated independently). ")),
                                
                                div(style = "text-align: center",bsButton("start","Go to the overview",icon("bolt"),style = "danger",size = "large",class="circle grow"))
                                
                        ),
                        
                        tabItem(tabName ="game",
                                div(style="display: inline-block;vertical-align:top;",
                                    tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                ),
                                div(style="display: inline-block;vertical-align:top;",
                                    circleButton("info",icon = icon("info"), status = "myClass",size = "xs")
                                ),
                                
                                wellPanel(
                                  fluidRow(uiOutput("question"))
                                ),hr(),
                                fluidRow(
                                  column(4,plotOutput("target", click = 'Click'), style = "height: 320px;"),
                                  column(8, 
                                         conditionalPanel(
                                           condition = 'input.submit != 0',
                                           fluidRow(
                                             #change scroll over for bias and reliability
                                             column(6,plotOutput("plota"), style = "height: 320px;",
                                                    bsPopover("plota", "Bias", "How far the points from the center.",placement = "top")),
                                             column(6,plotOutput("plotb"), style = "height: 320px;",
                                                    bsPopover("plotb", "Reliability", "How far points are from each other",placement = "top")))
                                         ))),
                                fluidRow(
                                  column(4, offset = 4, 
                                         bsButton("submit",label = "Submit",type = "toggle", size = "large", value = FALSE, disabled = TRUE),
                                         bsButton("new",label = "Next>>", style = "danger", size = "large",disabled=TRUE),
                                         bsButton('clear', label = "Try Again", style = 'danger', size = 'large')
                                  ),
                                  column(3, offset = 1,
                                         conditionalPanel("input.submit != 0", 
                                                          wellPanel(
                                                            div(style = "position: relative; top:0",print("Feedback")),
                                                            img(src = "arrow.gif", width = 40), class = "arrow")))
                                ),
                                fluidRow(
                                  hr(),
                                  conditionalPanel("input.submit != 0",
                                                   h1(uiOutput("answer")),
                                                   wellPanel(uiOutput("feedback1"),
                                                             uiOutput("feedback2"),
                                                             uiOutput("feedback3"), class = "wellfeedback")
                                  ))
                                
                        )
                      )
                    )
                    
)









