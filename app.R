# Load Required Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(ggforce)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Bias and Reliability"
APP_DESCP  <<- paste(
  "The bias of a measurement describes to what degree it is systematically off
  target from the true value. The reliability of a measurement describes how
  consistent the measurement is when you repeat it. This app is designed to help
  with understanding those concepts."
)
# End App Meta Data------------------------------------------------------------

# Load Question Bank ----
questionBank <- read.csv(file = "questionBank.csv",
                         header = TRUE,
                         stringsAsFactors = FALSE)

# Define global functions and constants ----
comparison <- function(value, type, target, tol = 0.1) {
  if(length(value) == 0 || is.null(value) ||
     length(type) == 0 || is.null(type) ||
     length(target) == 0 || is.null(target)) {
    return(NULL)
  } else {
    if(type == "greater") {
      return(ifelse(value > target - tol, TRUE, FALSE))
    } else {
      return(ifelse(value < target + tol, TRUE, FALSE))
    }
  }
}


circles <- data.frame(
  x0 = rep(0, 4),
  y0 = rep(0, 4),
  r = seq.int(from = 1, to = 4, by = 1)
)

mainPlot <- ggplot(data = circles,
                   mapping = aes(x0 = x0, y0 = y0, r = r)) +
  ggforce::geom_circle(
    color = boastUtils::psuPalette[2],
    size = 3
  ) +
  geom_hline(yintercept = 0, color = boastUtils::psuPalette[2]) +
  geom_vline(xintercept = 0, color = boastUtils::psuPalette[2]) +
  coord_fixed() +
  theme_void()

defaultFeedback <- ""

defaultBiasPlot <- ggplot(
  data = data.frame(values = rep(0, 10)),
  mapping = aes(x = values)) +
  geom_vline(xintercept = 0,
             color = boastUtils::psuPalette[2],
             size = 2,
             lty = 2) +
  scale_x_continuous(expand = expansion(mult = 0, add = 1)) +
  theme_bw() +
  theme(
    text = element_text(size = 16)
  ) +
  xlab("Bias metric")

defaultReliabilityPlot <- ggplot(
  data = data.frame(values = rep(0, 10)),
  mapping = aes(x = values)) +
  geom_density(size = 2,
               color = boastUtils::boastPalette[8],
               fill = boastUtils::boastPalette[8]) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05), add = 0),
                     limits = c(0, NA)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1), add = 0)) +
  theme_bw() +
  theme(
    text = element_text(size = 16)
  ) +
  xlab("Distance between points") +
  ylab("Density")

# Define UI ----
ui <- list(
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  dashboardPage(
    skin = "red",
    ## Header ----
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
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(id='tabs',
                  menuItem("Overview",tabName = "overview", icon = icon("tachometer-alt")),
                  menuItem("Prerequisites", tabName= "prerequisites", icon=icon("book")),
                  menuItem("Challenge",tabName = "challenge", icon = icon("cog")),
                  menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        tabItem(
          ### Overivew ----
          tabName = "overview",
          h1("Bias & Reliability"),
          p("This app helps with understanding the concepts of bias and
            reliability."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Read the challenge and think about how you might achieve it."),
            tags$li("Click on the target to place points. You will need to put
                    at least 10 points."),
            tags$li("Examine and use the provided feedback to place points until
                    you meet the challenge."),
            tags$li("When you achieve the current challenge, press
                    the Next button to try another. (Note: there are only four
                    unique challenges.)")
          ),
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
          p("This app was developed and coded by Yuxin Zhang and updated by
            Luxin Wang and Thomas McIntyre. The most recent version was updated
            by Chenese Gray with additional coding changes by Neil J. Hatfield.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/6/2020 by NJH.")
          )
        ),
        tabItem(
          ### Prereq's ----
          tabName = "prerequisites",
          h2("Prerequisites"),
          p("Review the following concepts."),
          tags$ul(
            tags$li("Measurement is one of the main components of Statistics.
                    Whether we're measuring something about a single object
                    (e.g., the mass of a cookie), a single living being (e.g.,
                    a person's height), or a collection through the use of a
                    statistic (e.g., how many people prefer Meyer Dairy over
                    Berkey Creamery), measurements", tags$em(" can and will "),
                    "vary."),
            tags$li(strong('Bias'), "refers to the degree by which a measurement
                    systematically misses the true value of what you're measuring.
                    If the true value is the bull's eye of a target, how might
                    high and low values of bias impact where you hit the target?"),
            tags$li(strong('Reliability'), "refers to how consistent the
                    measurement process is. If you carry out the process in the
                    exact same way on the exact same object and then compare all
                    of your measurements, are they all the same (no variation),
                    fairly close to each each other (low variation), or really
                    far from each other (high variation)? Low variation indicates
                    high reliability; high variation indicates low reliability.")
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
          ### Challenge ----
          tabName ="challenge",
          h2("Challenge Yourself!"),
          p("You will need to place at least ten (10) points on the plot below by
            clicking the plot. Place your points keeping the listed challenge in
            mind. Keep in mind that you will need to think about the horizontal
            and vertical positioning of your points."),
          fluidRow(
            column(
              width = 5,
              wellPanel(
                h3("Current Challenge"),
                div(
                  class = "largerFont",
                  uiOutput("challenge")
                ),
                br(),
                uiOutput("pointCounter"),
                bsButton(
                  inputId = "submit",
                  label = "Submit",
                  style = "default",
                  size = "large",
                  disabled = TRUE
                ),
                bsButton(
                  inputId = "reattempt",
                  label = "Reattempt",
                  style = "default",
                  size = "large",
                  icon = icon("retweet")
                ),
                br(),
                bsButton(
                  inputId = "nextChallenge",
                  label = "Next Challenge",
                  style = "default",
                  size = "large",
                  disabled = TRUE
                )
              ),
              uiOutput("gradingIcon"),
              uiOutput("gradeMessage"),
            ),
            column(
              width = 7,
              div(
                style = "text-align: center;",
                plotOutput(
                  outputId = "inputPlot",
                  click = "userClick"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('inputPlot').setAttribute('aria-label',
                  `A target with four concentric circles and crosshairs; click
                  on the target to place points`)})"
                ))
              ),
            )
          ),
          h3("Feedback"),
          uiOutput("description"),
          h4("Bias"),
          fluidRow(
            column(
              width = 5,
              plotOutput(
                outputId = "biasPlot",
                height = "200px"
              ),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('biasPlot').setAttribute('aria-label',
                  `A red dashed vertical line shows no bias, a solid black line
                indicates your value of bias`)})"
              ))
            ),
            column(
              width = 7,
              uiOutput("biasFeedback"),
              br(),
              p(tags$em("Note: "),
                "The closer the metric (your value is the solid black line) is to
                zero (denoted by the red dashed line), the less bias your process
                has. Negative values indicate that your process is systematically ",
                tags$strong("under-estimating"), " the true value. Positive
                values indicate that your process is systematically ",
                tags$strong("over-estimating"), " the true value."
              )
            )
          ),
          h4("Reliability"),
          fluidRow(
            column(
              width = 7,
              uiOutput("reliabilityFeedback"),
              br(),
              p(tags$em("Note: "),
                "The more reliable your process is, the tighter the blue shaded
                region will be around the mean distance between estimates (as
                shown by the red dashed line). The less reliable your process is
                the more variation in the distance between estimates, causing the
                blue shaded area to expand further and further from the mean distance."
              )
            ),
            column(
              width = 5,
              plotOutput(
                outputId = "reliabilityPlot",
                height = "200px")
            ),
            tags$script(HTML(
              "$(document).ready(function() {
                  document.getElementById('reliabilityPlot').setAttribute('aria-label',
                  `A density plot that shows how reliable your process is`)})"
            ))
          )
        ),
        tabItem(
          ### References ----
          tabName = "references",
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter Bootstrap Components
                      for Shiny. R package version 0.61. Available from
                      https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils:
                      BOAST Utilities. R package version 0.1.6.3. Available from
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
                      R. R package version 1.5.0. Available from
                      https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Pedersen, T. L. (2020). ggforce: Acclerating 'ggplot2'.
            R package version 0.3.2.
            Available from https://CRAN.R-project.org/package=ggforce"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020). shinyWidgets:
                      Custom Inputs Widgets for Shiny. R package version 0.5.3.
                      Available from https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Available from https://ggplot2.tidyverse.org"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define Server ----
server <- function(input, output, session) {
  ## Define session level variables ----
  biasMetric <-reactiveVal(NA)
  reliabilityMetric <- reactiveVal(NA)

  contexts <- reactiveVal(0)
  counter <- reactiveVal(1)
  activeGame <- reactiveVal(FALSE)

  userPoints <- reactiveValues()
  userPoints$DT <- data.frame(
    x = numeric(),
    y = numeric()
  )

  ## Info button ----
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "Click in the target to create points that meet the challenge.",
      type = "info"
    )
  })

  ## Go button ----
  observeEvent(input$go,{
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "prerequisites")
  })

  ## Start button ----
  observeEvent(input$start,{
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "challenge")
  })

  ## Setup first challenge ----
  observeEvent(input$tabs, {
    if(input$tabs == "challenge" && !(activeGame())) {
      contexts(sample(1:nrow(questionBank), size = nrow(questionBank)))
      activeGame(TRUE)
    }
    output$challenge <- renderUI({
      paste("Create a process which has",
            questionBank[contexts()[counter()], "bias"], "bias and",
            questionBank[contexts()[counter()], "reliability"], "reliability.")
    })

    output$biasPlot <- renderPlot({
      return(defaultBiasPlot)
    })
    output$biasFeedback <- renderUI({
      paste(defaultFeedback)
    })
    output$reliabilityPlot <- renderPlot({
      return(defaultReliabilityPlot)
    })
    output$reliabilityFeedback <- renderUI({
      return(defaultFeedback)
    })

  })

  ## Watch for user clicks to plot points ----
  observeEvent(input$userClick, {
    newRow <- data.frame(
      x = input$userClick$x,
      y = input$userClick$y
    )

    userPoints$DT <- rbind(userPoints$DT, newRow)
  })

  ## Display counter for number of points ploted ----
  output$pointCounter <- renderUI({
    if(nrow(userPoints$DT) >= 10) {
      p("You have at least 10 points plotted.")
    } else {
      p("Please plot at least ", 10 - nrow(userPoints$DT), " more points.")
    }
  })

  ## Create the main target plot ----
  output$inputPlot <- renderPlot({
    if(nrow(userPoints$DT) == 0) {
      return(mainPlot)
    } else {
      mainPlot + geom_point(
        inherit.aes = FALSE,
        data = userPoints$DT,
        mapping = aes(x = x, y = y),
        color = "black",
        size = 5
      )
    }
  })

  observeEvent(userPoints$DT, {
    if(nrow(userPoints$DT) >= 10) {
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
    }
  })

  ## Submit Button ----
  observeEvent(input$submit, {
    ### Enable Next Challenge button ----
    updateButton(
      session = session,
      inputId = "nextChallenge",
      disabled = FALSE
    )

    ### Render Output Plots and Feedback ----
    ### Bias Plot ----
    if(nrow(userPoints$DT) >= 10) {
      biasMetric(sqrt(mean(userPoints$DT$x)^2 + mean(userPoints$DT$y)^2))
      biasData <- data.frame(
        values = seq(from = (-1 * biasMetric()), to = biasMetric(), by = 0.1)
      )
    }
    output$biasPlot <- renderPlot({
      validate(
        need(
          expr = nrow(userPoints$DT) >= 10,
          message = "You need at least 10 points before pressing Submit."
        )
      )
      ggplot(data = biasData, mapping = aes(x = values)) +
        geom_vline(xintercept = 0,
                   color = boastUtils::psuPalette[2],
                   size = 2,
                   lty = 2) +
        geom_vline(xintercept = biasMetric(),
                   color = "black",
                   size = 2,
                   lty = 1) +
        scale_x_continuous(expand = expansion(mult = 0, add = 1)) +
        theme_bw() +
        theme(
          text = element_text(size = 16)
        ) +
        xlab("Bias metric")
    })
    ### Bias Feedback ----
    output$biasFeedback <- renderUI({
      paste0("Your estimated bias is ", round(biasMetric(), 3), ".")
    })

    ### Reliability Plot ----
    if(nrow(userPoints$DT) >= 10) {
      reliabData <- data.frame(
        values = as.vector(dist(cbind(userPoints$DT)))
      )
      reliabilityMetric(mean(reliabData$values))
    }
    output$reliabilityPlot <- renderPlot({
      validate(
        need(
          expr = nrow(userPoints$DT) >= 10,
          message = "You need at least 10 points before pressing Submit."
        )
      )
      ggplot(data = reliabData,
             mapping = aes(x = values)) +
        geom_density(size = 2,
                     color = boastUtils::boastPalette[8],
                     fill = boastUtils::boastPalette[8]) +
        geom_vline(xintercept = mean(reliabData$values),
                   color = boastUtils::psuPalette[2],
                   size = 2,
                   lty = 2) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.05), add = 0),
                           limits = c(0, NA)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1), add = 0)) +
        theme_bw() +
        theme(
          text = element_text(size = 16)
        ) +
        xlab("Distance between points") +
        ylab("Density")
    })
    ### Reliability Feedback ----
    output$reliabilityFeedback <- renderUI({
      paste0("Your estimated reliability is ", round(reliabilityMetric(), 3),
             ". Note: smaller indicates higher reliability.")
    })

    ### Description of Student's Plot ----
    if(nrow(userPoints$DT) < 10) {
      output$description <- renderUI({
        "Please place at least ten points on the target."
      })
    } else {
      if(mean(userPoints$DT$x, na.rm = TRUE) > 0 &&
         mean(userPoints$DT$y, na.rm = TRUE) > 0) {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the upper right
        quadrant of the target."
        })
      } else if(mean(userPoints$DT$x, na.rm = TRUE) < 0 &&
                mean(userPoints$DT$y, na.rm = TRUE) > 0) {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the upper left
        quadrant of the target."
        })
      } else if(mean(userPoints$DT$x, na.rm = TRUE) < 0 &&
                mean(userPoints$DT$y, na.rm = TRUE) < 0) {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the lower left
        quadrant of the target."
        })
      } else if(mean(userPoints$DT$x, na.rm = TRUE) > 0 &&
                mean(userPoints$DT$y, na.rm = TRUE) < 0) {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the lower right
        quadrant of the target."
        })
      } else if(mean(userPoints$DT$x, na.rm = TRUE) == 0 &&
                mean(userPoints$DT$y, na.rm = TRUE) < 0) {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the lower
        half of the target."
        })
      } else if(mean(userPoints$DT$x, na.rm = TRUE) == 0 &&
                mean(userPoints$DT$y, na.rm = TRUE) > 0) {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the upper
        half of the target."
        })
      } else if(mean(userPoints$DT$x, na.rm = TRUE) < 0 &&
                mean(userPoints$DT$y, na.rm = TRUE) == 0) {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the left
        quadrant of the target."
        })
      } else if(mean(userPoints$DT$x, na.rm = TRUE) > 0 &&
                mean(userPoints$DT$y, na.rm = TRUE) == 0) {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the right
        quadrant of the target."
        })
      } else {
        output$description <- renderUI({
          "Your plot indicates that your process graviated towards the center
         of the target."
        })
      }
    }

    ## Grading ----
    biasCheck <- comparison(
      value = biasMetric(),
      type = questionBank[contexts()[counter()], "biasComp"],
      target = questionBank[contexts()[counter()], "biasLimit"]
    )
    reliabilityCheck <- comparison(
      value = reliabilityMetric(),
      type = questionBank[contexts()[counter()], "reliabilityComp"],
      target = questionBank[contexts()[counter()], "reliabilityLimit"]
    )
    if(biasCheck && reliabilityCheck){
      output$gradingIcon <- boastUtils::renderIcon("correct")
      output$gradeMessage <- renderUI({
        paste("Congrats! You succeeded in the current challenge. Check out the
              feedback below to see just how well you did. Then try the next
              challenge.")
      })
    } else if (biasCheck && !reliabilityCheck) {
      output$gradingIcon <- boastUtils::renderIcon("partial")
      output$gradeMessage <- renderUI({
        paste("You are correct with your Bias, but not Reliability.
              Please look at the feedback below to see how you can improve and
              then reattempt this challenge.")
      })
    } else if (!biasCheck && reliabilityCheck) {
      output$gradingIcon <- boastUtils::renderIcon("partial")
      output$gradeMessage <- renderUI({
        paste("You are correct with your Reliability, but not Bias.
              Please look at the feedback below to see how you can improve and
              then reattempt this challenge.")
      })
    } else {
      output$gradingIcon <- boastUtils::renderIcon("incorrect")
      output$gradeMessage <- renderUI({
        paste("You're not quite correct in both dimensions. Please look at the
              feedback below to see how to improve and then reattempt this
              challenge.")
      })
    }
  })

  ## Grading Icons ----
  observeEvent(input$submit, {


  })

  ## Reattempt Button ----
  observeEvent(input$reattempt, {
    userPoints$DT <- data.frame(
      x = NULL,
      y = NULL
    )
    biasMetric(NA)
    reliabilityMetric(NA)
    updateButton(
      session =  session,
      inputId = "submit",
      disabled = TRUE
    )
    output$biasPlot <- renderPlot({
      return(defaultBiasPlot)
    })
    output$biasFeedback <- renderUI({
      return(defaultFeedback)
    })
    output$reliabilityPlot <- renderPlot({
      return(defaultReliabilityPlot)
    })
    output$reliabilityFeedback <- renderUI({
      return(defaultFeedback)
    })
    output$description <- renderUI({
      return(defaultFeedback)
    })
    output$gradingIcon <- boastUtils::renderIcon()
    output$gradeMessage <- renderUI({
      return(defaultFeedback)
    })

    updateButton(
      session = session,
      inputId = "nextChallenge",
      disabled = TRUE
    )
  })

  ## Next button ----
  observeEvent(input$nextChallenge, {
    updateButton(
      session = session,
      inputId = "submit",
      disabled = FALSE
    )
    if(counter() < nrow(questionBank)) {
      counter(counter() + 1)
    } else {
      sendSweetAlert(
        session = session,
        title = "Out of Challenges",
        text = "You have completed all of the challenges. Please refresh the app
        to start again.",
        type = "error"
      )
    }
    userPoints$DT <- data.frame(
      x = NULL,
      y = NULL
    )
    biasMetric(NA)
    reliabilityMetric(NA)
    updateButton(
      session =  session,
      inputId = "submit",
      disabled = TRUE
    )
    output$biasPlot <- renderPlot({
      return(defaultBiasPlot)
    })
    output$biasFeedback <- renderUI({
      return(defaultFeedback)
    })
    output$reliabilityPlot <- renderPlot({
      return(defaultReliabilityPlot)
    })
    output$reliabilityFeedback <- renderUI({
      return(defaultFeedback)
    })
    output$description <- renderUI({
      return(defaultFeedback)
    })
    output$gradingIcon <- boastUtils::renderIcon()
    output$gradeMessage <- renderUI({
      return(defaultFeedback)
    })
  })
}

# App Call----
boastUtils::boastApp(ui = ui, server = server)