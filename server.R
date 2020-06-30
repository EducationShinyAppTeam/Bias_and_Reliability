library(shiny)
library(png)
library(shinyBS)
library(shinyjs)
library(V8)
library(shinydashboard)
library(shinyWidgets)

##BIAS app

#Define the function to disable all the button

shinyServer(function(input, output,session) {
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click in the target to create points that meet the challenge.",
      type = "info"
    )
  })
  
  ###UPDATE: adding the go button 
  observeEvent(input$go,{
    updateTabItems(session,"tabs","prerequisites")
  })
  
  observeEvent(input$start,{
    updateTabItems(session,"tabs","challenge")
  })
  
  #Create reactiveValues to restore the coordinates of clicked points and the calculated bias and reliability
  var <- reactiveValues(x = NULL, y = NULL, bias = NULL, reliability = NULL)
  
  #Randomly generate one question out of the four questions every time when "Next" button is clicked.
  index <- reactiveValues(index = 4)
  index_list<-reactiveValues(listc=sample(1:3,3,replace=FALSE))
  
  ta<-reactiveValues(ta=4)
  
  observeEvent(input$new,{
    index$index <- index_list$listc[1]
    index_list$listc=index_list$listc[-1]
    
    ta$ta<-index$index
  })
  
  
  observeEvent(input$clear,{
    index$index <- ta$ta
  })

  output$question <- renderUI({
    validate(
      need(index$index != "", "You have exhausted our question bank! Please refresh the page!")
    )
    if (index$index == 1){
      p("Create a model for large bias and low reliability?  Please put on at least 10 dots.")
    }else if (index$index == 2){
      p("Create a model for large bias and high reliability?  Please put on at least 10 dots.")
    }else if (index$index == 3){
      p("Create a model for no bias and low reliability?  Please put on at least 10 dots.")
    }else if (index$index == 4){
      p("Create a model for no bias and high reliability? (Please put down at least 10 dots.)")
    }
  })
  
  #Create function for ploting the target  
  plotTarget <- function(x,y){
    #Get image
    isolate(ima <- readPNG("t6.png"))
    
    #Set up the plot area
    isolate(plot(x=-5:5,ylim=c(-5,5),xlim=c(-5,5),type='p',xlab = '',ylab = ''))
    
    #Get the plot information so the image will fill the plot box, and draw it
    isolate(lim <- par())
    isolate(rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]))
    isolate(grid())
    lines(x,y, xlim = c(-5,5), ylim = c(-5,5),type = 'p', pch = 19)
  }
  #Create function for ploting the bias plot  
  plotA <- function(x,y){
    plot(x = -3:3,xlim = c(-6,6), type = "n", xlab = "", ylab = "", main = "Bias",yaxt = 'n', cex.main = 1.25, cex.axis = 1.25, cex.lab = 1.25)
    box(col = "red")
    abline(v=0,col = "red")
    abline(v = sqrt(mean(x)^2 + mean(y)^2))
  }
  #Create function for ploting the reliability plot  
  plotB <- function(x,y){
    #Density plot of the distance between each dots
    plot(density(dist(cbind(x,y))),xlim = c(-2,12),xlab = "", main = "Reliability", cex.main = 1.25, cex.axis = 1.25, cex.lab = 1.25)
    abline(v = mean(dist(cbind(x,y))))
    box(col = "red")
  }
  
  #Save the clicked points or clear all.    
  observe({
    # Initially will be empty
    if (is.null(input$Click)){
      return()
    }
    #Save the coordinates of clicked points as two vectors
    isolate({
      var$x <- c(var$x, input$Click$x)
      var$y <- c(var$y, input$Click$y)
    })
  })
  
  #Only when the user clicks more than 10 points, can the "submit" button be enabled.
  observe({
    if (length(var$x) >= 10){
      updateButton(session,"submit",label = "Submit",style = "danger", size = "large", disabled = FALSE)
    }
  })
  
  #Reset the button to FALSE so that all the conditionalPanel will disappear
  observeEvent(input$submit,{{
    updateButton(session,"submit",label = "Submit",style = "danger",size = "large", disabled = T)
  }})
  #Reset(clear) the clicked points
  observe({
    if (input$submit == FALSE){
      var$x <- NULL
      var$y <- NULL
    }
  })
  observeEvent(input$clear,{
    if (input$submit == FALSE){
      var$x <- NULL
      var$y <- NULL
    }
  })
 
  ##Set the related relationship between three buttons: "submit" "clear" "next"  
  observe({
    if (length(var$x) == 1){
      updateButton(session,"submit", label = "Submit", disabled = TRUE)
    }
  })
  observeEvent(input$submit,{
    updateButton(session,"clear", disabled = FALSE)
  })
  observeEvent(input$new,{
    updateButton(session,"clear", disabled = FALSE)
  })
  
  observeEvent(input$new,{
    updateButton(session, "submit", label = "Submit",value = FALSE, disabled = TRUE)
  })
  observeEvent(input$clear,{
    updateButton(session, "submit", label = "Submit",value = FALSE, disabled = TRUE)
  })
  observeEvent(input$clear,{
    updateButton(session, "new",disabled = TRUE)
  })
 
  #The "next" button will be enabled once the user hits submit. 
  observe(priority = 1,{
    if (input$submit== FALSE){
      updateButton(session,"new", label="Next>>", disabled = TRUE)
    }
    else{updateButton(session,"new", label="Next>>", disabled = FALSE)}
  })
  
  observe(priority = 2,{
    if (length(index_list$listc)==1){
      updateButton(session,"new", label="Next>>", disabled = TRUE)
    }
  })
  ##Plot three outputs using the functions defined before  
  output$target <- renderPlot({
    plotTarget(var$x,var$y)
  },height = 320, width = 320)
  
  output$plota <- renderPlot({
    plotA(var$x,var$y)
  },height = 320, width = 320)
  
  output$plotb <- renderPlot({
    plotB(var$x,var$y)
  },height = 320, width = 320)
  observe({
    if (input$submit == TRUE){
      #Distance between the center of target(population) and the center of the cloud of dots(ave. of samples)
      var$bias <- sqrt(mean(var$x)^2 + mean(var$y)^2)
      #Average distance between each dots
      var$reliability <- mean(dist(cbind(var$x,var$y)))
    }
  })
  
  ##Display the numeric results and limit the digits.
  output$bias <- renderText({
    print(round(var$bias, digits = 2))
  })
  
  output$reliability <- renderText({
    print(round(var$reliability, digits = 2))
  })
  
  ##Assess the answers given by the user.
  output$answer <- renderUI({
    #Four questions.
    #For each question, there are three leveled responses.
    if (index$index == 1){
      if ((var$bias > 4) & (var$reliability > 3)){
        return("Great! Nicely done!")
      }else if ((var$bias > 3) & (var$reliability > 2.5)){
        return("Good job!")
      }else{
        return("You can do better. Try again. What could you do to get a closer answer?")
      }
    }else if (index$index == 2){
      if ((var$bias > 4) & (var$reliability < 1.5)){
        return("Great! Nicely done!")
      }else if ((var$bias > 3) & (var$reliability < 2)){
        return("Good job!")
      }else{
        return("You can do better. Try again. What could you do to get a closer answer?")
      }
    }else if (index$index == 3){
      if ((var$bias < 0.25) & (var$reliability > 3)){
        return("Great! Nicely done!")
      }else if ((var$bias < 0.3) & (var$reliability > 2.5)){
        return("Good job!")
      }else{
        return("You can do better. Try again. What could you do to get a closer answer?")
      }
    }else if (index$index == 4){
      if ((var$bias < 0.25) & (var$reliability < 1.5)){
        return("Great! Nicely done!")
      }else if ((var$bias < 0.3) & (var$reliability < 2)){
        return("Good job!")
      }else{
        return("You can do better. Try again. What could you do to get a closer answer?")
      }
    }
  })
 
  ##Print feedbacks.  
  
  output$feedback1 <- renderUI({
    paste("Average bias = ",round(var$bias,digits = 2),"(smaller values indicate less bias)")
  }) 
  output$feedback2 <- renderUI({
    paste("Average reliability = ", round(var$reliability,digits = 2), "(smaller values indicate better reliability)")
  })
  output$feedback3 <- renderUI({
    #which quadrants
    if ((mean(var$x) > 0 ) & (mean(var$y) > 0)){
      quad = "upper right"
    }else if ((mean(var$x) < 0) & (mean(var$y) > 0 )){
      quad = "upper left"
    }else if ((mean(var$x) < 0) & (mean(var$y) < 0)){
      quad = "bottom left"
    }else if ((mean(var$x) > 0) & (mean(var$y) < 0)){
      quad = "bottom right"
    }
    
    #reliability
    if (var$reliability > 3){
      reliaFeedback = ", with a low reliability."
    }else if (var$reliability > 2.5){
      reliaFeedback = ", with a relatively low reliability."
    }else if (var$reliability < 1.5){
      reliaFeedback = ", with a relatively high reliability."
    }else if (var$reliability < 2){
      reliaFeedback = ", with a high reliability."
    }else{
      reliaFeedback = "."
    }
    
    #paste together the feedbacks and print them out.
    paste("The dots you put down are centered at the",quad,"quadrant,",signif(var$bias, 2),
          "away from the center",reliaFeedback)
  })
})



