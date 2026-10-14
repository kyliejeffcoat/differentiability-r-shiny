library(shiny)

ui <- fluidPage(
  
  titlePanel("Understanding Differentiability"),
  
  tabsetPanel(
    
    tabPanel(
      "App",
      
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput(
            "function_choice",
            "Choose a function:",
            choices = c(
              "x^2",
              "|x|",
              "Vertical Tangent",
              "Piecewise Function"
            )
          ),
          
          sliderInput(
            "x_range",
            "Choose x-range:",
            min = -10,
            max = 10,
            value = c(-5, 5)
          ),
          
          sliderInput(
            "a",
            "Choose point a:",
            min = -5,
            max = 5,
            value = 0
          ),
          
          sliderInput(
            "h",
            "Choose h:",
            min = 0.01,
            max = 1,
            value = 0.1
          ),
          
          h4("Help / Guide"),
          
          p("Move a to the point you want to test. Make h smaller and compare the left and right derivative estimates. If they approach the same finite number, the function is likely differentiable."),
          
          p("The h slider starts above zero because h = 0 would cause division by zero in the difference quotient.")
        ),
        
        mainPanel(
          
          plotOutput("graph"),
          
          h3("Numerical Diagnostics"),
          
          verbatimTextOutput("diagnostics")
        )
      )
    ),
    
    tabPanel(
      "Theory",
      
      h3("What does differentiable mean?"),
      
      p("A function is differentiable at a point when it has one clear, finite slope at that point. Another way to think about it is that the slope coming from the left and the slope coming from the right have to approach the same number. The derivative is found using the limit of the difference quotient as h gets closer to zero. If a function is differentiable at a point, it also has to be continuous there. However, being continuous does not always mean it is differentiable. For example, x squared is smooth and differentiable everywhere. Absolute value is continuous at zero, but it is not differentiable there because it has a corner. The slope from the left is negative one while the slope from the right is positive one, so there is not one derivative at zero.")
    )
  )
)

server <- function(input, output) {
  
  # Choose the function
  f <- reactive({
    
    if (input$function_choice == "x^2") {
      return(function(x) x^2)
    }
    
    if (input$function_choice == "|x|") {
      return(function(x) abs(x))
    }
    
    if (input$function_choice == "Vertical Tangent") {
      return(function(x) sign(x) * abs(x)^(1/3))
    }
    
    if (input$function_choice == "Piecewise Function") {
      return(function(x) ifelse(x < 0, x, x + 2))
    }
    
  })
  
  
  # Make the graph
  output$graph <- renderPlot({
    
    fun <- f()
    a <- input$a
    h <- input$h
    
    x <- seq(
      input$x_range[1],
      input$x_range[2],
      length.out = 500
    )
    
    y <- fun(x)
    
    plot(
      x,
      y,
      type = "l",
      lwd = 2,
      xlab = "x",
      ylab = "f(x)",
      main = input$function_choice
    )
    
    fa <- fun(a)
    fah <- fun(a + h)
    
    # Point at a
    points(
      a,
      fa,
      pch = 19,
      cex = 1.4
    )
    
    # Point at a + h
    points(
      a + h,
      fah,
      pch = 1,
      cex = 1.4
    )
    
    # Difference quotient / secant slope
    difference_quotient <- (fah - fa) / h
    
    # Secant line
    abline(
      a = fa - difference_quotient * a,
      b = difference_quotient,
      lty = 2
    )
    
    # Left and right derivative estimates
    left_slope <- (fa - fun(a - h)) / h
    right_slope <- (fun(a + h) - fa) / h
    
    # Tangent slope estimate
    tangent_slope <- (left_slope + right_slope) / 2
    
    # Compare left and right estimates
    difference <- abs(left_slope - right_slope)
    
    scale <- max(
      1,
      abs(left_slope),
      abs(right_slope)
    )
    
    differentiable <- difference < (0.25 * scale)
    
    
    # Special cases at zero
    if (
      input$function_choice == "|x|" &&
      a == 0
    ) {
      differentiable <- FALSE
    }
    
    if (
      input$function_choice == "Vertical Tangent" &&
      a == 0
    ) {
      differentiable <- FALSE
    }
    
    if (
      input$function_choice == "Piecewise Function" &&
      a == 0
    ) {
      differentiable <- FALSE
    }
    
    
    # Draw tangent line when differentiable
    if (differentiable) {
      
      abline(
        a = fa - tangent_slope * a,
        b = tangent_slope,
        lwd = 2
      )
      
    }
    
    
    # Draw vertical tangent at zero
    if (
      input$function_choice == "Vertical Tangent" &&
      a == 0
    ) {
      
      abline(
        v = a,
        lwd = 2
      )
      
    }
    
  })
  
  
  # Numerical diagnostics
  output$diagnostics <- renderText({
    
    fun <- f()
    a <- input$a
    h <- input$h
    
    fa <- fun(a)
    fah <- fun(a + h)
    
    difference_quotient <- (fah - fa) / h
    
    left_slope <- (fa - fun(a - h)) / h
    right_slope <- (fun(a + h) - fa) / h
    
    tangent_slope <- (left_slope + right_slope) / 2
    
    difference <- abs(left_slope - right_slope)
    
    scale <- max(
      1,
      abs(left_slope),
      abs(right_slope)
    )
    
    differentiable <- difference < (0.25 * scale)
    
    
    # Absolute value corner at zero
    if (
      input$function_choice == "|x|" &&
      a == 0
    ) {
      
      tangent_text <- "Does not exist"
      conclusion <- "Likely not differentiable at a"
      
      
      # Vertical tangent at zero
    } else if (
      input$function_choice == "Vertical Tangent" &&
      a == 0
    ) {
      
      tangent_text <- "Vertical"
      conclusion <- "Likely not differentiable at a"
      
      
      # Piecewise jump at zero
    } else if (
      input$function_choice == "Piecewise Function" &&
      a == 0
    ) {
      
      tangent_text <- "Does not exist"
      conclusion <- "Likely not differentiable at a"
      
      
      # Normal differentiable case
    } else if (differentiable) {
      
      tangent_text <- round(tangent_slope, 4)
      conclusion <- "Likely differentiable at a"
      
      
      # Other non-differentiable case
    } else {
      
      tangent_text <- "Does not exist"
      conclusion <- "Likely not differentiable at a"
      
    }
    
    
    paste(
      "a =", a,
      "\nh =", h,
      "\nf(a) =", round(fa, 4),
      "\nDifference quotient =", round(difference_quotient, 4),
      "\nLeft derivative estimate =", round(left_slope, 4),
      "\nRight derivative estimate =", round(right_slope, 4),
      "\nTangent slope estimate =", tangent_text,
      "\nConclusion:", conclusion
    )
    
  })
  
}

shinyApp(ui = ui, server = server)