continuousPowerCalcUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(4,
      wellPanel(
        selectInput(ns("alpha"), "Significance level",
                    c("Alpha = 0.01" = 0.01, "Alpha = 0.05" = 0.05, "Alpha = 0.10" = 0.1), 
                    selected = 0.05),
        numericInput(ns("tau"), "Treatment effect size", value = 5, min = 0, max = 40),
        numericInput(ns("sigma"), "Standard deviation of outcome variable", value = 10, min = 0, max = 30),
        sliderInput(ns("target"), "Power target", value = 0.8, min = 0, max = 1),
        numericInput(ns("maxn"), "Maximum number of subjects", value = 2000, min = 0, max = 10000000)
      )
    ),
    column(6,
      plotOutput(ns("powerplot"))
    ),
    column(2,
      wellPanel(
        tags$h3(htmlOutput(ns("nrequired")))
      )
    )
  )
}

continuousPowerCalc <- function(input, output, session, Ns_small, Ns_big) {
  
  betas_fun <- reactive({
    sigma <- input$sigma
    tau <- input$tau
    target <- input$target
    maxn <- input$maxn
    alpha <- as.numeric(input$alpha)
    betas_small <- power_calculator(mu_t = 60 + tau, mu_c = 60, sigma = sigma, alpha = alpha, N = Ns_small)
    betas_big <- power_calculator(mu_t = 60 + tau, mu_c = 60, sigma = sigma, alpha = alpha, N = Ns_big)
    big <- (sum(betas_small >= target, na.rm = TRUE) == 0 | maxn > 10000)
    return(list(betas_small = betas_small, betas_big = betas_big, big = big))
  })
  
  output$powerplot <- renderPlot({
    sigma <- input$sigma
    tau <- input$tau
    target <- input$target
    maxn <- input$maxn
    alpha <- as.numeric(input$alpha)
    results <- betas_fun()
    if(!results$big){
      plot(NA, ylim = c(0, 1), xlim = c(0, maxn),
           main = paste0("Power analysis: hypothetical treatment effect = ",
                         tau, "\nSD of outcome = ", sigma),
           ylab = "Power (probability of statistical significance)",
           xlab = "Number of subjects")
      lines(Ns_small, results$betas_small, lwd = 4, col = "#2C88A2")
      abline(h = target, col = "#850F19", lty = 2)
    }
    
    if(results$big){
      plot(
        NA,
        ylim = c(0, 1),
        xlim = c(0, maxn),
        main = paste0(
          "Power analysis: hypothetical treatment effect = ",
          tau,
          "\nSD of outcome = ",
          sigma
        ),
        ylab = "Power (probability of statistical significance)",
        xlab = "Number of subjects"
      )
      lines(Ns_big, results$betas_big, lwd = 4, col = "#2C88A2")
      abline(h = target, col = "#850F19", lty = 2)
    }
  })
  
  output$nrequired <- renderUI({
    sigma <- input$sigma
    tau <- input$tau
    target <- input$target
    alpha <- as.numeric(input$alpha)
    results <- betas_fun()
    if (!results$big) {
      nrequired <- Ns_small[which.max(results$betas_small >= target)]
    }
    if (results$big) {
      nrequired <- Ns_big[which.max(results$betas_big >= target)]
    }
    
    str1 <-
      paste0(
        "In order to achieve <strong>",
        target * 100,
        "% power</strong>, you'll need to use a sample size of at least <strong>",
        nrequired,
        "</strong>."
      )
    if (sum(results$betas_big >= target, na.rm = TRUE) == 0) {
      str1 <-
        paste0(
          "In order to achieve <strong>",
          target * 100,
          "% power</strong>, you'll need to use a sample size of more than <strong>10,000,000</strong>."
        )
    }
    
    HTML(str1)
  })
}


