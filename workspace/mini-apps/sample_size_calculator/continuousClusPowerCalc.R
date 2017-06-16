continuousClusPowerCalcUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(4,
           wellPanel(
             selectInput(ns("alpha_c"), "Significance Level",
                         c("Alpha = 0.01" = 0.01, "Alpha = 0.05" = 0.05, "Alpha = 0.10" = 0.1),
                         selected = 0.05),
             numericInput(ns("tau_c"), "Treatment Effect Size", value = 5, min = 0, max = 40),
             numericInput(ns("sigma_c"), "Standard Deviation of Outcome Variable", value = 10, min = 0, max = 30),
             sliderInput(ns("ICC_c"), "Intra-cluster Correlation", value = 0.5, min = 0, max = 1),
             numericInput(ns("n_clus_per_arm_c"), "Number of Clusters per Arm", value = 40, min = 0, max = 200),
             sliderInput(ns("target_c"), "Power Target", value = 0.8, min = 0, max = 1),
             numericInput(ns("maxn_c"), "Maximum Number of Subjects", value = 2000, min = 0, max = 10000000)
           )
    ),
    column(8,
           wellPanel(
             plotOutput(ns("powerplot"))
           )
    ),
    column(12,
           wellPanel(
             tags$h3(htmlOutput(ns("nrequired")))
           )
    )
  )
}

continuousClusPowerCalc <- function(input, output, session, Ns_small, Ns_big) {
  
  betas_fun <- reactive({
    sigma <- input$sigma_c
    ICC <- input$ICC_c
    tau <- input$tau_c
    n_clus_per_arm <- input$n_clus_per_arm_c
    target <- input$target_c
    maxn <- input$maxn_c
    alpha <- as.numeric(input$alpha_c)
    betas_small <- power_calculator_cluster(mu_t = (60 + tau), mu_c = 60, sigma = sigma, ICC = ICC,
                                            n_clus_per_arm = n_clus_per_arm, alpha = alpha, N = Ns_small)
    betas_big <- power_calculator_cluster(mu_t = (60 + tau), mu_c = 60, sigma = sigma, ICC = ICC,
                                          n_clus_per_arm = n_clus_per_arm, alpha = alpha, N = Ns_big)
    big <- (sum(betas_small >= target, na.rm = TRUE) == 0 | maxn > 10000)
    return(list(betas_small = betas_small, betas_big = betas_big, big = big))
  })
  
  output$powerplot <- renderPlot({
    sigma <- input$sigma_c
    ICC <- input$ICC_c
    tau <- input$tau_c
    n_clus_per_arm <- input$n_clus_per_arm_c
    target <- input$target_c
    alpha <- as.numeric(input$alpha_c)
    maxn <- input$maxn_c
    results <- betas_fun()
    if(!results$big){
      plot(
        NA,
        ylim = c(0, 1),
        xlim = c(0, maxn),
        main = paste0(
          "Power Analysis: Hypothetical Treatment Effect = ",
          tau,
          "\nSD of outcome = ",
          sigma,
          "; ICC = ",
          ICC
        ),
        ylab = "Power (Probability of Statistical Significance)",
        xlab = "Number of Subjects"
      )
      lines(Ns_small, results$betas_small, lwd = 4, col = "green")
      lines(
        Ns_small,
        power_calculator(mu_t = (60 + tau), mu_c = 60, sigma = sigma, alpha = alpha, N = Ns_small),
        lwd = 4,
        col = "blue"
      )
      abline(h = target, col = "red", lty = 2)
      legend(
        "bottomright",
        legend = c("Equivalent Individual-Level Design", "Clustered Design"),
        col = c("blue", "green"),
        lwd = c(4, 4),
        lty = c(1, 1)
      )
    }
    
    if(results$big){
      plot(
        NA,
        ylim = c(0, 1),
        xlim = c(0, maxn),
        main = paste0(
          "Power Analysis: Hypothetical Treatment Effect = ",
          tau,
          "\nSD of outcome = ",
          sigma,
          "; ICC = ",
          ICC
        ),
        ylab = "Power (Probability of Statistical Significance)",
        xlab = "Number of Subjects"
      )
      lines(Ns_big, results$betas_big, lwd = 4, col = "green")
      lines(
        Ns_big,
        power_calculator(mu_t = (60 + tau), mu_c = 60, sigma = sigma, alpha = alpha, N = Ns_big),
        lwd = 4,
        col = "blue"
      )
      abline(h = target, col = "red", lty = 2)
      legend(
        "bottomright",
        legend = c("Equivalent Individual-Level Design", "Clustered Design"),
        col = c("blue", "green"),
        lwd = c(4, 4),
        lty = c(1, 1)
      )
    }
  })
  
  output$nrequired <- renderUI({
    sigma <- input$sigma_c
    ICC <- input$ICC_c
    tau <- input$tau_c
    n_clus_per_arm <- input$n_clus_per_arm_c
    target <- input$target_c
    alpha <- as.numeric(input$alpha_c)
    maxn <- input$maxn_c
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
        "</strong>, or an average of at least <strong>",
        round(nrequired / (n_clus_per_arm * 2)),
        "</strong> subjects in each of <strong>",
        n_clus_per_arm * 2,
        "</strong> clusters. Right-click to download image."
      )
    if (sum(results$betas_big >= target, na.rm = TRUE) == 0) {
      str1 <-
        paste0(
          "In order to achieve <strong>",
          target * 100,
          "% power</strong>, you'll need to use a sample size of more than <strong>10,000,000</strong>. You may need to increase the number of clusters."
        )
    }
    
    HTML(str1)
  })
  
}

