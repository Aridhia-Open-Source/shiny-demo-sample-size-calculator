binaryClusPowerCalcUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(4,
      wellPanel(
        selectInput(ns("alpha_bc"), "Significance level",
                    c("Alpha = 0.01" = 0.01, "Alpha = 0.05" = 0.05, "Alpha = 0.10" = 0.1),
                    selected = 0.05),
        numericInput(ns("p0_bc"), "Proportion (DV = 1) in control group", value = 0.5, min = 0, max = 1),
        numericInput(ns("p1_bc"), "Proportion (DV = 1) in treatment group", value = 0.65, min = 0, max = 1),
        sliderInput(ns("ICC_bc"), "Intra-cluster correlation", value = 0.5, min = 0, max = 1),
        numericInput(ns("n_clus_per_arm_bc"), "Number of clusters per arm", value = 40, min = 0, max = 200),
        sliderInput(ns("target_bc"), "Power target", value = 0.8, min = 0, max = 1),
        numericInput(ns("maxn_bc"), "Maximum number of subjects", value = 2000, min = 0, max = 10000000)
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

binaryClusPowerCalc <- function(input, output, session, Ns_small, Ns_big) {
  
  betas_fun <- reactive({
    p0 <- input$p0_bc
    p1 <- input$p1_bc
    ICC <- input$ICC_bc
    n_clus_per_arm <- input$n_clus_per_arm_bc
    target <- input$target_bc
    maxn <- input$maxn_bc
    alpha <- as.numeric(input$alpha_bc)
    betas_small <- power_calculator_binary_cluster(p0 = p0, p1 = p1, ICC = ICC, n_clus_per_arm = n_clus_per_arm,
                                                   alpha = alpha, N = Ns_small)
    betas_big <- power_calculator_binary_cluster(p0 = p0, p1 = p1, ICC = ICC, n_clus_per_arm = n_clus_per_arm,
                                                 alpha = alpha, N = Ns_big)
    big <- (sum(betas_small >= target, na.rm = TRUE) == 0 | maxn > 10000)
    return(list(betas_small = betas_small, betas_big = betas_big, big = big))
  })
  
  output$powerplot <- renderPlot({
    p0 <- input$p0_bc
    p1 <- input$p1_bc
    ICC <- input$ICC_bc
    n_clus_per_arm <- input$n_clus_per_arm_bc
    target <- input$target_bc
    alpha <- as.numeric(input$alpha_bc)
    maxn <- input$maxn_bc
    results <- betas_fun()
    if(!results$big){
      plot(
        NA,
        ylim = c(0, 1),
        xlim = c(0, maxn),
        main = paste0(
          "Power analysis: hypothetical treatment effect = ",
          round(abs(p1 - p0), 3),
          " Percentage points; \n ICC = ",
          ICC
        ),
        ylab = "Power (probability of statistical significance)",
        xlab = "Number of subjects"
      )
      lines(Ns_small, results$betas_small, lwd = 4, col = "green")
      lines(
        Ns_small,
        power_calculator_binary(p1 = p1, p0 = p0, alpha = alpha, N = Ns_small),
        lwd = 4,
        col = "#2C88A2"
      )
      abline(h = target, col = "#850F19", lty = 2)
      legend(
        "bottomright",
        legend = c("Equivalent individual-level design", "Clustered design"),
        col = c("#2C88A2", "#3C8556"),
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
          "Power analysis: hypothetical treatment effect = ",
          round(abs(p1 - p0), 3),
          " Percentage points; \n ICC = ",
          ICC
        ),
        ylab = "Power (probability of statistical significance)",
        xlab = "Number of subjects"
      )
      lines(Ns_big, results$betas_big, lwd = 4, col = "#3C8556")
      lines(
        Ns_big,
        power_calculator_binary(p1 = p1, p0 = p0, alpha = alpha, N = Ns_big),
        lwd = 4,
        col = "#2C88A2"
      )
      abline(h = target, col = "#850F19", lty = 2)
      legend(
        "bottomright",
        legend = c("Equivalent individual-level design", "Clustered design"),
        col = c("#2C88A2", "#3C8556"),
        lwd = c(4, 4),
        lty = c(1, 1)
      )
    }
  })
  
  output$nrequired <- renderUI({
    p0 <- input$p0_bc
    p1 <- input$p1_bc
    ICC <- input$ICC_bc
    n_clus_per_arm <- input$n_clus_per_arm_bc
    target <- input$target_bc
    alpha <- as.numeric(input$alpha_bc)
    maxn <- input$maxn_bc
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
        "%</strong> power, you'll need to use a sample size of at least ",
        nrequired,
        ", or an average of at least <strong>",
        round(nrequired / (n_clus_per_arm * 2)),
        "</strong> subjects in each of <strong>",
        n_clus_per_arm * 2,
        "</strong> clusters."
      )
    if (sum(results$betas_big >= target, na.rm = TRUE) == 0) {
      str1 <-
        paste0(
          "In order to achieve <strong>",
          target * 100,
          "%</strong> power, you'll need to use a sample size of more than <strong>10,000,000</strong>. You may need to increase the number of clusters."
        )
    }
    
    HTML(str1)
  })
}



