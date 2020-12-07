
shinyUI(
  fluidPage(theme = "xapstyles.css",
    headerPanel("Sample Size Calculator"),
    fluidRow(
      column(12,
        tabsetPanel(
          tabPanel("Application",
            fluidPage(
              h3("Study type"),
              checkboxInput(inputId = "clustered", label = "Clustered design?", value = FALSE),
              checkboxInput(inputId = "binary", label = "Binary dependent variable?", value = FALSE),
                                         
              conditionalPanel(
                condition = "input.clustered == false & input.binary == false",
                continuousPowerCalcUI("continuous")
              ),
              conditionalPanel(
                condition = "input.clustered == false & input.binary == true",
                binaryPowerCalcUI("binary")
              ),
              conditionalPanel(
                condition = "input.clustered == true & input.binary == false",
                continuousClusPowerCalcUI("clus_continuous")
              ),
              conditionalPanel(
                condition = "input.clustered == true & input.binary == true",
                binaryClusPowerCalcUI("clus_binary")
              )
            )
          ),
          
          documentation_tab()

        )
      )
    )
  )
)

