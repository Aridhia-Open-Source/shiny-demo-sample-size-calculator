##################
####### UI #######
##################


shinyUI(
  fluidPage(
    theme = "xapstyles.css",
    
    # Header
    headerPanel("Sample Size Calculator"),
    
    fluidRow(
      column(12,
        
      # Panels
      tabsetPanel(
        
          # App Panel ----------------------------------------------------------
          tabPanel("Application",
                   
            fluidPage(
              # Choose characteristics of your study
              h3("Study type"),
              p("Tick characteristics of your study"),
              checkboxInput(inputId = "clustered", label = "Clustered design?", value = FALSE),
              checkboxInput(inputId = "binary", label = "Binary dependent variable?", value = FALSE),
              
              # Continuous calculations if clustered & binary = FALSE                           
              conditionalPanel(
                condition = "input.clustered == false & input.binary == false",
                continuousPowerCalcUI("continuous")
              ),
              
              # Binary calculations if binary = TRUE & clustered = FALSE
              conditionalPanel(
                condition = "input.clustered == false & input.binary == true",
                binaryPowerCalcUI("binary")
              ),
              
              # Clustered calculations if clustered = TRUE & binary = FALSE
              conditionalPanel(
                condition = "input.clustered == true & input.binary == false",
                continuousClusPowerCalcUI("clus_continuous")
              ),
              
              # Binary Clustered calculations if clustered & binary = TRUE
              conditionalPanel(
                condition = "input.clustered == true & input.binary == true",
                binaryClusPowerCalcUI("clus_binary")
              )
            )
          ),
          
          # Help tab -----------------------------------------------------------
          documentation_tab()

        )
      )
    )
  )
)

