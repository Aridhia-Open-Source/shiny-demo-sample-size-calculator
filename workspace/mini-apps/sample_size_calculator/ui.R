
library(shiny)

source("helpers.R")
source("continuousPowerCalc.R")
source("binaryPowerCalc.R")
source("continuousClusPowerCalc.R")
source("binaryClusPowerCalc.R")

shinyUI(fluidPage(
  headerPanel("Power Calculator"),
  fluidRow(
    column(12,
           wellPanel(
             helpText("This calculator can help you understand the power of your experimental
                      design to detect treatment effects. This will inform the sample size you
                      require to ensure your study is sufficiently powered.",
                      
                      p("You can choose between a standard design in which individuals are randomly
                      assigned to treatment or control, and a 
                      clustered design, in which groups of individuals are assigned to treatment
                      and control together."),
                      
                      p("In addition, a binary study design can be selected, with a binary outcome 
                        assessed between a", strong("control"), "and", strong("treated"), "group."),
                      p("Experiment with different values and how they affect the graphical output, to 
                        get a feel for how required sample size changes with power, treatment effect and 
                        standard deviation of the outcome. The required sample size is output at the bottom
                        of the app."),
                      br(),
                      p(strong("This R Shiny app is provided unsupported and at user's risk. If you
                               are planning to use this app to inform your study, please review the
                               code and ensure you are comfortable with the calculations made.")),
                      br()
                      ),
             checkboxInput(inputId = "clustered", label = "Clustered Design?", value = FALSE),
             checkboxInput(inputId = "binary", label = "Binary Dependent Variable?", value = FALSE)
           )
    )
  ),
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
))
