
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
             helpText("This calculator can help you understand the power of yourexperimental
                      design to detect treatment effects.You can choose between a standard design
                      in which individuals are randomly assigned to treatment or control and a 
                      clustered design, in which groups of individuals are assigned to treatment
                      and control together."),
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
