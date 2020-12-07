documentation_tab <- function(){
  tabPanel("Help",
         fluidPage(width = 12,
                   fluidRow(
                     column(6,
                            h3("Calculating required study sample size"),
                            p("This mini-app acts as a calculator, helping you understand the power of your
                    experimental design to detect treatment effects. This will inform the sample 
                    size you require to ensure your study is sufficiently powered."),
                            h4("Mini-app layout"),
                            p("The mini-app contains two tabs; this Help tab gives you an overview 
                    of the mini-app itself. The Application tab allows you to select and run the 
                    required study type."),
                            h4("To use the mini-app"),
                            p("To experiment with how study type affects the required sample size, you can: "),
                            tags$ol(
                              tags$li("Choose a ", strong("standard design "), 
                                      "in which individuals are randomly assigned to treatment or control."),
                              tags$li("Choose a ", strong("clustered design"), "in which groups of individuals are assigned to treatment and control together."),
                              tags$li("Opt for a ", strong("binary study design"), "with a binary outcome assessed between a control and treated group.")
                            ),
                            
                            p("Experiment with different values and how they affect the graphical output, to get a feel for how required sample size changes with 
                    power, treatment effect, and standard deviation of the outcome. The required sample size is displayed on the right of the app.")
                     ),
                     column(6,
                            h3("Walkthrough video"), 
                            tags$iframe(src="//www.youtube.com/embed/cHv0nUDAFdk?rel=0", types = "video/mp4", width = "100%", height = "350", frameborder="0", controls = NA),
                            p(class = "nb", "NB: This mini-app is for provided for demonstration purposes, is unsupported and is utilised at user's risk.
                    If you plan to use this mini-app to inform your study, please review the code and ensure you are comfortable with the calculations made before proceeding. ")
                     )
                   )
         )
)
}
