######################
####### SERVER #######
######################


server <- function(input, output, session) {
  
  Ns_small <- 1:10000
  Ns_big <- c(seq(1, 9999, 1), seq(10000, 100000000, 1000))
  
  # Binary & clustered = FALSE
  callModule(continuousPowerCalc, "continuous", Ns_small, Ns_big)
  
  # Binary = TRUE & clustered = FALSE
  callModule(binaryPowerCalc, "binary", Ns_small, Ns_big)
  
  # Clustered = TRUE & binary = FALSE
  callModule(continuousClusPowerCalc, "clus_continuous", Ns_small, Ns_big)
  
  # Binary & Clustered = TRUE
  callModule(binaryClusPowerCalc, "clus_binary", Ns_small, Ns_big)
}
