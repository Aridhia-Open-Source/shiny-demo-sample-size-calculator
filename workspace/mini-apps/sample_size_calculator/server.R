
Ns_small <- 1:10000
Ns_big <- c(seq(1, 9999, 1), seq(10000, 100000000, 1000))

server <- function(input, output, session) {
  callModule(continuousPowerCalc, "continuous", Ns_small, Ns_big)
  callModule(binaryPowerCalc, "binary", Ns_small, Ns_big)
  callModule(continuousClusPowerCalc, "clus_continuous", Ns_small, Ns_big)
  callModule(binaryClusPowerCalc, "clus_binary", Ns_small, Ns_big)
}
