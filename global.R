# Install packages if necessary

source("dependencies.R")


# Load packages

library(shiny)


# Source all files in the code folder

for (file in list.files("code", full.names = TRUE)){
  source(file, local = TRUE)
}