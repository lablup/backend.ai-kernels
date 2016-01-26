#! /usr/bin/env Rscript
r <- getOption("repos")
r["CRAN"] <- "http://cran.nexr.com/"
options(repos = r)

# Minimum requirements for REPL
install.packages("rzmq")
install.packages("rjson")

# Some useful user packages
install.packages("ggplot2")
