
# Packages for the analysis
pkgs <- c("tibble", "magrittr", "tidyverse","knitr","dplyr",
          "kableExtra", "purrr", "DT", "jsonlite", "reshape2", "yaml", "plotly", "viridis", "collapsibleTree",  "sunburstR",
          "htmltools", "d3r", "readxl", "tidyr", "networkD3" )

lapply(pkgs, require, character.only = TRUE) # Loading the packages

rm(pkgs)