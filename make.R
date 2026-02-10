rm(list = ls())

if (!require("devtools")) install.packages("devtools")
devtools::install_deps(upgrade = "never") # Installs missing ones
# If you just want to load them without installing:
# deps <- desc::desc_get_deps()$package
# lapply(deps, library, character.only = TRUE)

# 2. Load all functions in the /R folder
# We filter for .R files and exclude make.R itself if it's in the same root
message("Loading custom functions from /R...")
functions <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]$")

# Sourcing everything in /R (assuming your analysis scripts are elsewhere 
# or named differently to avoid sourcing them as functions)
lapply(functions, source)

# 3. Execution Steps
message("Starting analysis pipeline...")
# source("scripts/01_process_elevation.R")