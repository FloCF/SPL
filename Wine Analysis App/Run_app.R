######################################################
##                THIS IS OUR WINE APP              ##
##                                                  ##
## Plese specify your working path before launching ##
######################################################

path = "/home/flocf/Documents/git/SPL/Wine Analysis App/"
setwd(path)

# List of all needed packages
packages_needed = c("shiny" ,"shinythemes", "purrr", "ggplot2", "corrplot", "glmnet", "caret", "ranger",
                    "e1071", "keras", "MASS", "nnet", "plotmo", "selectiveInference")

# Check if packages need to be installed
for (pack in packages_needed) {
  if (!(as.character(pack) %in% installed.packages())) {
    message("Installing the needed package ", as.character(pack))
    install.packages(as.character(pack))
  }
}

# Load all packages
invisible(lapply(packages_needed, require, character.only = TRUE, quietly = TRUE))

# Delete list of needed packages
rm(packages_needed)

# Load necessary functions
sapply(list.files(pattern="[.]R$", path="Wine_App/ui_server", full.names=TRUE), source)
sapply(list.files(pattern="[.]R$", path="Wine_App/functions", full.names=TRUE), source)

# Run the App
runApp("Wine_App", quiet = TRUE)#, launch.browser = TRUE)