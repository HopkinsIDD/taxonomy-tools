## USER needs to update this section
#setwd("~/Documents/cholera-taxonomy")  ## enter the path to the cholera taxonomy directory
args=(commandArgs(TRUE))
if (length(args) == 1) {
    cholera.id <- as.numeric(args[[1]])
} else {
    stop("Please provide only the cholera taxonomy ID number")
}

source("Source/R/checkNewEntry.R")
checkNewEntry(cholera.id=cholera.id)

