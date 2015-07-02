#################################################################################################
# This is the main script used to control the production of FAO statistical pocketbook workflow #
#
################################################################################################

# libraries

# load the sybdata

# sanitise the data

# Create the folder structure


## Chapters to include

include_part1 <- TRUE
include_part2 <- TRUE
include_part3 <- FALSE
include_part4 <- FALSE

include_country_profiles <- FALSE
include_metadata <- FALSE

setwd("~/btsync/fao_sync/pocketbooks/regional15/project")
knitr::knit("syb_main.Rnw")
#system('pdflatex syb_main.tex')
setwd("~/btsync/fao_sync/pocketbooks/regional15")