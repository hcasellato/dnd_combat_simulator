### Code Summary: ########################################################################
# This code generates a character for the combat simulation.
#
### Basic packages: ######################################################################
repo <- "http://cran.us.r-project.org"

# Required packages:
if(!require(data.table)) install.packages("data.table", repos = repo, dependencies = TRUE)
if(!require(tidyverse))  install.packages("tidyverse",  repos = repo, dependencies = TRUE)
if(!require(caret))      install.packages("caret",      repos = repo, dependencies = TRUE)
if(!require(dplyr))      install.packages("dplyr",      repos = repo, dependencies = TRUE)

library(data.table)
library(tidyverse)
library(caret)
library(dplyr)

##########################################################################################





##########################################################################################