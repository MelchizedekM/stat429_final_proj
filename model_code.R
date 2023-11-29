if (!require('this.path')) install.packages('this.path')
if (!require('readxl')) install.packages("readxl")

library(this.path)
library(readxl)

# Read in the data
curdir <- this.path()
infla_rate <- read_excel(paste0(curdir, "data/inflation_rate.xlsx"))