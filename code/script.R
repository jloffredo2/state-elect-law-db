rm(list = ls())

library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(rvest)
library(stringr)
library(magrittr)
library(purrr)

# load scraper fxns
source("code/misc_fxns.R")
source("code/ncsl.R")
source("code/vrl.R")
source("code/update_documentation.R")
