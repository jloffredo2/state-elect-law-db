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

# Build scraped datasets
ncsl_bill_database <- build_ncsl_bill_database()
vrl_bill_database <- build_vrl_bill_database()

# Save outputs
write.csv(ncsl_bill_database, file = "output/ncsl_bill_database.csv",row.names = FALSE)
save(ncsl_bill_database, file = "output/ncsl_bill_database.Rdata")

write.csv(vrl_bill_database, file = "output/vrl_bill_database.csv",row.names = FALSE)
save(vrl_bill_database, file = "output/vrl_bill_database.Rdata")
