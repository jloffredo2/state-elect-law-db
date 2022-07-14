rm(list = ls())

library(tidyverse)
library(rvest)
library(lubridate)

# load scraper fxns
source("code/misc_fxns.R")
source("code/ncsl.R")

# Build scraped datasets
ncsl_bill_database <- build_ncsl_bill_database()

# Save outputs
write.csv(ncsl_bill_database, file = "output/ncsl_bill_database.csv",row.names = FALSE)
save(ncsl_bill_database, file = "output/ncsl_bill_database.Rdata")
save(ncsl_bill_database, file = "output/state_elect_law_db.Rdata")

# Edit codebook and readme
codebook = readLines("codebook.txt")
codebook[4] = sprintf("Date: %s",today())
codebook[10] = sprintf("  National Conference of State Legislatures. 2022. *State Elections Legislation Database*. www.ncsl.org/research/elections-and-campaigns/elections-legislation-database.aspx (%s)",(stamp("March 1, 1999")(today())))
writeLines(codebook, "codebook.txt")

readme = readLines("README.md")
readme[6] = sprintf("**Date**: %s", today())
readme[13] = sprintf("> National Conference of State Legislatures. 2022. *State Elections Legislation Database*. www.ncsl.org/research/elections-and-campaigns/elections-legislation-database.aspx (%s).",(stamp("March 1, 1999")(today())))
writeLines(readme, "README.md")
