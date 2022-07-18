rm(list = ls())

library(tidyverse)
library(rvest)
library(lubridate)

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

save(vrl_bill_database, ncsl_bill_database, file = "output/state_elect_law_db.Rdata")

# Edit codebook and readme
codebook = readLines("../output/codebook.txt")
codebook[4] = sprintf("Date: %s",today())
codebook[10] = sprintf("  National Conference of State Legislatures. 2022. *State Elections Legislation Database*. www.ncsl.org/research/elections-and-campaigns/elections-legislation-database.aspx (%s)",(stamp("March 1, 1999")(today())))
codebook[12] = sprintf("  Voting Rights Lab. 2022. *Legislative Tracker*. https://tracker.votingrightslab.org/pending/search (%s)",(stamp("March 1, 1999")(today())))
writeLines(codebook, "../output/codebook.txt")

readme = readLines("../output/README.md")
readme[6] = sprintf("**Date**: %s", today())
readme[13] = sprintf("> National Conference of State Legislatures. 2022. *State Elections Legislation Database*. www.ncsl.org/research/elections-and-campaigns/elections-legislation-database.aspx (%s).",(stamp("March 1, 1999")(today())))
readme[15] = sprintf("> Voting Rights Lab. 2022. *Legislative Tracker*. https://tracker.votingrightslab.org/pending/search (%s).",(stamp("March 1, 1999")(today())))
writeLines(readme, "../output/README.md")
