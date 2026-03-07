rm(list = ls())

library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(rvest)
library(stringr)
library(magrittr)
library(purrr)

# Download fresh VRL data via browser (bypasses Cloudflare)
message("Downloading VRL data...")
result <- system2("node", args = "code/download_vrl.js",
                  env = c(paste0("VRL_EMAIL=", Sys.getenv("VRL_EMAIL")),
                          paste0("VRL_PASSWORD=", Sys.getenv("VRL_PASSWORD"))),
                  stdout = TRUE, stderr = TRUE)
message(paste(result, collapse = "\n"))

# load scraper fxns
source("code/misc_fxns.R")
source("code/ncsl.R")
source("code/vrl.R")
source("code/update_documentation.R")
