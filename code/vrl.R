library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(stringr)
library(purrr)

source("code/misc_fxns.R")
source("code/vrl_topic_map.R")

## FUNCTIONS FOR SCRAPING VOTING RIGHTS LAB LEG TRACKER
# Function to count number of Dem coauthors (includes DFL in MN)
vrl_count_dem_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(rjson::fromJSON(json),"\\(D\\)")) + sum(str_detect(rjson::fromJSON(json),"\\(DFL\\)")),NA))
}

# Function to count number of Rep coauthors
vrl_count_rep_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(rjson::fromJSON(json),"\\(R\\)")),NA))
}

# Function to count total number of coauthors
vrl_count_coauthors <- function(json){
  return(ifelse(!is.na(json),length(rjson::fromJSON(json)),NA))
}

classify_tags <- function(tags,anti_voter_tags,pro_voter_tags,neutral_tags,mixed_tags) {
  tags <- str_trim(tags) |> na.omit()
  categorized_tags <- case_when(
    tags %in% anti_voter_tags ~ "R",
    tags %in% pro_voter_tags ~ "E",
    tags %in% neutral_tags  ~ "N",
    tags %in% mixed_tags ~ "M",
    TRUE ~ NA_character_
  )
  
  # count the frequency of each element in the vector
  freq_table <- table(categorized_tags)
  
  # find the index of the maximum frequency
  max_index <- which.max(freq_table)
  
  # extract the element(s) with the maximum frequency
  most_common <- names(freq_table)[freq_table == freq_table[max_index]]
  
  if(length(most_common) == 1){
    output = most_common
  } else if(length(most_common) == 4){
    output = 'M'
  } else if(setequal(most_common,c("E","R","M"))){
    output = 'M'
  } else if(setequal(most_common,c("E","R","N"))){
    output = 'M'
  } else if(setequal(most_common,c("E","N","M"))){
    output = 'E'
  } else if(setequal(most_common,c("R","N","M"))){
    output = 'R'
  } else if(setequal(most_common,c("E","R"))){
    output = 'M'
  } else if(setequal(most_common,c("E","M"))){
    output = 'E'
  } else if(setequal(most_common,c("R","M"))){
    output = 'R'
  } else if(setequal(most_common,c("E","N"))){
    output = 'E'
  } else if(setequal(most_common,c("R","N"))){
    output = 'R'
  } else {
    output = 'N'
  }
  
}

clean_classified_tags <- function(col,anti_voter_tags,pro_voter_tags,neutral_tags,mixed_tags){
  tag_lists <- str_split(col, ",")
  
  # Apply the classify_tags() function to each element of the list
  tag_class <- sapply(tag_lists, classify_tags,anti_voter_tags,pro_voter_tags,neutral_tags,mixed_tags)
  
  return(tag_class)
}     
  
  

build_vrl_bill_database <- function(){
  print("loading VRL json from local cache")
  cache_dir <- "data/vrl_cache"
  years_available <- c(2021, 2022, 2023, 2024, 2025, 2026)
  bills_list <- lapply(years_available, function(yr) {
    d <- jsonlite::fromJSON(file.path(cache_dir, paste0("bills-", yr, ".json")))$data
    rownames(d) <- NULL
    if (is.data.frame(d$tags)) {
      d$tags <- d$tags[, names(d$tags) != "", drop = FALSE]
    } else {
      d$tags <- lapply(d$tags, function(t) if (is.list(t)) t[names(t) != ""] else t)
    }
    d
  })
  bills <- bind_rows(bills_list)
  bills <- bills[!duplicated(bills$id), ]
  # Flatten tag-group list columns once; all three tables use them
  flatten_tags <- \(col) map_chr(col, ~if(is.null(.)) '' else str_c(if("tag" %in% names(.)) .[["tag"]] else .[[1]], collapse = ", "))
  bills_flat <- bills |>
    unnest(cols = tags, keep_empty = TRUE) |>
    mutate(across(c(starts_with("21"), `-Impact`), flatten_tags)) |>
    mutate(across(starts_with("21"), \(x) str_remove_all(x, "null")))
  tags <- jsonlite::fromJSON(file.path(cache_dir, "tags.json"))$data
  anti_voter_tags <- c("-Anti-Voter",tags$id[tags$name=="-Anti-Voter"],"Anti-voter",tags$id[tags$name=="Anti-voter"])
  pro_voter_tags <- c("-Pro-Voter",tags$id[tags$name=="-Pro-Voter"],"Pro-voter",tags$id[tags$name=="Pro-voter"])
  neutral_tags <- c("-Neutral",tags$id[tags$name=="-Neutral"],"Neutral",tags$id[tags$name=="Neutral"])
  mixed_tags <- c("-Mixed_Unclear",tags$id[tags$name=="-Mixed_Unclear"],"Mixed_Unclear",tags$id[tags$name=="Mixed_Unclear"])
  
  ####### CREATE VRL MAIN OUTPUT ######
  print("creating main output")
  #### Match up VRL with NCSL
  # Rename and change date to date type
  vrl_bill_database <- bills_flat |>
    rename(INTRODUCEDDATE = intro_date
           ,PREFILEDATE = prefile_date
           ,BILLTEXTURL = text_url
           ,BILLSUMMARY = summary
           ,VRLANALYSIS = public_commentary) |>
    mutate(INTRODUCEDDATE = mdy(INTRODUCEDDATE),
           PREFILEDATE = mdy(PREFILEDATE),
           YEAR = year(INTRODUCEDDATE))
  # Recode
  vrl_bill_database$BILLNUM = sprintf("%s%i", vrl_bill_database$legtype, vrl_bill_database$bill_number)
  vrl_bill_database$BILLSTATUS = recode_levels(vrl_bill_database$current_disposition,
                                            "Failed" = "Failed - Adjourned",
                                            "Carryover" = "Pending - Carryover",
                                            "To Executive" = "To Governor",
                                            "To Executive" = "To Congress",
                                            "Vetoed" = "Override Pending",
                                            "Enacted" = "Adopted")
  vrl_bill_database = vrl_bill_database |>
    mutate(BILLLOCATION = case_when(
      str_detect(bill_location, "Committee of the Whole") ~ "Floor",
      bill_location == "Conference Committee" ~ "Conference",
      bill_location == "Bills in Conference Committee" ~ "Conference",
      bill_location == "Bills Carrying Request Messages" ~ "Conference",
      str_detect(bill_location, "Reading") ~ 'Reading',
      str_detect(bill_location,"Committee") ~ "Committee",
      bill_location == "House Election Integrity" ~ "Committee",
      bill_location == "House Government Oversight" ~ "Committee",
      bill_location == "SENATE" ~ "Floor",
      bill_location == "HOUSE" ~ "Floor",
      bill_location == "Laid on Table" ~ "Floor",
      bill_location == "Legislature" ~ "Floor",
      bill_location == "ASSEMBLY" ~ "Floor",
      bill_location == "Held on Desk" ~ "Floor",
      str_detect(bill_location,"Consideration") ~ "Floor",
      bill_location == "Chaptered" ~ 'Passed',
      bill_location == "Chapter" ~ 'Passed',
      bill_location == "Signed by Governor" ~ 'Passed',
      bill_location == "Resolutions Referred" ~ 'Passed',
      bill_location == "Adopted" ~ 'Passed',
      bill_location == 'Died' ~ 'Failed',
      bill_location == 'Failed to Pass' ~ 'Failed',
      bill_location == 'Withdrawn' ~ 'Withdrawn',
      bill_location == "Withdrawn from further consideration"  ~ 'Withdrawn',
      bill_location == 'Postponed Indefinitely' ~ 'Withdrawn',
      bill_location == 'Indefinitely Postponed' ~ 'Withdrawn',
      bill_location == 'Tabled' ~ 'Withdrawn',
      bill_location == "Vetoed by Governor" ~ 'Vetoed',
      bill_location == "Governor's Veto" ~ 'Vetoed',
      bill_location == "Concurrence" ~ "Conference",
      bill_location == "Eligible for Governor" ~ "To Governor",
      bill_location == "To Governor" ~ "To Governor",
      bill_location == "Enacting Clause Struck" ~ "Substituted",
      bill_location == "Replaced by New Draft" ~ "Substituted",
      bill_location == "Became Law Without Governor's Signature" ~ "Passed",
      bill_location == "To Enrollment" ~ "Passed",
      bill_location == "Filed with Secretary of State" ~ "Passed",
      bill_location == "To Congress" ~ "To Governor",
      bill_location == "In Joint Session" ~ "Floor",
      bill_location == "Assembly Inactive File" ~ "Floor",
      bill_location == "Senate Inactive File" ~ "Floor",
      bill_location == "Vetoed" ~ "Vetoed",
      str_detect(bill_location, "Subcommittee") ~ "Committee",
      str_detect(bill_location, "Government, Military") ~ "Committee",
      str_detect(bill_location, "Legislative Commissioner") ~ "Committee"
      )) |> filter(!(bill_location %in% c("Council Floor","Eligible for Congress")))
  
  vrl_bill_database$AUTHORNAME = ifelse(str_detect(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),
         trimws(str_remove_all(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),"both"),
         vrl_bill_database$author)
  vrl_bill_database$AUTHORPARTY = str_remove_all(str_extract(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),"[()]")
  vrl_bill_database$LASTACTIONDATE = mdy(sapply(vrl_bill_database$status_actions, function(x){
    if(is.null(x) || nrow(x) == 0) return(NA_character_)
    tail(x$date, 1)
  }))
  vrl_bill_database$HISTORY = lapply(vrl_bill_database$status_actions, rjson::toJSON)
  vrl_bill_database$HISTORY = unlist(vrl_bill_database$HISTORY)
  vrl_bill_database$HISTORY[vrl_bill_database$HISTORY=="\"NA\""] = NA
  
  vrl_bill_database$COAUTHORS = lapply(str_split(vrl_bill_database$addl_auths,";"),rjson::toJSON)
  vrl_bill_database$COAUTHORS = unlist(vrl_bill_database$COAUTHORS)
  vrl_bill_database$COAUTHORS[vrl_bill_database$COAUTHORS=="\"NA\""] = NA

  vrl_bill_database$NCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_coauthors)
  vrl_bill_database$NDEMCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_dem_coauthors)
  vrl_bill_database$NREPCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_rep_coauthors)
  
  # VRL Rating
  vrl_bill_database <- vrl_bill_database |>
    mutate(VRLRATING = case_when(
      str_detect(`-Impact`, str_c(anti_voter_tags, collapse = "|")) ~ "R",
      str_detect(`-Impact`, str_c(pro_voter_tags, collapse = "|")) ~ "E",
      str_detect(`-Impact`, str_c(neutral_tags, collapse = "|")) ~ "N",
      str_detect(`-Impact`, str_c(mixed_tags, collapse = "|")) ~ "M"))
                                       
  vrl_bill_database <- build_topic_dummies(vrl_bill_database, vrl_topic_spec) |>
    mutate(EOGENR = as.integer(EOLOCA == 1 | EOSTWD == 1),
           PPGENR = as.integer(PPACES == 1 | PPLOCA == 1 | PPVCEN == 1 | PPVHRS == 1),
           REGGEN = as.integer(REGAGY == 1 | REGAPP == 1 | REGATO == 1 | REGDRV == 1 | REGDTE == 1 |
                                 REGEDY == 1 | REGELE == 1 | REGMSC == 1 | REGPRE == 1))
  for (g in names(vrl_general_cols)) {
    vrl_bill_database[[g]] <- as.integer(str_length(vrl_bill_database[[vrl_general_cols[[g]]]]) > 0)
  }
  vrl_bill_database <- vrl_bill_database |>
    mutate(across(c(NCOAUTHORS, NDEMCOAUTHORS, NREPCOAUTHORS), ~replace_na(., 0))) |>
    select(-starts_with("21"))
  
  topic_cols <- sort(c(names(vrl_topic_spec), "EOGENR", "PPGENR", "REGGEN"))
  
  # Produce final output
  vrl_bill_database <- vrl_bill_database |>
    mutate(UUID = str_c(state,YEAR,BILLNUM)) |>
    select(UUID
           ,YEAR
           ,STATE=state
           ,BILLNUM
           ,BILLSTATUS
           ,BILLLOCATION
           ,AUTHORNAME
           ,AUTHORPARTY
           ,PREFILEDATE
           ,INTRODUCEDDATE
           ,LASTACTIONDATE
           ,NCOAUTHORS
           ,NDEMCOAUTHORS
           ,NREPCOAUTHORS
           ,all_of(topic_cols)
           ,COAUTHORS
           ,HISTORY
           ,VRLRATING
           ,BILLTEXTURL
           ,BILLSUMMARY
           ,VRLANALYSIS
           ,starts_with("GENERAL")) |>
    mutate(
      STATE = as.factor(STATE)
      ,AUTHORPARTY = as.factor(AUTHORPARTY))
  
  ####### CREATE VRL PROVISIONS TABLE #######
  print("creating provisions table")
  vrl_provisions <- bills_flat |>
    mutate(
      year = year(mdy(intro_date))
      ,UUID = str_c(state, year, sprintf("%s%i", legtype, bill_number)))

  rating_cols <- c("-Impact", grep("^21", names(vrl_provisions), value = TRUE))
  for (col in rating_cols) {
    vrl_provisions[[col]] <- clean_classified_tags(vrl_provisions[[col]], anti_voter_tags, pro_voter_tags, neutral_tags, mixed_tags)
  }

  vrl_provisions <- vrl_provisions |> select(UUID,VRLRATING=`-Impact`,starts_with("21")) 

  colnames(vrl_provisions) <- str_remove_all(colnames(vrl_provisions),"21")

  ####### CREATE VRL PROCESS CHECK  #######
  print("creating process check table")
  vrl_process_check <- bills_flat |>
    mutate(
      year = year(mdy(intro_date))
      ,UUID = str_c(state, year, sprintf("%s%i", legtype, bill_number))) |>
    build_topic_dummies(vrl_process_spec, check = FALSE) |>
    select(UUID, starts_with("EXPAND"), starts_with("RESTRICT"))
  
  
  return(list(vrl_bill_database=vrl_bill_database
              ,vrl_provisions=vrl_provisions
              ,vrl_process_check=vrl_process_check))
}

db <- build_vrl_bill_database()

vrl_bill_database <- db$vrl_bill_database
write.csv(vrl_bill_database, file = "output/vrl_bill_database.csv",row.names = FALSE)
save(vrl_bill_database, file = "output/vrl_bill_database.Rdata")

vrl_provisions <- db$vrl_provisions
write.csv(vrl_provisions, file = "output/vrl_provisions.csv",row.names = FALSE)
save(vrl_provisions, file = "output/vrl_provisions.Rdata")

vrl_process_check <- db$vrl_process_check
write.csv(vrl_process_check, file = "output/vrl_process_check.csv",row.names = FALSE)
save(vrl_process_check, file = "output/vrl_process_check.Rdata")
