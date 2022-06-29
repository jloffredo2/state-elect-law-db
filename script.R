rm(list = ls())

library(tidyverse)
library(rvest)
library(rjson)
library(lubridate)

splitAt <- function(x, pos) {
  out <- list()
  for (i in seq_along(pos)) {
    if (i == tail(seq_along(pos), 1)) {
      out[[i]] = x[pos[i]:length(x)]
    } else{
      out[[i]] = x[pos[i]:(pos[i + 1] - 1)]
    }
  }
  return(out)
}

extract_bill_info <- function(curr, year) {
  bill_id = curr[1]
  print(bill_id)
  status = curr[grepl(pattern = "^Status:", curr)] %>% gsub("Status:", "", x = .) %>% str_trim
  authors = (curr[grepl(pattern = "^Author:", curr)] %>% str_trim %>% str_split("Additional Authors:"))[[1]]
  author = authors[1] %>% gsub("Author:", "", x = .) %>% str_trim
  coauthors = ifelse(length(authors) > 1, (authors[2] %>% str_split(";")), NA)[[1]]
  coauthors = trimws(coauthors,"both")
  topics = (curr[grepl(pattern = "^Topics:", curr)] %>% gsub("Topics:", "", x = .) %>% str_trim %>% str_split(", "))[[1]]
  summary = curr[grepl(pattern = "^Summary:", curr)] %>% gsub("Summary:", "", x = .) %>% str_trim
  history_index = which(str_detect(curr, "^History:")) + 1
  history = curr[history_index:length(curr)] %>% str_trim
  # Check intro versus prefile date
  prefiled_date = ifelse(sum(str_detect(history, "PREFILED")) > 0 ,
                     history[str_detect(history, "PREFILED") == TRUE] %>% str_sub(1, 10),
                     NA)
  introduced_date = ifelse(sum(str_detect(history, "INTRODUCED")) > 0 ,
                           history[str_detect(history, "INTRODUCED") == TRUE] %>% str_sub(1, 10),
                           NA)
  last_action_date = ifelse(str_detect(history[length(history)],".*[0-9].*"),history[length(history)] %>% str_sub(1, 10),history[length(history)-1] %>% str_sub(1, 10))
  
  return(
    data.frame(
      YEAR = year,
      ID = bill_id,
      STATUS = status,
      PREFILEDDATE = prefiled_date,
      INTRODUCEDDATE = introduced_date,
      LASTACTIONDATE = last_action_date,
      AUTHOR = author,
      COAUTHORS = toJSON(coauthors),
      SUMMARY = summary,
      TOPICS = toJSON(topics),
      HISTORY = toJSON(history)
    )
  )
}

check_topics <- function(json,topic){
  return(ifelse(sum(str_detect(fromJSON(json),topic))>0,1,0))
}

count_dem_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(fromJSON(json),"\\(D\\)")) + sum(str_detect(fromJSON(json),"\\(DFL\\)")),NA))
}

count_rep_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(fromJSON(json),"\\(R\\)")),NA))
}

count_coauthors <- function(json){
  return(ifelse(!is.na(json),length(fromJSON(json)),NA))
}

bill_database <- data.frame()

for (year in (2011:2021)) {
  print(year)
  input <- read_html(sprintf("input/ncsl/%i.html", year))
  
  text <- input %>%
    html_elements("#dnn_ctr71252_StateNetDB_linkList") %>%
    html_text2()
  
  (text = gsub(pattern = "[ ]+", replacement = " ", text))
  thetext = strsplit(text, split = "\r\n\r\n\n\r\n\r\n\r")[[1]]
  (html_text = trimws(thetext[thetext != "\r"] %>% str_remove_all("\r"), "both"))
  
  (split_text <- str_split(html_text, "\n"))
  for (s in 1:length(split_text)) {
    curr = split_text[[s]]
    (curr = trimws(curr[curr != "" &
                          curr != sprintf(" %i", year) &
                          !str_detect(curr, "Associated Bills:") &
                          curr != "Bill Text Lookup" & !(curr %in% state.name)], "both"))
    # Add a check if two bills caught
    if (length(which(curr == "History: Click for History")) != 1) {
      # need to split curr, add second to next element in list
      split_indicies = which(str_detect(curr, "^[A-Z]{2}[:space:][A-Z]{1,4}[:space:][0-9]+"))
      if (length(split_indicies) > 1) {
        split <- splitAt(curr, split_indicies)
        for (curr_split in split) {
          bill_database <- rbind(bill_database, extract_bill_info(curr_split, year))
        }
      }
    } else{
      bill_database <- rbind(bill_database, extract_bill_info(curr, year))
    }
  }
}

save(bill_database, file = "scrape_dataset.RData")

load("scrape_dataset.RData")
# Extract state
bill_database$STATE <- str_sub(bill_database$ID,1,2)
# Extract bill num
bill_database$BILLNUM <- str_remove_all(str_sub(bill_database$ID,4)," ")
# Extract result
bill_database$BILLSTATUS <- str_squish(str_split_fixed(bill_database$STATUS,"-",2)[,1])
# Extract act num if signed
bill_database$ACTNUM <- ifelse(bill_database$BILLSTATUS=="Enacted",str_remove_all(str_split_fixed(bill_database$STATUS,"-",2)[,2]," Act No. "),NA)
# Extract author name and party
bill_database$AUTHORNAME <- ifelse(str_detect(bill_database$AUTHOR,"\\([A-Z]{1,3}\\)"),
                                    trimws(str_remove_all(bill_database$AUTHOR,"\\([A-Z]{1,3}\\)"),"both"),
                                    bill_database$AUTHOR)
bill_database$AUTHORPARTY <- str_remove_all(str_extract(bill_database$AUTHOR,"\\([A-Z]{1,3}\\)"),"[()]")
# Count cosponsors
bill_database$COAUTHORS[bill_database$COAUTHORS=="\"NA\""] = NA
bill_database$NCOAUTHORS = sapply(bill_database$COAUTHORS,count_coauthors)
bill_database$NDEMCOAUTHORS = sapply(bill_database$COAUTHORS,count_dem_coauthors)
bill_database$NREPCOAUTHORS = sapply(bill_database$COAUTHORS,count_rep_coauthors)

# Categorize wide
bill_database$AVAPPL = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-Application for")
bill_database$AVBDIS = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-Distributing Ballots")
bill_database$AVEVIP = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-Early Voting/In-Person Absentee")
bill_database$AVELIG = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-Eligibility")
bill_database$AVMIOV = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-Military/Overseas")
bill_database$AVMISC = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-Misc.")
bill_database$AVMOVE = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-MOVE Act")
bill_database$AVNOEX = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-No Excuse")
bill_database$AVPERM = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-Permanent Status")
bill_database$AVBRET = sapply(bill_database$TOPICS, check_topics, "Absentee Voting-Returning Ballots")
bill_database$VOTEME = sapply(bill_database$TOPICS, check_topics, "Alt Voting Methods (Ranked Choice, etc)")
bill_database$AUDITS = sapply(bill_database$TOPICS, check_topics, "Audits-Post Election")
bill_database$BACAND = sapply(bill_database$TOPICS, check_topics, "Ballot Access-Candidates")
bill_database$BAPART = sapply(bill_database$TOPICS, check_topics, "Ballot Access-Parties")
bill_database$BALDES = sapply(bill_database$TOPICS, check_topics, "Ballots-Format & Design")
bill_database$CANQUL = sapply(bill_database$TOPICS, check_topics, "Candidates-Qualifications for Office")
bill_database$CANRTR = sapply(bill_database$TOPICS, check_topics, "Candidates-Resign to Run")
bill_database$CANWDW = sapply(bill_database$TOPICS, check_topics, "Candidates-Withdrawal/Death")
bill_database$CANWRI = sapply(bill_database$TOPICS, check_topics, "Candidates-Write-in")
bill_database$VTRCHA = sapply(bill_database$TOPICS, check_topics, "Challenges to Voters")
bill_database$CNTEST = sapply(bill_database$TOPICS, check_topics, "Contests")
bill_database$ELCOST = sapply(bill_database$TOPICS, check_topics, "Cost of Elections")
bill_database$VCOUNT = sapply(bill_database$TOPICS, check_topics, "Counting Votes")
bill_database$CYBSEC = sapply(bill_database$TOPICS, check_topics, "Cybersecurity")
bill_database$ELDATE = sapply(bill_database$TOPICS, check_topics, "Dates of Elections")
bill_database$PTDRES = sapply(bill_database$TOPICS, check_topics, "DREs-Paper Trail")
bill_database$CRIMES = sapply(bill_database$TOPICS, check_topics, "Election Crimes")
bill_database$DATART = sapply(bill_database$TOPICS, check_topics, "Election Data-Collection/Retention of")
bill_database$EDHOLI = sapply(bill_database$TOPICS, check_topics, "Election Day Holiday")
bill_database$EOCAMP = sapply(bill_database$TOPICS, check_topics, "Election Officials-Campaign Activities")
bill_database$EOLOCA = sapply(bill_database$TOPICS, check_topics, "Election Officials-Local")
bill_database$EOSTWD = sapply(bill_database$TOPICS, check_topics, "Election Officials-Statewide")
bill_database$REPRES = sapply(bill_database$TOPICS, check_topics, "Election Results/Canvass, Reporting of")
bill_database$ELEING = sapply(bill_database$TOPICS, check_topics, "Electioneering")
bill_database$ELECOL = sapply(bill_database$TOPICS, check_topics, "Electoral College")
bill_database$ECONPV = sapply(bill_database$TOPICS, check_topics, "Electoral College-National Popular Vote")
bill_database$EMEDIS = sapply(bill_database$TOPICS, check_topics, "Emergencies/Disasters")
bill_database$EXPOLL = sapply(bill_database$TOPICS, check_topics, "Exit Polling")
bill_database$DUALFU = sapply(bill_database$TOPICS, check_topics, "Fusion/Dual-Party")
bill_database$INVOTE = sapply(bill_database$TOPICS, check_topics, "Internet Voting")
bill_database$MAILVO = sapply(bill_database$TOPICS, check_topics, "Mail Voting")
bill_database$MISCEL = sapply(bill_database$TOPICS, check_topics, "Miscellaneous")
bill_database$FILING = sapply(bill_database$TOPICS, check_topics, "Offices-Method of Filling")
bill_database$POLPAR = sapply(bill_database$TOPICS, check_topics, "Political Parties")
bill_database$POLWAT = sapply(bill_database$TOPICS, check_topics, "Poll Watchers")
bill_database$PWCOMP = sapply(bill_database$TOPICS, check_topics, "Poll Workers-Compensation")
bill_database$PWMISC = sapply(bill_database$TOPICS, check_topics, "Poll Workers-Misc.")
bill_database$PWQUAL = sapply(bill_database$TOPICS, check_topics, "Poll Workers-Selection/Qualifications of")
bill_database$PWTRAI = sapply(bill_database$TOPICS, check_topics, "Poll Workers-Training")
bill_database$PWYOTH = sapply(bill_database$TOPICS, check_topics, "Poll Workers-Youth")
bill_database$PPPROC = sapply(bill_database$TOPICS, check_topics, "Polling Places-Arrangement of/Procedures at")
bill_database$PPACES = sapply(bill_database$TOPICS, check_topics, "Polling Places-Disabled Access")
bill_database$PPVHRS = sapply(bill_database$TOPICS, check_topics, "Polling Places-Hours")
bill_database$PPLOCA = sapply(bill_database$TOPICS, check_topics, "Polling Places-Locations")
bill_database$PPVCEN = sapply(bill_database$TOPICS, check_topics, "Polling Places-Vote Centers")
bill_database$PREDEF = sapply(bill_database$TOPICS, check_topics, "Precinct Definition")
bill_database$PRIDAT = sapply(bill_database$TOPICS, check_topics, "Primaries-Dates")
bill_database$PRIMIS = sapply(bill_database$TOPICS, check_topics, "Primaries-Misc.")
bill_database$PRIPUS = sapply(bill_database$TOPICS, check_topics, "Primaries-Presidential")
bill_database$PRIRNF = sapply(bill_database$TOPICS, check_topics, "Primaries-Runoff")
bill_database$PRITYP = sapply(bill_database$TOPICS, check_topics, "Primaries-Types")
bill_database$PROVOT = sapply(bill_database$TOPICS, check_topics, "Provisional Voting")
bill_database$RECOUN = sapply(bill_database$TOPICS, check_topics, "Recounts")
bill_database$REGDRI = sapply(bill_database$TOPICS, check_topics, "Registration Drives")
bill_database$REGAPP = sapply(bill_database$TOPICS, check_topics, "Registration-Application Form/Content")
bill_database$REGATO = sapply(bill_database$TOPICS, check_topics, "Registration-Automatic")
bill_database$REGCVL = sapply(bill_database$TOPICS, check_topics, "Registration-Centralized Voter List")
bill_database$REGDTE = sapply(bill_database$TOPICS, check_topics, "Registration-Deadline")
bill_database$REGEDY = sapply(bill_database$TOPICS, check_topics, "Registration-Election Day")
bill_database$REGELE = sapply(bill_database$TOPICS, check_topics, "Registration-Electronic")
bill_database$REGIDR = sapply(bill_database$TOPICS, check_topics, "Registration-ID Required")
bill_database$REGLST = sapply(bill_database$TOPICS, check_topics, "Registration-List Maintenance")
bill_database$REGMSC = sapply(bill_database$TOPICS, check_topics, "Registration-Misc.")
bill_database$REGPRE = sapply(bill_database$TOPICS, check_topics, "Registration-Preregistration")
bill_database$REGSDL = sapply(bill_database$TOPICS, check_topics, "Registration-Sale/Distribution/Use of Lists")
bill_database$RUNOFF = sapply(bill_database$TOPICS, check_topics, "Run-Off Elections")
bill_database$SPELEC = sapply(bill_database$TOPICS, check_topics, "Special Elections")
bill_database$STVOTE = sapply(bill_database$TOPICS, check_topics, "Straight Ticket Voting")
bill_database$TFSCIC = sapply(bill_database$TOPICS, check_topics, "Task Forces/Study Commissions/Interim Committees")
bill_database$VACNCY = sapply(bill_database$TOPICS, check_topics, "Vacancies")
bill_database$VEDINF = sapply(bill_database$TOPICS, check_topics, "Voter Education/Information")
bill_database$VOTRID = sapply(bill_database$TOPICS, check_topics, "Voter Identification")
bill_database$VOTAFW = sapply(bill_database$TOPICS, check_topics, "Voters-Absence from Work")
bill_database$VOTAGE = sapply(bill_database$TOPICS, check_topics, "Voters-Age")
bill_database$VOTAST = sapply(bill_database$TOPICS, check_topics, "Voters-Assistance to")
bill_database$VOTFVR = sapply(bill_database$TOPICS, check_topics, "Voters-Felon Voting Rights")
bill_database$VOTMQU = sapply(bill_database$TOPICS, check_topics, "Voters-Miscellaneous Qualifications")
bill_database$TECHSS = sapply(bill_database$TOPICS, check_topics, "Voting Equipment/Technology-Selection & Standards")
bill_database$VSSCST = sapply(bill_database$TOPICS, check_topics, "Voting System Testing/Security/Storage")

topic_cols = sort(colnames(bill_database)[21:109])
state_elect_law_db <- bill_database %>%
  select(YEAR
         ,STATE
         ,BILLNUM
         ,BILLSTATUS
         ,ACTNUM
         ,AUTHORNAME
         ,AUTHORPARTY
         ,PREFILEDDATE
         ,INTRODUCEDDATE
         ,LASTACTIONDATE
         ,NCOAUTHORS
         ,NDEMCOAUTHORS
         ,NREPCOAUTHORS
         ,topic_cols
         ,COAUTHORS
         ,HISTORY) %>%
  mutate(STATE = as.factor(STATE)
         ,BILLSTATUS = as.factor(BILLSTATUS)
         ,AUTHORPARTY = as.factor(AUTHORPARTY)
         ,PREFILEDDATE = mdy(PREFILEDDATE)
         ,INTRODUCEDDATE = mdy(INTRODUCEDDATE)
         ,LASTACTIONDATE = mdy(LASTACTIONDATE))

write.csv(state_elect_law_db, file = "state_elect_law_db.csv",row.names = FALSE)
save(state_elect_law_db, file = "state_elect_law_db.Rdata")
