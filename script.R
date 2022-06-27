rm(list = ls())

library(tidyverse)
library(rvest)

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
  coauthors = lapply(coauthors,trimws,"both")
  topics = (curr[grepl(pattern = "^Topics:", curr)] %>% gsub("Topics:", "", x = .) %>% str_trim %>% str_split(", "))[[1]]
  summary = curr[grepl(pattern = "^Summary:", curr)] %>% gsub("Summary:", "", x = .) %>% str_trim
  history_index = which(str_detect(curr, "^History:")) + 1
  history = curr[history_index:length(curr)]
  introduced_date = history[1] %>% str_sub(1, 10)
  last_action_date = history[length(history)] %>% str_sub(1, 10)
  
  return(
    data.frame(
      year = year,
      ID = bill_id,
      status = status,
      introduced_date = introduced_date,
      last_action_date = last_action_date,
      author = author,
      coauthors = I(list(coauthors)),
      summary = summary,
      topics = I(list(topics)),
      history = I(list(history))
    )
  )
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
bill_database$state <- str_sub(bill_database$ID,1,2)
# Extract bill num
bill_database$bill_num <- str_remove_all(str_sub(bill_database$ID,4)," ")
# Extract result
bill_database$final_status <- str_squish(str_split_fixed(bill_database$status,"-",2)[,1])
# Extract act num if signed
bill_database$act_num <- ifelse(bill_database$final_status=="Enacted",str_remove_all(str_split_fixed(bill_database$status,"-",2)[,2]," Act No. "),NA)
# Extract author name and party
bill_database$author_name <- ifelse(str_detect(bill_database$author,"\\([A-Z]{1,3}\\)"),
                                    trimws(str_remove_all(bill_database$author,"\\([A-Z]{1,3}\\)"),"both"),
                                    bill_database$author)
bill_database$author_party <- str_remove_all(str_extract(bill_database$author,"\\([A-Z]{1,3}\\)"),"[()]")
# Use open states to get leg info

categories <- c(
"Absentee Voting-Application for"
,"Absentee Voting-Distributing Ballots"
,"Absentee Voting-Early Voting/In-Person Absentee"
,"Absentee Voting-Eligibility"
,"Absentee Voting-Military/Overseas"
,"Absentee Voting-Misc."
,"Absentee Voting-MOVE Act"
,"Absentee Voting-No Excuse"
,"Absentee Voting-Permanent Status"
,"Absentee Voting-Returning Ballots"
,"Alt Voting Methods (Ranked Choice, etc)"
,"Audits-Post Election"
,"Ballot Access-Candidates"
,"Ballot Access-Parties"
,"Ballots-Format & Design"
,"Candidates-Qualifications for Office"
,"Candidates-Resign to Run"
,"Candidates-Withdrawal/Death"
,"Candidates-Write-in"
,"Challenges to Voters"
,"Contests"
,"Cost of Elections"
,"Counting Votes"
,"Cybersecurity"
,"Dates of Elections"
,"DREs-Paper Trail"
,"Election Crimes"
,"Election Data-Collection/Retention of"
,"Election Day Holiday"
,"Election Officials-Campaign Activities"
,"Election Officials-Local"
,"Election Officials-Statewide"
,"Election Results/Canvass, Reporting of"
,"Electioneering"
,"Electoral College"
,"Electoral College-National Popular Vote"
,"Emergencies/Disasters"
,"Exit Polling"
,"Fusion/Dual-Party"
,"Internet Voting"
,"Mail Voting"
,"Miscellaneous"
,"Offices-Method of Filling"
,"Political Parties"
,"Poll Watchers"
,"Poll Workers-Compensation"
,"Poll Workers-Misc."
,"Poll Workers-Selection/Qualifications of"
,"Poll Workers-Training"
,"Poll Workers-Youth"
,"Polling Places-Arrangement of/Procedures at"
,"Polling Places-Disabled Access"
,"Polling Places-Hours"
,"Polling Places-Locations"
,"Polling Places-Vote Centers"
,"Precinct Definition"
,"Primaries-Dates"
,"Primaries-Misc."
,"Primaries-Presidential"
,"Primaries-Runoff"
,"Primaries-Types"
,"Provisional Voting"
,"Recounts"
,"Registration Drives"
,"Registration-Application Form/Content"
,"Registration-Automatic"
,"Registration-Centralized Voter List"
,"Registration-Deadline"
,"Registration-Election Day"
,"Registration-Electronic"
,"Registration-ID Required"
,"Registration-List Maintenance"
,"Registration-Misc."
,"Registration-Preregistration"
,"Registration-Sale/Distribution/Use of Lists"
,"Run-Off Elections"
,"Special Elections"
,"Straight Ticket Voting"
,"Task Forces/Study Commissions/Interim Committees"
,"Vacancies"
,"Voter Education/Information"
,"Voter Identification"
,"Voters-Absence from Work"
,"Voters-Age"
,"Voters-Assistance to"
,"Voters-Felon Voting Rights"
,"Voters-Miscellaneous Qualifications"
,"Voting Equipment/Technology-Selection & Standards"
,"Voting System Testing/Security/Storage")
