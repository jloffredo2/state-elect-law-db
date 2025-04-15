library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(rvest)
library(stringr)
library(magrittr)
library(purrr)

source("code/misc_fxns.R")
## FUNCTIONS FOR SCRAPING NCSL

# Function for setting values for topic binary indicators
ncsl_check_topics <- function(json,topic){
  return(ifelse(sum(str_detect(rjson::fromJSON(json),topic))>0,1,0))
}

# Function to count number of Dem coauthors (includes DFL in MN)
ncsl_count_dem_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(rjson::fromJSON(json),"\\(D\\)")) + sum(str_detect(rjson::fromJSON(json),"\\(DFL\\)")),NA))
}

# Function to count number of Rep coauthors
ncsl_count_rep_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(rjson::fromJSON(json),"\\(R\\)")),NA))
}

# Function to count total number of coauthors
ncsl_count_coauthors <- function(json){
  return(ifelse(!is.na(json),length(rjson::fromJSON(json)),NA))
}

ncsl_extract_bill_info <- function(curr, year) {
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
      PREFILEDATE = prefiled_date,
      INTRODUCEDDATE = introduced_date,
      LASTACTIONDATE = last_action_date,
      AUTHOR = author,
      COAUTHORS = rjson::toJSON(coauthors),
      SUMMARY = summary,
      TOPICS = rjson::toJSON(topics),
      HISTORY = rjson::toJSON(history)
    )
  )
}

scrape_ncsl <- function(year){
  print("scraping NCSL webform")
  URL <- "https://www.ncsl.org/elections-and-campaigns/state-election-legislation-database"
  search <- html_form(read_html(URL, config = config(ssl_verifypeer = FALSE)))[[1]]
  
  params = list(
    "dnn$ctr31026$StateNetDB$ckBxAllTopics" = "on" 
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$0" = 'ABS-Request'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$1" = 'ABS-Processing'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$2" = 'ABS-Deliver'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$3" = 'ABS-EligibilityNoExc'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$4" = 'ABS-Return'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$5" = 'ABS-AllMail'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$6" = 'RCV/AltVoting'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$7" = 'BallotAccess'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$8" = 'Ballot'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$9" = 'Candidates'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$10" = 'Voter-Challenge'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$11" = 'Cost'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$12" = 'Count/Canvass'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$13" = 'Cybersecurity'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$14" = 'Date'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$15" = 'Early'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$16" = 'Contest/CourtChall'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$17" = 'Crimes'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$18" = 'Data/Records'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$19" = 'Official-Local'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$20" = 'Official-State'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$21" = 'Result/Report'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$22" = 'ElectionTech'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$23" = 'Electioneering'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$24" = 'ElectoralCollege'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$25" = 'Emergency'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$26" = 'Internet'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$27" = 'MISC'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$28" = 'Parties'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$29" = 'Watchers'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$30" = 'PollWorker'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$31" = 'Polls-Arrangement'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$32" = 'Polls-Hours/Location'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$33" = 'PostElecAudit'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$34" = 'Precinct'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$35" = 'Primary-Presidential'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$36" = 'Primary-St.DateRunof'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$37" = 'Primary-Type'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$38" = 'Provisional'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$39" = 'Recall'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$40" = 'Recount'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$41" = 'REG-AppEligID'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$42" = 'REG-Automatic'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$43" = 'REG-Deadline'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$44" = 'REG-Election_Day'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$45" = 'REG-List_Maint'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$46" = 'REG-Online'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$47" = 'REG-Pre-reg'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$48" = 'REG-Sale/ListAccess'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$49" = 'REG-Drives'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$50" = 'Special'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$51" = 'Taskforce'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$52" = 'Vacancy'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$53" = 'Voter-ED'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$54" = 'Voter-ID'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$55" = 'Voter-Elig/Qualific'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$56" = 'Voter-Felon'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$57" = 'Voter-MilitaryOvrSea'
    ,"dnn$ctr31026$StateNetDB$ckBxTopics$58" = 'Voter-Assist'
    ,"dnn$ctr31026$StateNetDB$ckBxAllStates" = "on"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$0" = "AL"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$1" = "AK"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$2" = "AZ"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$3" = "AR"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$4" = "CA"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$5" = "CO"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$6" = "CT"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$7" = "DE"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$8" = "DC"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$9" = "FL"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$10" = "GA"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$11" = "HI"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$12" = "ID"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$13" = "IL"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$14" = "IN"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$15" = "IA"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$16" = "KS"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$17" = "KY"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$18" = "LA"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$19" = "ME"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$20" = "MD"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$21" = "MA"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$22" = "MI"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$23" = "MN"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$24" = "MS"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$25" = "MO"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$26" = "MT"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$27" = "NE"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$28" = "NV"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$29" = "NH"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$30" = "NJ"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$31" = "NM"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$32" = "NY"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$33" = "NC"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$34" = "ND"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$35" = "OH"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$36" = "OK"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$37" = "OR"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$38" = "PA"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$39" = "RI"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$40" = "SC"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$41" = "SD"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$42" = "TN"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$43" = "TX"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$44" = "UT"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$45" = "VT"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$46" = "VA"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$47" = "WA"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$48" = "WV"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$49" = "WI"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$50" = "WY"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$51" = "AS"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$52" = "GU"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$53" = "MP"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$54" = "PR"
    ,"dnn$ctr31026$StateNetDB$ckBxStates$55" = "VI"
    ,"dnn$ctr31026$StateNetDB$ddlYear" = sprintf("%i", year)
)
  
  (search <- search %>% html_form_set(!!!params))
  
  # retrieve html
  resp <- read_html(html_form_submit(search,submit = "dnn$ctr31026$StateNetDB$btnSearch"), config = config(ssl_verifypeer = FALSE))
  
  # html_text
  html_text_output <- resp %>% html_elements("#dnn_ctr31026_StateNetDB_linkList") %>% html_text2()
  
  # links to bill text
  bill_text <- resp %>%
    html_nodes("#dnn_ctr31026_StateNetDB_linkList a") %>%
    { data.frame(Text = html_text(.), Link = html_attr(., 'href')) } %>%
    mutate(
      ID = str_squish(Text) %>% str_trim(),
      YEAR = year)
  
  return(list(html_text_output = html_text_output, bill_text = bill_text))
}

build_ncsl_bill_database <- function(){
  # Loop through each year's scraped html
  ncsl_bill_database <- data.frame()
  bill_links <- data.frame()
  for (year in (2025:year(Sys.Date()))) {
    print(year)
    
    scrape_results <- scrape_ncsl(year)
    text <- scrape_results$html_text_output
    bill_links <- rbind(bill_links, scrape_results$bill_text)
    
    if(!is_empty(text)){
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
        # Check if two bills caught
        if (length(which(curr == "History: Click for History")) != 1) {
          split_indicies = which(str_detect(curr, "^[A-Z]{2}[:space:][A-Z]{1,4}[:space:][0-9]+"))
          # If we catch multiple bills, split and extract bill info for each
          if (length(split_indicies) > 1) {
            split <- splitAt(curr, split_indicies)
            for (curr_split in split) {
              ncsl_bill_database <- rbind(ncsl_bill_database, ncsl_extract_bill_info(curr_split, year))
            }
          }
        } else{
          ncsl_bill_database <- rbind(ncsl_bill_database, ncsl_extract_bill_info(curr, year))
        }
      }
    } else{
      print("no results for this year")
    }
  }
  
  # Extract state
  ncsl_bill_database$STATE <- str_sub(ncsl_bill_database$ID,1,2)
  # Extract bill num
  ncsl_bill_database$BILLNUM <- str_remove_all(str_sub(ncsl_bill_database$ID,4)," ")
  # Extract result
  ncsl_bill_database$BILLSTATUS <- str_squish(str_split_fixed(ncsl_bill_database$STATUS,"-",2)[,1])
  ncsl_bill_database$BILLSTATUS <- fct_recode(as.factor(ncsl_bill_database$BILLSTATUS),
                                              "To Executive" = "To Governor",
                                              "To Executive" = "To Mayor",
                                              "Enacted" = "Adopted")
  # Add UUID to match other dataset
  ncsl_bill_database$UUID <- sprintf("%s%i%s",ncsl_bill_database$STATE, ncsl_bill_database$YEAR, ncsl_bill_database$BILLNUM )
  
  # Extract author name and party
  ncsl_bill_database$AUTHORNAME <- ifelse(str_detect(ncsl_bill_database$AUTHOR,"\\([A-Z]{1,3}\\)"),
                                          trimws(str_remove_all(ncsl_bill_database$AUTHOR,"\\([A-Z]{1,3}\\)"),"both"),
                                          ncsl_bill_database$AUTHOR)
  ncsl_bill_database$AUTHORPARTY <- str_remove_all(str_extract(ncsl_bill_database$AUTHOR,"\\([A-Z]{1,3}\\)"),"[()]")
  # Count cosponsors
  ncsl_bill_database$COAUTHORS[ncsl_bill_database$COAUTHORS=="\"NA\""] = NA
  ncsl_bill_database$NCOAUTHORS = sapply(ncsl_bill_database$COAUTHORS,ncsl_count_coauthors)
  ncsl_bill_database$NDEMCOAUTHORS = sapply(ncsl_bill_database$COAUTHORS,ncsl_count_dem_coauthors)
  ncsl_bill_database$NREPCOAUTHORS = sapply(ncsl_bill_database$COAUTHORS,ncsl_count_rep_coauthors)
  ncsl_bill_database <- ncsl_bill_database %>% mutate_at(vars(NCOAUTHORS,NDEMCOAUTHORS,NREPCOAUTHORS),replace_na, 0)
  
  # Categorize wide
  ncsl_bill_database$AVAPPL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting - Application and Request for")
  ncsl_bill_database$AVBDIS = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting - Delivering Ballots")
  ncsl_bill_database$AVBRET = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting - Ballot Processing, Signature Verification, Ballot Curing")
  ncsl_bill_database$AVEVIP = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Early In-Person Voting/In-Person Absentee")
  ncsl_bill_database$AVELIG = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting - Eligibility and No-Excuse Absentee Voting")
  ncsl_bill_database$AVMIOV = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Voters - Military and Overseas Voters")
  ncsl_bill_database$AVMISC = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting-Misc.")
  ncsl_bill_database$AVMOVE = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting-MOVE Act")
  ncsl_bill_database$AVNOEX = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting - Eligibility and No-Excuse Absentee Voting")
  ncsl_bill_database$AVPERM = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting-Permanent Status")
  ncsl_bill_database$AVBRET = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Absentee Voting - Returning Ballots")
  ncsl_bill_database$VOTEME = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Alternative  Voting Methods (Ranked Choice, etc.)")
  ncsl_bill_database$AUDITS = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Post-Election Audits")
  ncsl_bill_database$BACAND = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Ballot Access for Candidates")
  ncsl_bill_database$BAPART = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Ballot Access-Parties")
  ncsl_bill_database$BALDES = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Ballots - Required Number, Format & Design")
  ncsl_bill_database$CANQUL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Candidates - Qualification and Running for Office, Candidate Withdrawal")
  ncsl_bill_database$CANRTR = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Candidates-Resign to Run")
  ncsl_bill_database$CANWDW = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Candidates - Qualification and Running for Office, Candidate Withdrawal")
  ncsl_bill_database$CANWRI = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Candidates-Write-in")
  ncsl_bill_database$VTRCHA = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Challenges to Voters")
  ncsl_bill_database$CNTEST = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Contests (Court Challenges)")
  ncsl_bill_database$ELCOST = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Costs and Funding for Elections")
  ncsl_bill_database$VCOUNT = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Counting Votes and Canvassing Procedures")
  ncsl_bill_database$CYBSEC = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Cybersecurity")
  ncsl_bill_database$ELDATE = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Dates of Elections and Election Holidays")
  ncsl_bill_database$PTDRES = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "DREs")
  ncsl_bill_database$CRIMES = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Crimes")
  ncsl_bill_database$DATART = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Data and Records - Collection/Retention of")
  ncsl_bill_database$EDHOLI = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Day Holiday")
  ncsl_bill_database$EOCAMP = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Officials-Campaign Activities")
  ncsl_bill_database$EOLOCA = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Officials - Local")
  ncsl_bill_database$EOSTWD = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Officials - Statewide")
  ncsl_bill_database$REPRES = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Reporting, Results and Certification")
  ncsl_bill_database$ELEING = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Electioneering and Campaigning")
  ncsl_bill_database$ELECOL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Electoral College")
  ncsl_bill_database$ECONPV = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Electoral College-National Popular Vote")
  ncsl_bill_database$EMEDIS = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Emergencies/Disasters")
  ncsl_bill_database$EXPOLL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Exit Polling")
  ncsl_bill_database$DUALFU = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Fusion/Dual-Party")
  ncsl_bill_database$INVOTE = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Internet/Electronic Delivery or Return of Ballots")
  ncsl_bill_database$MAILVO = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "All Mail Voting")
  ncsl_bill_database$MISCEL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Miscellaneous")
  ncsl_bill_database$FILING = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Offices-Method of Filling")
  ncsl_bill_database$POLPAR = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Political Parties")
  ncsl_bill_database$POLWAT = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Poll Watchers, Challengers, Election Observers")
  ncsl_bill_database$PWCOMP = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Poll Workers-Compensation")
  ncsl_bill_database$PWMISC = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Poll Workers")
  ncsl_bill_database$PWQUAL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Poll Workers-Selection/Qualifications of")
  ncsl_bill_database$PWTRAI = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Poll Workers-Training")
  ncsl_bill_database$PWYOTH = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Poll Workers-Youth")
  ncsl_bill_database$PPPROC = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Polling Places and Election Offices - Arrangements, Procedures and Security")
  ncsl_bill_database$PPACES = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Polling Places-Disabled Access")
  ncsl_bill_database$PPVHRS = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Polling Places and Vote Centers - Hours and Locations")
  ncsl_bill_database$PPLOCA = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Polling Places and Vote Centers - Hours and Locations")
  ncsl_bill_database$PPVCEN = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Polling Places and Vote Centers - Hours and Locations")
  ncsl_bill_database$PREDEF = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Precincts")
  ncsl_bill_database$PRIDAT = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Primaries - State Primary Dates, Runoffs, and Misc.")
  ncsl_bill_database$PRIMIS = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Primaries - State Primary Dates, Runoffs, and Misc.")
  ncsl_bill_database$PRIPUS = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Primaries - Presidential")
  ncsl_bill_database$PRIRNF = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Primaries - State Primary Dates, Runoffs, and Misc.")
  ncsl_bill_database$PRITYP = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Primaries - Types")
  ncsl_bill_database$PROVOT = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Provisional Ballots")
  ncsl_bill_database$RECOUN = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Recounts")
  ncsl_bill_database$REGDRI = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration Drives")
  ncsl_bill_database$REGAPP = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration - Application Form/Content and Eligibility/ID Required ")
  ncsl_bill_database$REGATO = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration - Automatic")
  ncsl_bill_database$REGCVL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration-Statewide Voter Registration Databases")
  ncsl_bill_database$REGDTE = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration - Deadlines")
  ncsl_bill_database$REGEDY = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration - Election Day or Same Day")
  ncsl_bill_database$REGELE = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration-Electronic")
  ncsl_bill_database$REGIDR = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration-Eligibility ID Required")
  ncsl_bill_database$REGLST = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration - List Maintenance")
  ncsl_bill_database$REGMSC = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration-Misc.")
  ncsl_bill_database$REGONL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration - Online")
  ncsl_bill_database$REGPRE = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration - Preregistration for 16- and 17-year-olds")
  ncsl_bill_database$REGSDL = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Registration - Sale/Distribution/Use of Lists")
  ncsl_bill_database$RUNOFF = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Run-Off Elections")
  ncsl_bill_database$SPELEC = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Special Elections")
  ncsl_bill_database$STVOTE = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Straight Ticket Voting")
  ncsl_bill_database$TFSCIC = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Task Forces/Study Commissions/Interim Committees")
  ncsl_bill_database$VACNCY = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Vacancies")
  ncsl_bill_database$VEDINF = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Voter Education/Information")
  ncsl_bill_database$VOTRID = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Voter Identification")
  ncsl_bill_database$VOTAFW = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Voters-Absence from Work")
  ncsl_bill_database$VOTAGE = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Voters-Age")
  ncsl_bill_database$VOTAST = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Voters-Assistance to")
  ncsl_bill_database$VOTFVR = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Voters - Incarceration and Restoration of Voting Rights")
  ncsl_bill_database$VOTMQU = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Voters - Eligibility and Citizenship")
  ncsl_bill_database$TECHSS = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Technology - Selection & Standards, Security, Storage and Testing")
  ncsl_bill_database$VSSCST = sapply(ncsl_bill_database$TOPICS, ncsl_check_topics, "Election Technology - Selection & Standards, Security, Storage and Testing")
  
  # Better way if creating general topic cols
  general_columns <- ncsl_bill_database %>%
    group_by(UUID) %>%
    mutate(EOGENR = max(EOCAMP,EOLOCA,EOSTWD),
           PPGENR = max(PPPROC,PPACES,PPVHRS,PPVCEN),
           REGGEN = max(REGAPP,REGATO,REGDRI,REGDTE,REGEDY,REGELE,REGIDR,REGMSC,REGPRE)) %>%
    ungroup() %>%
    select(UUID,EOGENR,PPGENR,REGGEN) %>%
    distinct()
  
  ncsl_bill_database <- ncsl_bill_database %>% left_join(general_columns,by="UUID")
  
  # Get columns for bill topics - helps sort these cols alphabetically 
  topic_cols = sort(colnames(ncsl_bill_database)[21:113])
  
  # Add urls
  ncsl_bill_database <- ncsl_bill_database |> left_join(bill_links, by = c("YEAR","ID")) |>
    select(-Text) |>
    rename(BILLTEXTURL = Link)
  
  # Produce final output
  ncsl_bill_database <- ncsl_bill_database %>%
    select(UUID
           ,YEAR
           ,STATE
           ,BILLNUM
           ,BILLSTATUS
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
           ,BILLTEXTURL) %>%
    mutate(STATE = as.factor(STATE)
           ,AUTHORPARTY = as.factor(AUTHORPARTY)
           ,PREFILEDATE = as.Date(PREFILEDATE,format = "%m/%d/%Y")
           ,INTRODUCEDDATE = mdy(INTRODUCEDDATE)
           ,LASTACTIONDATE = mdy(LASTACTIONDATE))
  
  return(ncsl_bill_database)
}

ncsl_bill_database <- build_ncsl_bill_database()
# Save outputs
write.csv(ncsl_bill_database, file = "output/ncsl_bill_database.csv",row.names = FALSE)
save(ncsl_bill_database, file = "output/ncsl_bill_database.Rdata")
