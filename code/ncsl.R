library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(rvest)
library(stringr)
library(purrr)

source("code/misc_fxns.R")
## FUNCTIONS FOR SCRAPING NCSL

ncsl_count_coauthors <- function(json){
  if (is.na(json)) return(c(NCOAUTHORS = 0, NDEMCOAUTHORS = 0, NREPCOAUTHORS = 0))
  v <- rjson::fromJSON(json)
  c(NCOAUTHORS = length(v),
    NDEMCOAUTHORS = sum(str_detect(v, "\\((D|DFL)\\)")),
    NREPCOAUTHORS = sum(str_detect(v, "\\(R\\)")))
}

# NCSL topic label -> column; shared labels keep the schema aligned with the 2011-2024 file
ncsl_topic_map <- c(
  AVAPPL = "Absentee Voting - Application and Request for"
  ,AVBDIS = "Absentee Voting - Delivering Ballots"
  ,AVBRET = "Absentee Voting - Returning Ballots"
  ,BACURE = "Absentee Voting - Ballot Processing, Signature Verification, Ballot Curing"
  ,AVEVIP = "Early In-Person Voting/In-Person Absentee"
  ,AVELIG = "Absentee Voting - Eligibility and No-Excuse Absentee Voting"
  ,AVMIOV = "Voters - Military and Overseas Voters"
  ,AVMISC = "Absentee Voting-Misc."
  ,AVMOVE = "Absentee Voting-MOVE Act"
  ,AVNOEX = "Absentee Voting - Eligibility and No-Excuse Absentee Voting"
  ,AVPERM = "Absentee Voting-Permanent Status"
  ,VOTEME = "Alternative Voting Methods (Ranked Choice, etc.)"
  ,AUDITS = "Post-Election Audits"
  ,BACAND = "Ballot Access for Candidates"
  ,BAPART = "Ballot Access-Parties"
  ,BALDES = "Ballots - Required Number, Format & Design"
  ,CANQUL = "Candidates - Qualification and Running for Office, Candidate Withdrawal"
  ,CANRTR = "Candidates-Resign to Run"
  ,CANWDW = "Candidates - Qualification and Running for Office, Candidate Withdrawal"
  ,CANWRI = "Candidates-Write-in"
  ,VTRCHA = "Challenges to Voters"
  ,CNTEST = "Election Contests (Court Challenges)"
  ,ELCOST = "Costs and Funding for Elections"
  ,VCOUNT = "Counting Votes and Canvassing Procedures"
  ,CYBSEC = "Cybersecurity"
  ,ELDATE = "Dates of Elections and Election Holidays"
  ,PTDRES = "DREs"
  ,CRIMES = "Election Crimes"
  ,DATART = "Election Data and Records - Collection/Retention of"
  ,EDHOLI = "Election Day Holiday"
  ,EOCAMP = "Election Officials-Campaign Activities"
  ,EOLOCA = "Election Officials - Local"
  ,EOSTWD = "Election Officials - Statewide"
  ,REPRES = "Election Reporting, Results and Certification"
  ,ELEING = "Electioneering and Campaigning"
  ,ELECOL = "Electoral College"
  ,ECONPV = "Electoral College-National Popular Vote"
  ,EMEDIS = "Emergencies/Disasters"
  ,EXPOLL = "Exit Polling"
  ,DUALFU = "Fusion/Dual-Party"
  ,INVOTE = "Internet/Electronic Delivery or Return of Ballots"
  ,MAILVO = "All Mail Voting"
  ,MISCEL = "Miscellaneous"
  ,FILING = "Offices-Method of Filling"
  ,POLPAR = "Political Parties"
  ,POLWAT = "Poll Watchers, Challengers, Election Observers"
  ,PWCOMP = "Poll Workers-Compensation"
  ,PWMISC = "Poll Workers"
  ,PWQUAL = "Poll Workers-Selection/Qualifications of"
  ,PWTRAI = "Poll Workers-Training"
  ,PWYOTH = "Poll Workers-Youth"
  ,PPPROC = "Polling Places and Election Offices - Arrangements, Procedures and Security"
  ,PPACES = "Polling Places-Disabled Access"
  ,PPVHRS = "Polling Places and Vote Centers - Hours and Locations"
  ,PPLOCA = "Polling Places and Vote Centers - Hours and Locations"
  ,PPVCEN = "Polling Places and Vote Centers - Hours and Locations"
  ,PREDEF = "Precincts"
  ,PRIDAT = "Primaries - State Primary Dates, Runoffs, and Misc."
  ,PRIMIS = "Primaries - State Primary Dates, Runoffs, and Misc."
  ,PRIPUS = "Primaries - Presidential"
  ,PRIRNF = "Primaries - State Primary Dates, Runoffs, and Misc."
  ,PRITYP = "Primaries - Types"
  ,PROVOT = "Provisional Ballots"
  ,RECALL = "Recall Elections for State Officials"
  ,RECOUN = "Recounts"
  ,REGDRI = "Registration Drives"
  ,REGAPP = "Registration - Application Form/Content and Eligibility/ID Required"
  ,REGATO = "Registration - Automatic"
  ,REGCVL = "Registration-Statewide Voter Registration Databases"
  ,REGDTE = "Registration - Deadlines"
  ,REGEDY = "Registration - Election Day or Same Day"
  ,REGELE = "Registration-Electronic"
  ,REGIDR = "Registration-Eligibility ID Required"
  ,REGLST = "Registration - List Maintenance"
  ,REGMSC = "Registration-Misc."
  ,REGONL = "Registration - Online"
  ,REGPRE = "Registration - Preregistration for 16- and 17-year-olds"
  ,REGSDL = "Registration - Sale/Distribution/Use of Lists"
  ,RUNOFF = "Run-Off Elections"
  ,SPELEC = "Special Elections"
  ,STVOTE = "Straight Ticket Voting"
  ,TFSCIC = "Task Forces/Study Commissions/Interim Committees"
  ,VACNCY = "Vacancies"
  ,VEDINF = "Voter Education/Information"
  ,VOTRID = "Voter Identification"
  ,VOTAFW = "Voters-Absence from Work"
  ,VOTAGE = "Voters-Age"
  ,VOTAST = "Voters with Disabilities or Limited English"
  ,VOTFVR = "Voters - Incarceration and Restoration of Voting Rights"
  ,VOTMQU = "Voters - Eligibility and Citizenship"
  ,TECHSS = "Election Technology - Selection & Standards, Security, Storage and Testing"
  ,VSSCST = "Election Technology - Selection & Standards, Security, Storage and Testing"
)

# Match on the re-joined string: the scraper's ", " split also splits labels containing commas
ncsl_topic_dummies <- function(topics_json){
  topic_str <- vapply(topics_json, \(j) str_c(str_trim(rjson::fromJSON(j)), collapse = ", "), character(1), USE.NAMES = FALSE)
  out <- lapply(ncsl_topic_map, \(label) as.integer(str_detect(topic_str, fixed(label))))
  as.data.frame(out)
}

ncsl_extract_bill_info <- function(curr, year) {
  bill_id = curr[1]
  print(bill_id)
  status = curr[grepl(pattern = "^Status:", curr)] |> str_remove("Status:") |> str_trim()
  authors = (curr[grepl(pattern = "^Author:", curr)] |> str_trim() |> str_split("Additional Authors:"))[[1]]
  author = authors[1] |> str_remove("Author:") |> str_trim()
  coauthors = ifelse(length(authors) > 1, (authors[2] |> str_split(";")), NA)[[1]]
  coauthors = trimws(coauthors,"both")
  topics = (curr[grepl(pattern = "^Topics:", curr)] |> str_remove("Topics:") |> str_trim() |> str_split(", "))[[1]]
  summary = curr[grepl(pattern = "^Summary:", curr)] |> str_remove("Summary:") |> str_trim()
  history_index = which(str_detect(curr, "^History:")) + 1
  history = curr[history_index:length(curr)] |> str_trim()
  # Check intro versus prefile date
  prefiled_date = ifelse(sum(str_detect(history, "PREFILED")) > 0 ,
                         history[str_detect(history, "PREFILED") == TRUE] |> str_sub(1, 10),
                         NA)
  introduced_date = ifelse(sum(str_detect(history, "INTRODUCED")) > 0 ,
                           history[str_detect(history, "INTRODUCED") == TRUE] |> str_sub(1, 10),
                           NA)
  last_action_date = ifelse(str_detect(history[length(history)],".*[0-9].*"),history[length(history)] |> str_sub(1, 10),history[length(history)-1] |> str_sub(1, 10))
  
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
  
  (search <- search |> html_form_set(!!!params))
  
  # retrieve html
  resp <- read_html(html_form_submit(search,submit = "dnn$ctr31026$StateNetDB$btnSearch"), config = config(ssl_verifypeer = FALSE))
  
  # html_text
  html_text_output <- resp |> html_elements("#dnn_ctr31026_StateNetDB_linkList") |> html_text2()
  
  # links to bill text
  bill_text <- resp |>
    html_nodes("#dnn_ctr31026_StateNetDB_linkList a") |>
    (\(x) data.frame(Text = html_text(x), Link = html_attr(x, 'href')))() |>
    mutate(
      ID = str_squish(Text) |> str_trim(),
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
      (html_text = trimws(thetext[thetext != "\r"] |> str_remove_all("\r"), "both"))
      
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
  ncsl_bill_database$BILLSTATUS <- recode_levels(ncsl_bill_database$BILLSTATUS,
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
  coauthor_counts <- t(sapply(ncsl_bill_database$COAUTHORS, ncsl_count_coauthors, USE.NAMES = FALSE))
  ncsl_bill_database <- cbind(ncsl_bill_database, coauthor_counts)
  
  ncsl_bill_database <- cbind(ncsl_bill_database, ncsl_topic_dummies(ncsl_bill_database$TOPICS))
  
  ncsl_bill_database <- ncsl_bill_database |>
    mutate(EOGENR = pmax(EOCAMP, EOLOCA, EOSTWD),
           PPGENR = pmax(PPPROC, PPACES, PPVHRS, PPVCEN),
           REGGEN = pmax(REGAPP, REGATO, REGDRI, REGDTE, REGEDY, REGELE, REGIDR, REGMSC, REGPRE))
  
  topic_cols <- sort(c(names(ncsl_topic_map), "EOGENR", "PPGENR", "REGGEN"))
  
  # Add urls
  bill_links <- bill_links |> distinct(YEAR, ID, .keep_all = TRUE)  # repeated listings would multiply rows
  ncsl_bill_database <- ncsl_bill_database |>
    left_join(bill_links, by = c("YEAR","ID"), relationship = "many-to-one") |>
    select(-Text) |>
    rename(BILLTEXTURL = Link)
  
  # Produce final output
  ncsl_bill_database <- ncsl_bill_database |>
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
           ,BILLTEXTURL) |>
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
