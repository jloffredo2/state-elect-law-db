library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(rvest)
library(stringr)
library(magrittr)
library(purrr)

source("code/misc_fxns.R")

vrl_check_topics <- function(categories,topic){
  return(ifelse(sum(str_detect(categories,topic))>0,1,0))
}

scrape_billtrack_data <- function(link){
  print(link)
  author_list <- head(lapply(gsub(
    "[\r\n]", "",
    read_html(link) %>%
      html_element('div:nth-child(9)') %>%
      html_text() %>%
      str_trim("both")
  ) %>% str_split(","),str_trim, "both")[[1]], - 1)
  
  INTRODUCEDDATE <- mdy(
    read_html(link) %>%
      html_element('.current:nth-child(1)') %>%
      html_text() %>%
      str_trim("both") %>%
      str_remove("Introduced\r\n")
  )
  
  HISTORY <- rjson::toJSON(
    as.list(
      read_html(link) %>%
        html_element('#actions .card-body') %>%
        html_table() %>%
        mutate(
          text = sprintf('%s - %s', Date, Action)
          ,Step = row_number()
          ,Date = mdy(Date)
        ) %>%
        arrange(Date, desc(Step)) %>%
        select(text) %>%
        filter(!(
          str_detect(text, "COSPONSOR") | str_detect(text, "Hearing") | str_detect(text,"DP: ")
        ))
    )$text
  )
  
  AUTHOR <- author_list[unlist(lapply(author_list, str_detect, "\\*"))][1]
  COAUTHORS <- author_list[!author_list %in% AUTHOR]
  
  NCOAUTHORS <- length(COAUTHORS)
  NDEMCOAUTHORS <- sum(str_detect(COAUTHORS, "\\(D\\)")) + sum(str_detect(COAUTHORS, "\\(DFL\\)"))
  NREPCOAUTHORS <- sum(str_detect(COAUTHORS, "\\(R\\)"))
  
  COAUTHORS = ifelse(str_detect(COAUTHORS,"\\([A-Z]{1,3}\\)"),
                     str_trim(str_remove_all(COAUTHORS,"\\([A-Z]{1,3}\\)"),"both"),
                     str_trim(COAUTHORS,"both"))
  
  return(
    data.frame(
      bill_track_link = link,
      AUTHORNAME = AUTHOR,
      COAUTHORS = ifelse(is_empty(COAUTHORS),NA,rjson::toJSON(COAUTHORS)),
      NCOAUTHORS = NCOAUTHORS,
      NDEMCOAUTHORS = NDEMCOAUTHORS,
      NREPCOAUTHORS = NREPCOAUTHORS,
      INTRODUCEDDATE = INTRODUCEDDATE,
      HISTORY = HISTORY
    )
  )
}

build_ballotpedia_bill_database <- function(){
  ballotpedia_initial = read.csv("output/ballotpedia_initial.csv")
  sponsor_data = lapply(unique(ballotpedia_initial$bill_track_link), scrape_billtrack_data)
  
  ballotpedia_scraped <- bind_rows(
    map(sponsor_data, function(x){merge(ballotpedia_initial,x,by="bill_track_link")}),
    .id = 'id'
  )

  colnames(ballotpedia_scraped) <- str_to_upper(colnames(ballotpedia_scraped))
  
  ballotpedia_bill_database <- ballotpedia_scraped %>%
    rename(YEAR = SESSION_YEAR
           ,BILLSTATUS = CURRENT_LEGISLATIVE_STATUS
           ,BILLNUM = BILL_NUMBER
           ,LASTACTIONDATE = ACTION_DATE
           ,BILLSUMMARY = SUMMARY
           ,BILLTEXTURL = BILL_TRACK_LINK
           ,AUTHORPARTY = SPONSORS_PARTISAN_AFFILIATIONS) %>%
    mutate(LASTACTIONDATE = ymd(LASTACTIONDATE)
           ,CATEGORIES = str_split(CATEGORIES, ",")
           ,BILLNUM = str_replace_all(BILLNUM,c("HB"='H','SB'='S','LB'='L'))
           ,UUID = str_c(STATE,YEAR,BILLNUM)
           ,AUTHORPARTY = ifelse(str_detect(AUTHORNAME,"\\([A-Z]{1,3}\\)\\*"),
                                  str_remove_all(str_extract(AUTHORNAME,"\\([A-Z]{1,3}\\)"),"[()]"),
                                  AUTHORPARTY)
           ,AUTHORNAME = ifelse(str_detect(AUTHORNAME,"\\([A-Z]{1,3}\\)\\*"),
                               str_trim(str_remove_all(AUTHORNAME,"\\([A-Z]{1,3}\\)\\*"),"both"),
                               str_trim(AUTHORNAME,"both"))
           ,BILLSTATUS = fct_recode(as.factor(BILLSTATUS),
                                    "Failed" = "Dead"
                                    ,"Pending" = "Passed both chambers"
                                    ,"Pending" = "Introduced"
                                    ,"Pending" = "Passed one chamber"
                                    ,"Pending" = "Advanced from committee"
                                    ,"Enacted" = "Enacted"))
  # CATEGORIZE WIDE
  ballotpedia_bill_database$AUDITS = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Audits")                                      
  ballotpedia_bill_database$AVAPPL = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Absentee/mail-in ballot request processes") 
  ballotpedia_bill_database$AVBRET = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Absentee/mail-in ballot return processes")
  ballotpedia_bill_database$AVDLIN = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Absentee/mail-in ballot deadlines" )
  ballotpedia_bill_database$AVELIG = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Absentee/mail-in voting eligibility")
  ballotpedia_bill_database$AVEVIP = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Early voting administration")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Early voting duration")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Early voting eligibility"))                    
  ballotpedia_bill_database$AVMIOV = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Military/UOCAVA voting")                      
  ballotpedia_bill_database$AVMISC = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Absentee/mail-in ballot administration")
  ballotpedia_bill_database$AVPERM = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Permanent lists")                             
  ballotpedia_bill_database$BACAND = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Ballot access for candidates")                
  ballotpedia_bill_database$BACHOC = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Chain of custody")                            
  ballotpedia_bill_database$BACURE = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Ballot collection")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Cure provisions")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Signature matching")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Witness or notary requirements"))              
  ballotpedia_bill_database$BALDES = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Ballot design")                               
  ballotpedia_bill_database$BAMEAS = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Ballot measures (initiative and referendum)") 
  ballotpedia_bill_database$BAPART = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Ballot access for parties")                   
  ballotpedia_bill_database$CRIMES = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Enforcement against non-officials")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Enforcement against officials"))
  ballotpedia_bill_database$CYBSEC = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Cybersecurity")                               
  ballotpedia_bill_database$ECONPV = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "National Popular Vote Interstate Compact")    
  ballotpedia_bill_database$ELAUTH = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Legislative authority")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Oversight and accountability"))                
  ballotpedia_bill_database$ELDATE = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Election dates and administrative deadlines") 
  ballotpedia_bill_database$ELEING = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Electioneering")                              
  ballotpedia_bill_database$EMEDIS = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "States of emergency")                         
  ballotpedia_bill_database$EOGENR = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Election officials")                          
  ballotpedia_bill_database$EPLOCL = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Municipal election procedures")               
  ballotpedia_bill_database$FUNDNG = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Federal funding")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Private funding")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "State appropriations"))                        
  ballotpedia_bill_database$DGVOTE = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Digital/electronic voting")                   
  ballotpedia_bill_database$INVOTE = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Online voting")                               
  ballotpedia_bill_database$MAILVO = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Automatic mail-in ballots")                   
  ballotpedia_bill_database$NONCTV = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Noncitizen voting")
  ballotpedia_bill_database$PHYSEC = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Drop-box security")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Physical security")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Video surveillance")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Drop-box security"))                          
  ballotpedia_bill_database$POLWAT = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Poll observers")                              
  ballotpedia_bill_database$PPGENR = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "In-person voting and polling places")         
  ballotpedia_bill_database$PRITYP = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Non-traditional primaries")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Primary systems"))                             
  ballotpedia_bill_database$PROVOT = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Provisional ballots")                         
  ballotpedia_bill_database$PWMISC = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Election workers/volunteers")                 
  ballotpedia_bill_database$RECALL = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Recalls")                                     
  ballotpedia_bill_database$RECOUN = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Recounts")                                    
  ballotpedia_bill_database$REDIST = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Redistricting commissions")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Redistricting criteria/requirements")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Redistricting funding")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Redistricting process")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Redistricting"))                               
  ballotpedia_bill_database$REGATO = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Automatic voter registration")                
  ballotpedia_bill_database$REGDTE = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Voter participation deadlines")               
  ballotpedia_bill_database$REGEDY = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Same-day/Election Day registration")          
  ballotpedia_bill_database$REGGEN = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Voter registration")                          
  ballotpedia_bill_database$REGLST = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Voter list maintenance")                      
  ballotpedia_bill_database$REPRES = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Certification")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Reporting"))                                   
  ballotpedia_bill_database$TECHSS = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Voting equipment")                            
  ballotpedia_bill_database$VACNCY = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Vacancy procedures")                          
  ballotpedia_bill_database$VCOUNT = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Counting and canvassing procedures")          
  ballotpedia_bill_database$VOTAGE = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Youth voting")  
  ballotpedia_bill_database$VOTAST = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Absentee/mail-in voter assistance")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "In-person voter assistance"))                  
  ballotpedia_bill_database$LNGACC = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Language accommodations")                     
  ballotpedia_bill_database$VOTEME = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Ranked-choice voting")                        
  ballotpedia_bill_database$VOTFVR = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Voting by felons or incarcerated individuals")
  ballotpedia_bill_database$VOTRID = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "In-person voter ID")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Voter ID for absentee/mail-in ballots")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Voter ID for low-income voters")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Voter ID for registration"))                   
  ballotpedia_bill_database$VTDROP = max(sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Drop-box availability")
                                         ,sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Drop-box security"))
  ballotpedia_bill_database$VTRCHA = sapply(ballotpedia_bill_database$CATEGORIES, vrl_check_topics, "Challenges")
  
  topic_cols = sort(colnames(ballotpedia_bill_database)[23:77])
  
  ballotpedia_bill_database <- ballotpedia_bill_database %>%
    select(UUID
           ,YEAR
           ,STATE
           ,BILLNUM
           ,BILLSTATUS
           ,AUTHORNAME
           ,AUTHORPARTY
           ,INTRODUCEDDATE
           ,LASTACTIONDATE
           ,NCOAUTHORS
           ,NDEMCOAUTHORS
           ,NREPCOAUTHORS
           ,all_of(topic_cols)
           ,COAUTHORS
           ,HISTORY
           ,BILLTEXTURL
           ,BILLSUMMARY) %>%
    mutate(
      STATE = as.factor(STATE)
      ,AUTHORPARTY = as.factor(AUTHORPARTY))

  return(ballotpedia_scraped)
}

ballotpedia_bill_database <- build_ballotpedia_bill_database()


write.csv(ballotpedia_bill_database, file = "output/ballotpedia_bill_database.csv",row.names = FALSE)
save(ballotpedia_bill_database, file = "output/ballotpedia_bill_database.Rdata")