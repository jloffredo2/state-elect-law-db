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
  ballotpedia_initial = head(read.csv("output/ballotpedia_initial.csv"),45)
  sponsor_data = lapply(unique(ballotpedia_initial$bill_track_link), scrape_billtrack_data)
  
  ballotpedia_scraped <- bind_rows(
    map(sponsor_data, function(x){merge(ballotpedia_initial,x,by="bill_track_link")}),
    .id = 'id'
  )
  
  #save(ballotpedia_scraped, file = "output/ballotpedia_scraped.RData")
  
  #load("output/ballotpedia_scraped.RData")
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
           ,BILLNUM = str_replace_all(BILLNUM,c("HB"='H','SB'='S'))
           ,UUID = str_c(STATE,YEAR,BILLNUM)
           ,AUTHORPARTY = ifelse(AUTHORPARTY == 'Bipartisan',
                                  str_remove_all(str_extract(AUTHORNAME,"\\([A-Z]{1,3}\\)"),"[()]"),
                                  AUTHORPARTY)
           ,AUTHORNAME = ifelse(str_detect(AUTHORNAME,"\\([A-Z]{1,3}\\)\\*"),
                               str_trim(str_remove_all(AUTHORNAME,"\\([A-Z]{1,3}\\)\\*"),"both"),
                               str_trim(AUTHORNAME,"both")))
  
  file.remove("output/ballotpedia_initial.csv")  
  return(ballotpedia_scraped)
}