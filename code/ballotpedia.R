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
  
  AUTHOR <-
    author_list[unlist(lapply(author_list, str_detect, "\\*"))]
  COAUTHORS <- author_list[!author_list %in% authors]
  
  NCOAUTHORS <- length(cosponsors)
  NDEMCOAUTHORS <- sum(str_detect(cosponsors, "\\(D\\)")) + sum(str_detect(cosponsors, "\\(DFL\\)"))
  NREPCOAUTHORS <- sum(str_detect(cosponsors, "\\(R\\)"))
  
  return(
    data.frame(
      bill_track_link = link,
      AUTHORNAME = rjson::toJSON(AUTHOR),
      COAUTHORS = rjson::toJSON(COAUTHORS),
      NCOAUTHORS = NCOAUTHORS,
      NDEMCOAUTHORS = NDEMCOAUTHORS,
      NREPCOAUTHORS = NREPCOAUTHORS,
      INTRODUCEDDATE = INTRODUCEDDATE
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
  
  #save(ballotpedia_scraped, file = "output/ballotpedia_scraped.RData")
  
  load("output/ballotpedia_scraped.RData")
  colnames(ballotpedia_scraped) <- str_to_upper(colnames(ballotpedia_scraped))
  ballotpedia_bill_database <- ballotpedia_scraped %>%
    rename(YEAR = SESSION_YEAR
           ,BILLSTATUS = CURRENT_LEGISLATIVE_STATUS
           ,BILLNUM = BILL_NUMBER
           ,LASTACTIONDATE = ACTION_DATE
           ,BILLSUMMARY = SUMMARY
           ,BILLTEXTURL = BILL_TRACK_LINK) %>%
    mutate(LASTACTIONDATE = ymd(LASTACTIONDATE)
           ,CATEGORIES = str_split(CATEGORIES, ",")
           ,BILLNUM = str_replace_all(BILLNUM,c("HB"='H','SB'='S'))
           ,UUID = str_c(STATE,YEAR,BILLNUM))
    
  return(ballotpedia_scraped)
}