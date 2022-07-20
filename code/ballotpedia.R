scrape_billtrack_sponsors <- function(link){
  print(link)
  author_list <- head(lapply(gsub(
    "[\r\n]", "",
    read_html(link) %>%
      html_element('div:nth-child(9)') %>%
      html_text() %>%
      str_trim("both")
  ) %>% str_split(","),str_trim, "both")[[1]], - 1)
  
  authors <- author_list[unlist(lapply(author_list,str_detect,"\\*"))]
  cosponsors <- author_list[!author_list %in% authors]
  
  NCOAUTHORS <- length(cosponsors)
  NDEMCOAUTHORS <- sum(str_detect(cosponsors,"\\(D\\)")) + sum(str_detect(cosponsors,"\\(DFL\\)"))
  NREPCOAUTHORS <- sum(str_detect(cosponsors,"\\(R\\)"))
  
  return(data.frame(bill_track_link=link,NCOAUTHORS=NCOAUTHORS,NDEMCOAUTHORS=NDEMCOAUTHORS,NREPCOAUTHORS=NREPCOAUTHORS))
}

build_ballotpedia_bill_database <- function(){
  ballotpedia_initial = read.csv("output/ballotpedia_initial.csv")
  sponsor_data = lapply(unique(ballotpedia_initial$bill_track_link), scrape_billtrack_sponsors)
  
  ballotpedia_scraped <- bind_rows(
    map(sponsor_data, function(x){merge(ballotpedia_initial,x,by="bill_track_link")}),
    .id = 'id'
  )
  
  save(ballotpedia_scraped, file = "output/ballotpedia_scraped.RData")
}