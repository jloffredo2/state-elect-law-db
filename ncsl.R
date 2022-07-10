## FUNCTIONS FOR SCRAPING NCSL

# Function for setting values for topic binary indicators
ncsl_check_topics <- function(json,topic){
  return(ifelse(sum(str_detect(fromJSON(json),topic))>0,1,0))
}

# Function to count number of Dem coauthors (includes DFL in MN)
ncsl_count_dem_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(fromJSON(json),"\\(D\\)")) + sum(str_detect(fromJSON(json),"\\(DFL\\)")),NA))
}

# Function to count number of Rep coauthors
ncsl_count_rep_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(fromJSON(json),"\\(R\\)")),NA))
}

# Function to count total number of coauthors
ncsl_count_coauthors <- function(json){
  return(ifelse(!is.na(json),length(fromJSON(json)),NA))
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

scrape_ncsl <- function(year){
  print("scraping webform")
  URL <- "https://www.ncsl.org/research/elections-and-campaigns/elections-legislation-database.aspx"
  search <- html_form(read_html(URL))[[1]]
  
  params = list("dnn$ctr71252$StateNetDB$ckBxAllTopics"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$0"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$1"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$2"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$3"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$4"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$5"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$6"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$7"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$8"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$9"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$10"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$11"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$12"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$13"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$14"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$15"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$16"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$17"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$18"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$19"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$20"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$21"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$22"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$23"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$24"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$25"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$26"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$27"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$28"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$29"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$30"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$31"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$32"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$33"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$34"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$35"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$36"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$37"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$38"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$39"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$40"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$41"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$42"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$43"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$44"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$45"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$46"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$47"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$48"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$49"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$50"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$51"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$52"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$53"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$54"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$55"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$56"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$57"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$58"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$59"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$60"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$61"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$62"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$63"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$64"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$65"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$66"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$67"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$68"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$69"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$70"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$71"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$72"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$73"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$74"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$75"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$76"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$77"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$78"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$79"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$80"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$81"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$82"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$83"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$84"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$85"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$86"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$87"="on"
                , "dnn$ctr71252$StateNetDB$ckBxTopics$88"="on"
                , "dnn$ctr71252$StateNetDB$ckBxAllStates=on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$0"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$1"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$2"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$3"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$4"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$5"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$6"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$7"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$8"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$9"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$10"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$11"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$12"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$13"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$14"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$15"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$16"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$17"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$18"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$19"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$20"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$21"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$22"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$23"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$24"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$25"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$26"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$27"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$28"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$29"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$30"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$31"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$32"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$33"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$34"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$35"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$36"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$37"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$38"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$39"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$40"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$41"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$42"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$43"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$44"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$45"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$46"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$47"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$48"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$49"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$50"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$51"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$52"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$53"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$54"="on"
                , "dnn$ctr71252$StateNetDB$ckBxStates$55"="on"
                , "dnn$ctr71252$StateNetDB$ddlYear"=sprintf("%i", year))
  
  (search <- search %>% html_form_set(!!!params))
  
  resp <- read_html(html_form_submit(search,submit = "dnn$ctr71252$StateNetDB$btnSearch"))%>%
    html_elements("#dnn_ctr71252_StateNetDB_linkList") %>%
    html_text2()
  
  return(resp)
}




