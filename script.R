library(tidyverse)
library(rvest)

test <- read_html("input/ncsl/michigan.html")

text <- test %>%
  html_elements("#dnn_ctr71252_StateNetDB_linkList") %>%
  html_text2()

text = gsub(pattern = "[ ]+", replacement = " ", text)
thetext = strsplit(text, split= "\n\n|\n")[[1]]
(html_text = thetext[thetext != "2021" & thetext != "Bill Text Lookup" & !(thetext %in% state.name)])

break_points = which(html_text=="")

splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos),drop=T))

split_text <- splitAt(html_text, break_points)

for(s in length(split_text)){
  curr = split_text[[s]]
  (curr = curr[curr!=""])
  (bill_id = curr[1])
  (status = curr[grepl(pattern = "^Status:", curr)] %>%
      gsub("Status:", "", x = .) %>% str_trim)
  (date_last_act = curr[grepl(pattern = "^Date of Last Action:\\*", curr)] %>%
      gsub("Date of Last Action:\\*", " ", x = .) %>% str_trim)
  (authors = curr[grepl(pattern = "^Author:", curr)] %>% str_trim %>% str_split("Additional Authors:") %>% .[[1]])
  (author = authors[1] %>% gsub("Author:", "", x = .) %>% str_trim)
  (coauthors = ifelse(length(authors)>1, (authors[2] %>% str_split(";")),NA))[[1]]
}
