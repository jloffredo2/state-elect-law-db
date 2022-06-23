library(tidyverse)
library(rvest)
library(DescTools)

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
  
  split_text <- str_split(html_text, "\n")
  for (s in 1:length(split_text)) {
    curr = split_text[[s]]
    (curr = trimws(curr[curr != "" &
                          curr != sprintf(" %i", year) &
                          !str_detect(curr, "Associated Bills:") &
                          curr != "Bill Text Lookup" & !(curr %in% state.name)], "both"))
    # Add a check if two bills caught
    if (length(which(curr == "History: Click for History")) != 1) {
      # need to split curr, add second to next element in list
      split_indicies = which(str_detect(curr, "[A-Z]{2}[:space:][A-Z]{1,3}[:space:][0-9]+"))
      if (length(split_indicies) > 1) {
        split <- splitAt(curr, split_indicies)
        for (s in split) {
          bill_database <- rbind(bill_database, extract_bill_info(s, year))
        }
      }
    } else{
      bill_database <- rbind(bill_database, extract_bill_info(curr, year))
    }
  }
}
