# Function for splitting strings
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
# fct_recode without the unknown-level warning
recode_levels <- function(x, ...) {
  x <- as.factor(x)
  map <- c(...)
  map <- map[map %in% levels(x)]
  if (length(map) == 0) return(x)
  forcats::fct_recode(x, !!!as.list(map))
}
