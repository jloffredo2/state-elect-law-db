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