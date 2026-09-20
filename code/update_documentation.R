library(lubridate)
library(stringr)

readme <- readLines("README.md")
long_date <- format(today(), "%B %d, %Y")
readme <- readme |>
  str_replace("^# STATE ELECTIONS LEGISLATION DATABASE \\(2011-\\d{4}\\)",
              sprintf("# STATE ELECTIONS LEGISLATION DATABASE (2011-%i)", year(today()))) |>
  str_replace("^\\*\\*Date\\*\\*: .*", sprintf("**Date**: %s", today())) |>
  str_replace("^(> National Conference of State Legislatures\\. .*)\\([A-Za-z]+ \\d{1,2}, \\d{4}\\)\\.$",
              sprintf("\\1(%s).", long_date)) |>
  str_replace("^(> Voting Rights Lab\\. .*)\\([A-Za-z]+ \\d{1,2}, \\d{4}\\)\\.$",
              sprintf("\\1(%s).", long_date))
writeLines(readme, "README.md")
