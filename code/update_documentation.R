library(lubridate)

# Edit codebook and readme
readme = readLines("README.md")
readme[1] = sprintf("# STATE ELECTIONS LEGISLATION DATABASE (2011-%i)",year(today()))
readme[6] = sprintf("**Date**: %s", today())
readme[13] = sprintf("> National Conference of State Legislatures. 2024. *State Elections Legislation Database*. www.ncsl.org/research/elections-and-campaigns/elections-legislation-database.aspx (%s).",(stamp("March 1, 1999")(today())))
readme[15] = sprintf("> Voting Rights Lab. 2024. *Legislative Tracker*. https://tracker.votingrightslab.org/pending/search (%s).",(stamp("March 1, 1999")(today())))
writeLines(readme, "README.md")
