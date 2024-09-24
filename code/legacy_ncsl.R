library(tidyverse)
library(readxl)
library(janitor)

rm(list = ls())
gc()

# Load old NCSL list
legacy <- read_excel("legacy_ncsl/2001-2010 election admin leg db.xlsx")

# Clean up legacy
clean <- legacy |>
  clean_names() |>
  drop_na(id) |>
  mutate(
    description = gsub(
      "<[^>]+>", 
      "", 
      str_trim(description) |> 
        str_squish()),
    bill_no = str_replace_all(
      bill_no, 
      c("Sub. " = "", "\\([0-9]{5}\\)" = "")), 
    bill_no_cleaned = str_replace_all(bill_no,c(
      'AB' = 'A', 'AJRSPECIAL' = 'AJR', 'HB' = 'H', 'HCONRES' = 'HCR', 'HCSR' = 'HCR',
      'HD' = 'H', 'HF' = 'H', 'HJRES' = 'HJR', 'HP' = 'H', 'HRES' = 'HR', 
      'LB' = "L", 'LD' = 'H', 'PR' = "P", 'PC' = "PCB", 'SB' = 'S', 'SCONRES' = 'SCR',
      'SF' = 'S', 'SJ' = 'SJR', 'SJRE' = 'SJR', 'SJRES' = 'SJR', 'SP' = 'S',
      'SRES' = 'SR')) |> str_remove_all("\\s+"), 
    uuid = str_c(state_code, year, bill_no_cleaned),
    # Fixing category mismatch
    place_holder = case_when(
      name == 'Absentee Voting' ~ 'Absentee Voting',
      name == 'Absentee Voting-Returning Ballots' ~ 'Absentee Voting',
      name == 'HAVA Compliance' ~ 'HAVA Compliance',
      name == 'Primaries-Dates' ~ 'Primaries-Dates',
      name == 'Primaries-Types' ~ 'Primaries-Types',
      name == 'Registration-Election Day' ~ 'Registration-Election Day',
      name == 'Vacancies' ~ 'Vacancies',
      name == 'Voter ID-All Voters' ~ 'Voter ID-All Voters',
      TRUE ~ NA_character_
    ),
    name = case_when(
      place_holder == 'Absentee Voting' ~ 'Elections',
      place_holder == 'Absentee Voting-Returning Ballots' ~ 'Elections',
      place_holder == 'HAVA Compliance' ~ 'Elections',
      place_holder == 'Primaries-Dates' ~ 'Elections',
      place_holder == 'Primaries-Types' ~ 'Elections',
      place_holder == 'Registration-Election Day' ~ 'Elections',
      place_holder == 'Vacancies' ~ 'Elections',
      place_holder == 'Voter ID-All Voters' ~ 'Elections',
      TRUE ~ name
    ),
    subcategory = case_when(
      !is.na(place_holder) ~ place_holder,
      TRUE ~ subcategory
    ),
    session_law = ifelse(session_law == "n/a", NA_character_, session_law)
  ) |>
  select(ncsl_id = id, uuid, state = state_code, bill_no, year, session, status, statute = session_law, category = name, subcategory, description) |>
  filter(category %in% c('Campaign Finance','Elections','Ethics','Initiative & Referendum', 'Lobbying', 'Term Limits')) |>
  arrange(state, year, bill_no, ncsl_id)

# Nest category list
nested_category <- clean |>
  summarise(subcategory_list = paste(sort(subcategory), collapse = ", "), .by = c('ncsl_id', 'category')) |>
  # Step 2: Spread the categories into columns
  pivot_wider(names_from = category, values_from = subcategory_list)

# Nested version to give to NCSL and Charles
clean_nested_category <- clean |>
  select(-c(category, subcategory)) |>
  distinct() |>
  left_join(nested_category, by = 'ncsl_id') |>
  clean_names() |>
  rename(
    "topic_initiative_referendum"="initiative_referendum",
    "topic_term_limits"="term_limits",
    "topic_campaign_finance"="campaign_finance",
    "topic_lobbying"="lobbying",
    "topic_ethics"="ethics",
    "topic_elections"="elections"
  ) |>
  arrange(state, year, bill_no, ncsl_id)

# My version
sleg <- clean |>
  filter(category == 'Elections') |>
  mutate(
    my_label = case_when(
      subcategory == 'Absentee Voting-Application for' ~ 'AVAPPL',
      subcategory == 'Absentee Voting-Distributing Ballots' ~ 'AVBDIS',
      subcategory == 'Absentee Voting-Early Voting/In-Person Absentee' ~ 'AVEVIP',
      subcategory == 'Absentee Voting-Eligibility' ~ 'AVELIG',
      subcategory == 'Absentee Voting-Military/Overseas' ~ 'AVMIOV',
      subcategory == 'Absentee Voting' ~ 'AVMISC',
      subcategory == 'Absentee Voting-MOVE Act' ~ 'AVMOVE',
      subcategory == 'Absentee Voting-No-Excuse' ~ 'AVNOEX',
      subcategory == 'Absentee Voting-Permanent Status' ~ 'AVPERM',
      subcategory == 'Absentee Voting-Returning Ballots' ~ 'AVBRET',
      subcategory == 'Audits - Post-Election' ~ 'AUDITS',
      subcategory == 'Ballot Access-Candidates' ~ 'BACAND',
      subcategory == 'Ballot Access-Parties' ~ 'BAPART',
      subcategory == 'Ballots-Format & Design' ~ 'BALDES',
      subcategory == 'Candidates-Qualifications for Office' ~ 'CANQUL',
      subcategory == 'Candidates-Resign-to-Run' ~ 'CANRTR',
      subcategory == 'Candidates-Withdrawal' ~ 'CANWDW',
      subcategory == 'Candidates-Write-in' ~ 'CANWRI',
      subcategory == 'Challenges to Voters' ~ 'VTRCHA',
      subcategory == 'Contests' ~ 'CNTEST',
      subcategory == 'Cost of Elections' ~ 'ELCOST',
      subcategory == 'Counting  Votes' ~ 'VCOUNT',
      subcategory == 'Dates of Elections' ~ 'ELDATE',
      subcategory == 'DREs-Paper Trail' ~ 'PTDRES',
      subcategory == 'Election Crimes' ~ 'CRIMES',
      subcategory == 'Election Data-Collection/Retention of' ~ 'DATART',
      subcategory == 'Election Day Holiday' ~ 'EDHOLI',
      subcategory == 'Election Officials-Campaign Activities' ~ 'EOCAMP',
      subcategory == 'Election Officials-County/Local' ~ 'EOLOCA',
      subcategory == 'Election Officials-Statewide' ~ 'EOSTWD',
      subcategory == 'Election Results' ~ 'REPRES',
      subcategory == 'Electioneering' ~ 'ELEING',
      subcategory == 'Electoral College' ~ 'ELECOL',
      subcategory == 'Electoral College-NPV' ~ 'ECONPV',
      subcategory == 'Emergencies/Disasters' ~ 'EMEDIS',
      subcategory == 'Exit Polling' ~ 'EXPOLL',
      subcategory == 'Fusion/Dual-Party' ~ 'DUALFU',
      subcategory == 'Internet/Electronic Voting' ~ 'INVOTE',
      subcategory == 'Mail Voting' ~ 'MAILVO',
      subcategory == 'Miscellaneous' ~ 'MISCEL',
      subcategory == 'Offices-Method of Filling' ~ 'FILING',
      subcategory == 'Political Parties' ~ 'POLPAR',
      subcategory == 'Poll Watchers' ~ 'POLWAT',
      subcategory == 'Poll Workers-Compensation' ~ 'PWCOMP',
      subcategory == 'Poll Workers' ~ 'PWMISC',
      subcategory == 'Poll Workers-Selection/Qualifications of' ~ 'PWQUAL',
      subcategory == 'Poll Workers-Training' ~ 'PWTRAI',
      subcategory == 'Poll Workers-Youth' ~ 'PWYOTH',
      subcategory == 'Polling Places-Arrangement of/Procedures at' ~ 'PPPROC',
      subcategory == 'Polling Places-Disabled Access/Assistance' ~ 'PPACES',
      subcategory == 'Polling Places-Hours' ~ 'PPVHRS',
      subcategory == 'Polling Places-Locations' ~ 'PPLOCA',
      subcategory == 'Polling Places-Vote Centers' ~ 'PPVCEN',
      subcategory == 'Precinct Definition' ~ 'PREDEF',
      subcategory == 'Primaries-Dates' ~ 'PRIDAT',
      subcategory == 'Primaries-General' ~ 'PRIMIS',
      subcategory == 'Primaries-Presidential' ~ 'PRIPUS',
      subcategory == 'Primaries-Run-offs' ~ 'PRIRNF',
      subcategory == 'Primaries-Types' ~ 'PRITYP',
      subcategory == 'Provisional Voting' ~ 'PROVOT',
      subcategory == 'Recounts' ~ 'RECOUN',
      subcategory == 'Registration Drives' ~ 'REGDRI',
      subcategory == 'Registration-Application Form/Content' ~ 'REGAPP',
      subcategory == 'Registration-Universal' ~ 'REGATO',
      subcategory == 'Registration-Centralized Voter List' ~ 'REGCVL',
      subcategory == 'Registration-Deadline' ~ 'REGDTE',
      subcategory == 'Registration-Election Day' ~ 'REGEDY',
      subcategory == 'Registration-Electronic' ~ 'REGELE',
      subcategory == 'Registration-ID Required' ~ 'REGIDR',
      subcategory == 'Registration-List Maintenance' ~ 'REGLST',
      subcategory == 'Registration-General' ~ 'REGMSC',
      subcategory == 'Registration-Electronic' ~ 'REGONL',
      subcategory == 'Registration-Preregistration' ~ 'REGPRE',
      subcategory == 'Registration-Sale/Distribution/Use of Lists' ~ 'REGSDL',
      subcategory == 'Run-Off Elections' ~ 'RUNOFF',
      subcategory == 'Special Elections' ~ 'SPELEC',
      subcategory == 'Straight Ticket Voting' ~ 'STVOTE',
      subcategory == 'Task Forces/Study Commissions/Interim Committees' ~ 'TFSCIC',
      subcategory == 'Vacancies' ~ 'VACNCY',
      subcategory == 'Voter Education/Information' ~ 'VEDINF',
      subcategory == 'Voter ID-All Voters' ~ 'VOTRID',
      subcategory == 'Voter ID-New Voters Only' ~ 'VOTRID',
      subcategory == 'Voters-Absence from Work' ~ 'VOTAFW',
      subcategory == 'Voters-Age' ~ 'VOTAGE',
      subcategory == 'Voters-Convicted Felons' ~ 'VOTFVR',
      subcategory == 'Voters-Miscellaneous Qualifications' ~ 'VOTMQU',
      subcategory == 'Voting Equipment/Technology' ~ 'TECHSS',
      subcategory == 'Voting System Standards/Testing/Security' ~ 'VSSCST',
      TRUE ~ NA_character_
    ),
    holder = 1
  ) |>
  filter(!is.na(my_label)) |>
  pivot_wider(id_cols = 'ncsl_id', names_from = my_label, values_from = holder, values_fn = sum) |>
  mutate(across(everything(), ~ replace(., is.na(.), 0)))

topic_cols <- sort(colnames(sleg)[2:ncol(sleg)])
sleg <- sleg |> select(ncsl_id, any_of(topic_cols))

clean_sleg <- clean |>
  select(-c(category, subcategory, session, statute, description)) |>
  distinct() |>
  inner_join(sleg, by = 'ncsl_id') |>
  rename(
    NCSLID = ncsl_id,
    UUID = uuid,
    STATE = state,
    BILLNUM = bill_no,
    YEAR = year,
    BILLSTATUS = status
  ) |>
  arrange(STATE, YEAR, BILLNUM, NCSLID)

# Save output
write_csv(clean, "legacy_ncsl/ncsl_2001_2010.csv")
write_csv(clean_nested_category, "legacy_ncsl/ncsl_2001_2010_nested.csv")
write_csv(clean_sleg, "output/ncsl_2001_2010_sleg.csv")

