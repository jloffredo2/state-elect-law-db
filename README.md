# STATE ELECTIONS LEGISLATION DATABASE (2011-2024)
**Author**: Joseph Loffredo

**Institution**: MIT

**Date**: 2024-02-20

## DESCRIPTION
This dataset is the product of scraping the *National Conference of State Legislature*'s "State Elections Legislation Database" and *Voting Rights Lab*'s "Legislative Tracker." The code for producing this dataset can be found in this GitHub repo: https://github.com/jloffredo2/state-elect-law-db. See the `code` subfolder for all relevant code.

If you are using this dataset, please cite it and provide, NCSL and Voting Rights Lab with proper citation as follows:

> National Conference of State Legislatures. 2024. *State Elections Legislation Database*. www.ncsl.org/research/elections-and-campaigns/elections-legislation-database.aspx (February 20, 2024).

> Voting Rights Lab. 2024. *Legislative Tracker*. https://tracker.votingrightslab.org/pending/search (February 20, 2024).

*To access tidy versions of these databases, use the following files:*
* `output/ncsl_bill_database.csv`: NCSL "State Elections Legislation" database in `.csv` format
* `output/ncsl_bill_database.Rdata`: NCSL "State Elections Legislation" database saved as a `R` data frame
* `output/vrl_bill_database.csv`: VRL "Legislative Tracker" database in `.csv` format
* `output/vrl_bill_database.Rdata`: VRL "Legislative Tracker" database saved as a `R` data frame

## COLUMN MAPPINGS
All datasets have a similar structure and following the column label/definitions below.
### IDENTIFICATION
* `UUID` Unique identifier in the form `STATE`-`YEAR`-`BILLNUM` (can be used to join datasets)

* `YEAR` Year tracking organization has tagged bill

* `STATE` State bill is introduced in

* `BILLNUM` Bill number

* `AUTHORNAME` Last name of bill author

* `AUTHORPARTY` Party affiliation of bill author

### STATUS
* `BILLSTATUS` 	Current bill status
  * **Adopted**
  * **Enacted**
  * **Failed**
  * **Override Pending**
  * **Pending**
  * **To Congress**
  * **To Executive**
  * **Vetoed**
* `BILLOCATIOM` Bill location in legislative process
* `INTRODUCEDDATE` Date bill is officially introduced
* `LASTACTIONDATE` Date of bill's last action
* `NCOAUTHORS` Total number of coauthors
* `NDEMCOAUTHORS` Number of Democratic coauthors
* `NREPCOAUTHORS` Number of Republican coauthors

### TOPICS
*Each topic indictator takes the value 1 if the organization has tagged the bill to a given topic, 0 otherwise.*
* `AUDITS` 	Audits-Post Election
* `AVAPPL` 	Absentee Voting-Application for
* `AVBDIS` 	Absentee Voting-Distributing Ballots
* `AVBRET` 	Absentee Voting-Returning Ballots
* `AVDLIN`  Absentee Voting-Deadlines
* `AVELIG` 	Absentee Voting-Eligibility
* `AVEVIP` 	Absentee Voting-Early Voting/In-Person Absentee
* `AVMIOV` 	Absentee Voting-Military/Overseas
* `AVMISC` 	Absentee Voting-Misc.
* `AVMOVE` 	Absentee Voting-MOVE Act
* `AVNOEX` 	Absentee Voting-No Excuse
* `AVPERM` 	Absentee Voting-Permanent Status
* `BACAND` 	Ballot Access-Candidates
* `BACURE`  Ballot Return, Verification, and Cure
* `BALDES` 	Ballots-Format & Design
* `BAMEAS`  Ballot measures
* `BAPART` 	Ballot Access-Parties
* `CANQUL` 	Candidates-Qualifications for Office
* `CANRTR` 	Candidates-Resign to Run
* `CANWDW` 	Candidates-Withdrawal/Death
* `CANWRI` 	Candidates-Write-in
* `CNTEST` 	Contests
* `CRIMES` 	Election Crimes
* `CYBSEC` 	Cybersecurity
* `DATART` 	Election Data-Collection/Retention of
* `DGVOTE`  Digital/electronic voting
* `DUALFU` 	Fusion/Dual-Party
* `ECONPV` 	Electoral College-National Popular Vote
* `EDHOLI` 	Election Day Holiday
* `ELAUTH`  Shifts in Election Authority
* `ELCOST` 	Cost/Funding of Elections
* `ELDATE` 	Dates of Elections
* `ELECOL` 	Electoral College
* `ELEING` 	Electioneering
* `EMEDIS` 	Emergencies/Disasters
* `EOCAMP` 	Election Officials-Campaign Activities
* `EOGENR`  Election Official-General (anything broadly related to election official)
* `EOLOCA` 	Election Officials-Local
* `EOSTWD` 	Election Officials-Statewide
* `EPLOCL`  Local election procedures
* `EXPOLL` 	Exit Polling
* `FILING` 	Offices-Method of Filling
* `INVOTE` 	Internet Voting
* `LNCACC`  Language accommodations
* `MAILVO` 	Mail Voting
* `MISCEL` 	Miscellaneous
* `POLPAR` 	Political Parties
* `POLWAT` 	Poll Watchers
* `PPACES` 	Polling Places-Disabled Access
* `PPGENR`  Polling Places-General (nonspecific)
* `PPLOCA` 	Polling Places-Locations
* `PPPROC` 	Polling Places-Arrangement of/Procedures at
* `PPVCEN` 	Polling Places-Vote Centers
* `PPVHRS` 	Polling Places-Hours
* `PREDEF` 	Precinct Definition
* `PRIDAT` 	Primaries-Dates
* `PRIMIS` 	Primaries-Misc.
* `PRIPUS` 	Primaries-Presidential
* `PRIRNF` 	Primaries-Runoff
* `PRITYP` 	Primaries-Types
* `PROVOT` 	Provisional Voting
* `PTDRES` 	DREs-Paper Trail
* `PWCOMP` 	Poll Workers-Compensation
* `PWMISC` 	Poll Workers-Misc.
* `PWQUAL` 	Poll Workers-Selection/Qualifications of
* `PWTRAI` 	Poll Workers-Training
* `PWYOTH` 	Poll Workers-Youth
* `RECALL`  Recall elections
* `RECOUN` 	Recounts
* `REDIST`  Redistricting
* `REGAGY`  Registration-Agencies
* `REGAPP` 	Registration-Application Form/Content
* `REGATO` 	Registration-Automatic
* `REGCVL` 	Registration-Centralized Voter List
* `REGDRI` 	Registration Drives
* `REGDTE` 	Registration-Deadline
* `REGEDY` 	Registration-Election Day/Same Day
* `REGELE` 	Registration-Electronic
* `REGGEN`  Registration-General
* `REGIDR` 	Registration-ID Required
* `REGLST` 	Registration-List Maintenance
* `REGMSC` 	Registration-Misc.
* `REGPRE` 	Registration-Preregistration
* `REGSDL` 	Registration-Sale/Distribution/Use of Lists
* `REPRES` 	Election Results/Canvass, Reporting of
* `RUNOFF` 	Run-Off Elections
* `SPELEC` 	Special Elections
* `STVOTE` 	Straight Ticket Voting
* `TECHSS` 	Voting Equipment/Technology-Selection & Standards
* `TFSCIC` 	Task Forces/Study Commissions/Interim Committees
* `VACNCY` 	Vacancies
* `VCOUNT` 	Counting Votes
* `VEDINF` 	Voter Education/Information
* `VOTAFW` 	Voters-Absence from Work
* `VOTAGE` 	Voters-Age
* `VOTAST` 	Voters-Assistance to
* `VOTEME` 	Alt Voting Methods (Ranked Choice, etc)
* `VOTFVR` 	Voters-Felon Voting Rights
* `VOTMQU` 	Voters-Miscellaneous Qualifications
* `VOTRID` 	Voter Identification
* `VOTSEC`  Voting security (polling places, drop box, counting)
* `VSSCST` 	Voting System Testing/Security/Storage
* `VTDROP`  Ballot Drop-off Locations
* `VTRCHA` 	Challenges to Voters

### ADDITIONAL INFORMATIONAL COLUMNS
* `VRLRATING` 	VRL's coding of overall bill impact
  * **Anti-voter** = `R`
  * **Pro-voter** = `E`
  * **Mixed/Unclear** = `M`
  * **Neutral** = `N`
* `BILLTEXTURL` 	URL to access bill's text
* `BILLSUMMARY` 	Organization's summarization of bill

### MISC DATA
*If using R, use `rjson::fromJSON()` to transform these values into an R vector.*

* `COAUTHORS` 	JSON string listing all coauthors
* `HISTORY` 	JSON string of bill's history log

## VRL PROVISION RATINGS
The Voting Rights Lab dataset also codes whether provisions related to certain topics are **Anti-voter**, **Pro-voter**, **Neutral**, or **Mixed/Unclear**. You can find those codings in `output/vrl_provisions.csv` or `output/vrl_provisions.RData`. What appears in the dataset follows the same `VRLRATING` mapping as above. The files in `output/vrl_process_check.csv` and `output/vrl_process_check.RData` are used to check handcoding for some of my ongoing projects.
