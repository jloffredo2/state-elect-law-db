
# Function to count number of Dem coauthors (includes DFL in MN)
vrl_count_dem_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(rjson::fromJSON(json),"\\(D\\)")) + sum(str_detect(rjson::fromJSON(json),"\\(DFL\\)")),NA))
}

# Function to count number of Rep coauthors
vrl_count_rep_coauthors <- function(json){
  return(ifelse(!is.na(json),sum(str_detect(rjson::fromJSON(json),"\\(R\\)")),NA))
}

# Function to count total number of coauthors
vrl_count_coauthors <- function(json){
  return(ifelse(!is.na(json),length(rjson::fromJSON(json)),NA))
}

scrape_vrl <- function(year){
  print("scraping VRL storage json")
  bills <- jsonlite::fromJSON(readLines("https://tracker.votingrightslab.org/storage/bills.json"))$data
  tags <- jsonlite::fromJSON(readLines("https://tracker.votingrightslab.org/storage/tags.json"))$data
  topics <- jsonlite::fromJSON(readLines("https://tracker.votingrightslab.org/storage/topics.json"))$data %>% select(id, label = name, parent_id = parent)
  bucket_topics <- jsonlite::fromJSON(readLines("https://tracker.votingrightslab.org/storage/bucket_topics.json"))$data %>% select(id, label = text)
  
  #### Match up VRL with NCSL
  vrl_bill_database <- bills
  # Rename and change date to date type
  vrl_bill_database <- vrl_bill_database %>%
    rename(ACTNUM = chapter_number
           ,PREFILEDDATE = prefile_date
           ,INTRODUCEDDATE = intro_date) %>%
    mutate(PREFILEDDATE = mdy(PREFILEDDATE)
           ,INTRODUCEDDATE = mdy(INTRODUCEDDATE))
  # Recode
  vrl_bill_database$BILLNUM = sprintf("%s%i", vrl_bill_database$legtype, vrl_bill_database$bill_number)
  vrl_bill_database$BILLSTATUS = fct_recode(as.factor(vrl_bill_database$current_disposition),
                                            "Failed" = "Failed - Adjourned",
                                            "Pending" = "Pending - Carryover",
                                            "To Executive" = "To Governor")
  
  vrl_bill_database$AUTHORNAME = ifelse(str_detect(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),
         trimws(str_remove_all(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),"both"),
         vrl_bill_database$author)
  vrl_bill_database$AUTHORPARTY = str_remove_all(str_extract(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),"[()]")
  vrl_bill_database$LASTACTIONDATE = mdy(lapply(vrl_bill_database$status_actions, function(x){tail(x,1) %>% str_sub(1, 10)}))
  vrl_bill_database$HISTORY = lapply(vrl_bill_database$status_actions, rjson::toJSON)
  
  vrl_bill_database$COAUTHORS = lapply(str_split(vrl_bill_database$addl_auths,";"),rjson::toJSON)
  vrl_bill_database$COAUTHORS[vrl_bill_database$COAUTHORS=="\"NA\""] = NA
  vrl_bill_database$NCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_coauthors)
  vrl_bill_database$NDEMCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_dem_coauthors)
  vrl_bill_database$NREPCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_rep_coauthors)
  
  # JSONify tags
  vrl_bill_database$Tags.Viz = lapply(vrl_bill_database$tags$`-Viz`,rjson::toJSON)
  vrl_bill_database$Tags.21AVR = lapply(vrl_bill_database$tags$`21AVR`,rjson::toJSON)
  vrl_bill_database$Tags.Impact = lapply(vrl_bill_database$tags$`-Impact`,rjson::toJSON)
  vrl_bill_database$Tags.21ElctnCrms = lapply(vrl_bill_database$tags$`21ElctnCrms`,rjson::toJSON)
  vrl_bill_database$Tags.YearsActive = lapply(vrl_bill_database$tags$`-YearsActive`,rjson::toJSON)
  vrl_bill_database$Tags.21AbsenteeVtg = lapply(vrl_bill_database$tags$`21AbsenteeVtg`,rjson::toJSON)
  vrl_bill_database$Tags.21BlltRtrnVfctnCure = lapply(vrl_bill_database$tags$`21BlltRtrnVfctnCure`,rjson::toJSON)
  vrl_bill_database$Tags.21ShftInElctnAthrty = lapply(vrl_bill_database$tags$`21ShftInElctnAthrty`,rjson::toJSON)
  vrl_bill_database$Tags.21IntrfrncElctnAdmin = lapply(vrl_bill_database$tags$`21IntrfrncElctnAdmin`,rjson::toJSON)
  vrl_bill_database$Tags.21VtrRgstrn = lapply(vrl_bill_database$tags$`21VtrRgstrn`,rjson::toJSON)
  vrl_bill_database$Tags.21SDR = lapply(vrl_bill_database$tags$`21SDR`,rjson::toJSON)         
  vrl_bill_database$Tags.21PAVL = lapply(vrl_bill_database$tags$`21PAVL`,rjson::toJSON)
  vrl_bill_database$Tags.21InPrsnVtng = lapply(vrl_bill_database$tags$`21InPrsnVtng`,rjson::toJSON)
  vrl_bill_database$Tags.21VoterID = lapply(vrl_bill_database$tags$`21VoterID`,rjson::toJSON)
  vrl_bill_database$Tags.21VBMElections = lapply(vrl_bill_database$tags$`21VBMElections`,rjson::toJSON)  
  vrl_bill_database$Tags.21ErlyVtngAvlblty = lapply(vrl_bill_database$tags$`21ErlyVtngAvlblty`,rjson::toJSON)
  vrl_bill_database$Tags.21VtrLstMntncPrgs = lapply(vrl_bill_database$tags$`21VtrLstMntncPrgs`,rjson::toJSON)
  vrl_bill_database$Tags.21COVIDSttsEmrgncy = lapply(vrl_bill_database$tags$`21COVIDSttsEmrgncy`,rjson::toJSON)
  vrl_bill_database$Tags.21EmergingIssues = lapply(vrl_bill_database$tags$`21EmergingIssues`,rjson::toJSON)  
  vrl_bill_database$Tags.21VtngRstrtn = lapply(vrl_bill_database$tags$`21VtngRstrtn`,rjson::toJSON)
  vrl_bill_database$Tags.21IncrcrtdVtng = lapply(vrl_bill_database$tags$`21IncrcrtdVtng`,rjson::toJSON)
  vrl_bill_database$Tags.21ElctnDyVtngSts = lapply(vrl_bill_database$tags$`21ElctnDyVtngSts`,rjson::toJSON)
  vrl_bill_database$Tags.21Rdstrctng = lapply(vrl_bill_database$tags$`21Rdstrctng`,rjson::toJSON)      
  vrl_bill_database$Tags.21PrfCtznshp = lapply(vrl_bill_database$tags$`21PrfCtznshp`,rjson::toJSON)     
  vrl_bill_database$tags = NULL
  
  # VRL Rating
  vrl_bill_database <- vrl_bill_database %>%
    mutate(VRLRATING = case_when(
      Tags.Impact == "{\"tag\":\"Anti-voter\"}" ~ "Anti-voter",
      Tags.Impact == "{\"tag\":\"Pro-voter\"}" ~ "Pro-voter",
      Tags.Impact == "{\"tag\":\"Mixed_Unclear\"}" ~ "Mixed/Unclear",
      Tags.Impact == "{\"tag\":\"Neutral\"}" ~ "Neutral"
    ))
                                       
  # Topic dummies
  vrl_bill_database2 <- vrl_bill_database %>%
    mutate(across(starts_with("Tags."), .fns = str_remove_all, "null")) %>%
    mutate(
      AVAPPL = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "AppContentOrFormat") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppDdlnErlr") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppDdlnLtr") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRqIDCrtExpnd") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRqIDElmntRlx") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppMailedAutoProhbts") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppMailedAutoAllVtr") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppMailedAutoCrtnVtr") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRqNtryWtnsCrtExp") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRqNtryWtnsElmRlx") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppOnlnCrtOnlnPrtl") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppOnlnCrtPrntOnln") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppOnlnCrtPrntHrdCpy") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppOnlnOthr") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRqOthrInfoChng") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRqOthrInfoCrt") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRqOthrInfoElmn") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppApplsToFwrElctns") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppApplsToMoreElctns") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRtrnPstgPdByGov") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRtrnPstgPdByVtr") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "App3dPrtyMailDistr") ~ 1
      )
      ,AVBDIS = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "BlltDlvrInPrsPckpExp") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "BlltDlvrInPrsPckpRst") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "BlltDlvrOnlnAvlblAll") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "BlltDlvrOnlnAvlblSm") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "BlltDlvrTmlnEarlier") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "BlltDlvrTmlnLater") ~ 1
      )
      ,AVBRET = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "BlltRtrnVrfctnCure") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "RplcmntBlltRules") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "ApplcbltyAbsntVtng") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "BlltRtrnDdlnEarlier") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "BlltRtrnDdlnLater") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLAvlExpnd') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLAvlRstrct') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLDysHrsOps') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLDrpBxScty') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLLctnRqrmt') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLNmbrRqrmt') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLUnofDrpBx') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnInPrsnIDRqrs') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnPstgPdByGov') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnPstgPdByVtr') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnThrdPrtyExpd') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnThrdPrtyRstr') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'UOCAVA') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VtrReqNtryWtnsCrtExp') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VtrReqNtryWtnsElmRlx') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VtrReqOtherCrtExpn') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VtrReqOtherElmRlx') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VrfCntSgntrMtchAuto') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VrfCntSgntrChng') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VrfCntSgntrElmnt') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VrfCntSgntrTraining') ~ 1
      )
      ,AVELIG = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "ElgblExcsAccptExpnd") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "ElgblExcsAccptNrrw") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "ElgblNoExcsVtngCrts") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "ElgblNoExcsVtngElmnt") ~ 1
      )
      ,AVMIOV = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "UOCAVA") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'UOCAVA') ~ 1
      )
      ,AVNOEX = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "ElgblNoExcsVtngCrts") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "ElgblNoExcsVtngElmnt") ~ 1
      )
      ,AVEVIP = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "InPrsnWthExcsCrtExpn") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "InPrsnWthExcsElmRstr") ~ 1
      )
      ,AVMISC = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "Other") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "RsdntlFacilities") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "VtInPrsAftrBltCrtExp") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "VtInPrsAftrBltElmRst") ~ 1
      )
      ,BACURE = case_when(
        str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrChngExstPrcss") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrCrtsNewPrcss") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrCrblDfctExpnLst") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrCrblDfctRstrLst") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrDdlnMvsEarlier") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrDdlnMvsLater") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrMthdsExpnds") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrMthdsRstrcts") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrElmntPrcss") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrNtfctnChngMthd") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrNtfctnRcvsErlr") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "NtcCrNtfctnRcvsLtr") ~ 1
      )
      ,CRIMES = ifelse(str_length(`Tags.21ElctnCrms`) > 0, 1, 0)
      ,DATART = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "BlltTrckngCrtsExpnds") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "BlltTrckngElmntRstrt") ~ 1
      )
      ,EOLOCA = case_when(
        str_detect(`Tags.21ElctnCrms`, "ApplcblElctnOffcl") ~ 1
        ,str_detect(`Tags.21ElctnCrms`, "OtcmElxOffclIncrsCrm") ~ 1
        ,str_detect(`Tags.21ElctnCrms`, "OtcmElxOffclDcrsCrm") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntSgntrTraining") ~ 1
      )
      ,INVOTE = case_when(
        str_detect(`Tags.21BlltRtrnVfctnCure`, "OnlnVtngAvlblAllVtrs") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "OnlnVtngAvlblSmVtrs") ~ 1
      )
      ,MAILVO = case_when(
        str_detect(`Tags.21BlltRtrnVfctnCure`, "ApplcbltyVBMElctn") ~ 1
      )
      ,POLWAT = case_when(str_detect(`Tags.21ElctnCrms`, "ApplcblPollWtchrs") ~ 1)
      ,REGATO = ifelse(str_length(Tags.21AVR) > 0, 1, 0)
      ,VCOUNT = case_when(
        str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntGrndsRjctnChng") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntGrndsRjctnCrt") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntGrndsRjctnElmn") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntObsrvrNmbr") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntObsrvrPrcss") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntObsrvrQlfctn") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntTmlnCntngLssTm") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VrfCntTmlnCntngMrTm") ~ 1
      )
      ,VOTAST = case_when(
        str_detect(`Tags.21ElctnCrms`, "ApplcblThrdPrty") ~ 1
        ,str_detect(`Tags.21ElctnCrms`, "OtcmAsstVtrsIncrsCrm") ~ 1
        ,str_detect(`Tags.21ElctnCrms`, "OtcmAsstVtrsDcrsCrm") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "Acsbl3dPtyAsstBltCmp") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "AcsblLanguage") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "AcsblPhysDisablty") ~ 1
      )
      ,VOTFVR = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "IncarceratedVoting") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "IncarceratedVoting") ~ 1
      )
      ,VOTRID = case_when(
        str_detect(`Tags.21AbsenteeVtg`, "AppRqIDCrtExpnd") ~ 1
        ,str_detect(`Tags.21AbsenteeVtg`, "AppRqIDElmntRlx") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "BlltRtrnInPrsnIDRqrs") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VtrReqIDCrtExpnd") ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`, "VtrReqIDElmntRlx") ~ 1
      )
      ,VTDROP = case_when(
        str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLAvlExpnd') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLAvlRstrct') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLDysHrsOps') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLDrpBxScty') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLLctnRqrmt') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLNmbrRqrmt') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'BlltRtrnDOLUnofDrpBx') ~ 1
        
      )
      ,VTRCHA = case_when(
        str_detect(`Tags.21BlltRtrnVfctnCure`,'VrfCntBlltChllgPrcss') ~ 1
        ,str_detect(`Tags.21BlltRtrnVfctnCure`,'VrfCntBlltChllgGrnds') ~ 1
      )
    )
  
  return(vrl_bill_database)
}
