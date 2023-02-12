  library(tidyr)
  library(dplyr)
  library(forcats)
  library(lubridate)
  library(rvest)
  library(stringr)
  library(magrittr)
  library(purrr)
  
  rm(list=ls())
  
  source("code/misc_fxns.R")
  
  ## FUNCTIONS FOR SCRAPING VOTING RIGHTS LAB LEG TRACKER
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
  
  build_vrl_bill_database <- function(){
    print("scraping VRL storage json")
    bills <- jsonlite::fromJSON(readLines("https://tracker.votingrightslab.org/storage/bills.json"))$data
    tags <- jsonlite::fromJSON(readLines("https://tracker.votingrightslab.org/storage/tags.json"))$data 
    anti_voter_tags <- c("-Anti-Voter",tags$id[tags$name=="-Anti-Voter"],"Anti-voter",tags$id[tags$name=="Anti-voter"])
    pro_voter_tags <- c("-Pro-Voter",tags$id[tags$name=="-Pro-Voter"],"Pro-voter",tags$id[tags$name=="Pro-voter"])
    neutral_tags <- c("-Neutral",tags$id[tags$name=="-Neutral"],"Neutral",tags$id[tags$name=="Neutral"])
    mixed_tags <- c("-Mixed_Unclear",tags$id[tags$name=="-Mixed_Unclear"],"Mixed_Unclear",tags$id[tags$name=="Mixed_Unclear"])
    
    ####### CREATE VRL MAIN OUTPUT ######
    print("creating main output")
    #### Match up VRL with NCSL
    # Rename and change date to date type
    vrl_bill_database <- bills %>%
      rename(INTRODUCEDDATE = intro_date
             ,BILLTEXTURL = text_url
             ,BILLSUMMARY = summary) %>%
      mutate(INTRODUCEDDATE = mdy(INTRODUCEDDATE),
             YEAR = year(INTRODUCEDDATE)) %>%
      unnest(tags,keep_empty = T) %>%
      mutate_at(
        vars(starts_with("21"),`-Impact`),
        funs(map_chr(.,~.[[1]] %>% str_c(collapse = ", "))))
    # Recode
    vrl_bill_database$BILLNUM = sprintf("%s%i", vrl_bill_database$legtype, vrl_bill_database$bill_number)
    vrl_bill_database$BILLSTATUS = fct_recode(as.factor(vrl_bill_database$current_disposition),
                                              "Failed" = "Failed - Adjourned",
                                              "Pending" = "Pending - Carryover",
                                              "To Executive" = "To Governor",
                                              "Enacted" = "Adopted")
    
    vrl_bill_database$AUTHORNAME = ifelse(str_detect(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),
           trimws(str_remove_all(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),"both"),
           vrl_bill_database$author)
    vrl_bill_database$AUTHORPARTY = str_remove_all(str_extract(vrl_bill_database$author,"\\([A-Z]{1,3}\\)"),"[()]")
    vrl_bill_database$LASTACTIONDATE = mdy(lapply(vrl_bill_database$status_actions, function(x){tail(x,1) %>% str_sub(1, 10)}))
    vrl_bill_database$HISTORY = lapply(vrl_bill_database$status_actions, rjson::toJSON)
    vrl_bill_database$HISTORY = unlist(vrl_bill_database$HISTORY)
    vrl_bill_database$HISTORY[vrl_bill_database$HISTORY=="\"NA\""] = NA
    
    vrl_bill_database$COAUTHORS = lapply(str_split(vrl_bill_database$addl_auths,";"),rjson::toJSON)
    vrl_bill_database$COAUTHORS = unlist(vrl_bill_database$COAUTHORS)
    vrl_bill_database$COAUTHORS[vrl_bill_database$COAUTHORS=="\"NA\""] = NA
  
    vrl_bill_database$NCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_coauthors)
    vrl_bill_database$NDEMCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_dem_coauthors)
    vrl_bill_database$NREPCOAUTHORS = sapply(vrl_bill_database$COAUTHORS,vrl_count_rep_coauthors)
    
    # VRL Rating
    vrl_bill_database <- vrl_bill_database %>%
      mutate(VRLRATING = case_when(
        str_detect(`-Impact`, str_c(anti_voter_tags, collapse = "|")) ~ "R",
        str_detect(`-Impact`, str_c(pro_voter_tags, collapse = "|")) ~ "E",
        str_detect(`-Impact`, str_c(neutral_tags, collapse = "|")) ~ "N",
        str_detect(`-Impact`, str_c(mixed_tags, collapse = "|")) ~ "M"))
                                         
    # Topic dummies
    vrl_bill_database <- vrl_bill_database %>%
      mutate(across(starts_with("21"), .fns = str_remove_all, "null")) %>%
      mutate(
        AVAPPL = case_when(
          str_detect(`21AbsenteeVtg`, "AppContentOrFormat") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppDdlnErlr") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppDdlnLtr") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRqIDCrtExpnd") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRqIDElmntRlx") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppMailedAutoProhbts") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppMailedAutoAllVtr") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppMailedAutoCrtnVtr") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRqNtryWtnsCrtExp") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRqNtryWtnsElmRlx") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppOnlnCrtOnlnPrtl") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppOnlnCrtPrntOnln") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppOnlnCrtPrntHrdCpy") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppOnlnOthr") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRqOthrInfoChng") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRqOthrInfoCrt") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRqOthrInfoElmn") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppApplsToFwrElctns") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppApplsToMoreElctns") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRtrnPstgPdByGov") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRtrnPstgPdByVtr") ~ 1
          ,str_detect(`21AbsenteeVtg`, "App3dPrtyMailDistr") ~ 1
        )
        ,AVBDIS = case_when(
          str_detect(`21AbsenteeVtg`, "BlltDlvrInPrsPckpExp") ~ 1
          ,str_detect(`21AbsenteeVtg`, "BlltDlvrInPrsPckpRst") ~ 1
          ,str_detect(`21AbsenteeVtg`, "BlltDlvrOnlnAvlblAll") ~ 1
          ,str_detect(`21AbsenteeVtg`, "BlltDlvrOnlnAvlblSm") ~ 1
          ,str_detect(`21AbsenteeVtg`, "BlltDlvrTmlnEarlier") ~ 1
          ,str_detect(`21AbsenteeVtg`, "BlltDlvrTmlnLater") ~ 1
          ,str_detect(`21VBMElections`, "BlltDlvrTmlnLatere") ~ 1
          ,str_detect(`21VBMElections`, "BlltDlvrTmlnEarlier") ~ 1
          ,str_detect(`21VBMElections`, "BlltDlvrAltrntAddrss") ~ 1
        )
        ,AVBRET = case_when(
          str_detect(`21AbsenteeVtg`, "BlltRtrnVrfctnCure") ~ 1
          ,str_detect(`21AbsenteeVtg`, "RplcmntBlltRules") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "ApplcbltyAbsntVtng") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "BlltRtrnDdlnEarlier") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "BlltRtrnDdlnLater") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLAvlExpnd') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLAvlRstrct') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLDysHrsOps') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLDrpBxScty') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLLctnRqrmt') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLNmbrRqrmt') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLUnofDrpBx') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnInPrsnIDRqrs') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnPstgPdByGov') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnPstgPdByVtr') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnThrdPrtyExpd') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnThrdPrtyRstr') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'UOCAVA') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VtrReqNtryWtnsCrtExp') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VtrReqNtryWtnsElmRlx') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VtrReqOtherCrtExpn') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VtrReqOtherElmRlx') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VrfCntSgntrMtchAuto') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VrfCntSgntrChng') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VrfCntSgntrElmnt') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VrfCntSgntrTraining') ~ 1
          ,str_detect(`21VBMElections`, "BlltRtrnVrfctnCure") ~ 1
        )
        ,AVDLIN = case_when(
          str_detect(`21AbsenteeVtg`, "AppDdlnErlr") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppDdlnLtr") ~ 1
        )
        ,AVELIG = case_when(
          str_detect(`21AbsenteeVtg`, "ElgblExcsAccptExpnd") ~ 1
          ,str_detect(`21AbsenteeVtg`, "ElgblExcsAccptNrrw") ~ 1
          ,str_detect(`21AbsenteeVtg`, "ElgblNoExcsVtngCrts") ~ 1
          ,str_detect(`21AbsenteeVtg`, "ElgblNoExcsVtngElmnt") ~ 1
        )
        ,AVMIOV = case_when(
          str_detect(`21AbsenteeVtg`, "UOCAVA") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'UOCAVA') ~ 1
          ,str_detect(`21VBMElections`, "UOCAVA") ~ 1
        )
        ,AVNOEX = case_when(
          str_detect(`21AbsenteeVtg`, "ElgblNoExcsVtngCrts") ~ 1
          ,str_detect(`21AbsenteeVtg`, "ElgblNoExcsVtngElmnt") ~ 1
        )
        ,AVEVIP = case_when(
          str_detect(`21AbsenteeVtg`, "InPrsnWthExcsCrtExpn") ~ 1
          ,str_detect(`21AbsenteeVtg`, "InPrsnWthExcsElmRstr") ~ 1
          ,str_detect(`21InPrsnVtng`,"ApplcblApplsErlyVtng") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbBOEDStyleEV") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbBOInPrsnNoExc") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbBOJrsdctnOptn") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbBOMndtrySttwd") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbBOAllElctns") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbBOCrtnElctns") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbCreates") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbEliminates") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbExpndsFcilitates") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "AvlbRestricts") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "IncarceratedVoting") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnDrvThruOutdrVtng") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnEmrgClsrRlct") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnHghCpctyVtCntr") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnVtrEdctn") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "Other") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "ResFcltyCrtChngPrcss") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngDdlnSttngDysHrs") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngEVPrdLgnthns") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngEVPrdShrtns") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngFrstDyMvsErlr") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngFrstDyMvsLtr") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngLstDyMvsErlr") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngLstDyMvsLtr") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngSatVtngCrtExpnd") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngSatVtngElmntNrrw") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngSunVtngCrtExpnd") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngSunVtngElmntNrrw") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngVtngHoursExpnds") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngVtngHoursRdcs") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "VtrBlltAppRqrmnts") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnAccsbltyRqrmnts") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnLctnCharCrtria") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnNmbrLctns") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnWaitTimes") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnSlctnPrcssTmln") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnVtgMchnsStations") ~ 1
        )
        ,AVMISC = case_when(
          str_detect(`21AbsenteeVtg`, "Other") ~ 1
          ,str_detect(`21AbsenteeVtg`, "RsdntlFacilities") ~ 1
          ,str_detect(`21AbsenteeVtg`, "VtInPrsAftrBltCrtExp") ~ 1
          ,str_detect(`21AbsenteeVtg`, "VtInPrsAftrBltElmRst") ~ 1
        )
        ,AVPERM = case_when(
          str_detect(`21PAVL`,"AppAddRgstrnApp") ~ 1
          ,str_detect(`21PAVL`,"AppDdlnErlr") ~ 1
          ,str_detect(`21PAVL`,"AppDdlnLtr") ~ 1
          ,str_detect(`21PAVL`,"AppOnlnCrtOnlnPrtl") ~ 1
          ,str_detect(`21PAVL`,"AppOnlnCrtPrntOnln") ~ 1
          ,str_detect(`21PAVL`,"AppOnlnCrtPrntHrdCp") ~ 1
          ,str_detect(`21PAVL`,"AppOtherChng") ~ 1
          ,str_detect(`21PAVL`,"AvlbltyBOAllVoters") ~ 1
          ,str_detect(`21PAVL`,"AvlbltyBOCrtnVoters") ~ 1
          ,str_detect(`21PAVL`,"AvlbltyCreates") ~ 1
          ,str_detect(`21PAVL`,"AvlbltyEliminates") ~ 1
          ,str_detect(`21PAVL`,"AvlbltyExpands") ~ 1
          ,str_detect(`21PAVL`,"AvlbltyRestricts") ~ 1
          ,str_detect(`21PAVL`,"Other") ~ 1
          ,str_detect(`21PAVL`,"PrmntLstAppBOAllVtr") ~ 1
          ,str_detect(`21PAVL`,"PrmntLstAppBOCrtnVt") ~ 1
          ,str_detect(`21PAVL`,"PrmntLstAppCrtExpnd") ~ 1
          ,str_detect(`21PAVL`,"PrmntLstAppElmtRstr") ~ 1
          ,str_detect(`21PAVL`,"RmvlFrmLstMksEsr") ~ 1
          ,str_detect(`21PAVL`,"RmvlFrmLstMksHrdr") ~ 1
          ,str_detect(`21PAVL`,"AvlbltyProhibits") ~ 1
        )
        ,BACURE = case_when(
          str_detect(`21BlltRtrnVfctnCure`, "NtcCrChngExstPrcss") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrCrtsNewPrcss") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrCrblDfctExpnLst") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrCrblDfctRstrLst") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrDdlnMvsEarlier") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrDdlnMvsLater") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrMthdsExpnds") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrMthdsRstrcts") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrElmntPrcss") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrNtfctnChngMthd") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrNtfctnRcvsErlr") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "NtcCrNtfctnRcvsLtr") ~ 1
        )
        ,CRIMES = case_when(
          str_length(`21ElctnCrms`) > 0 ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"IssueRprtInvstgEnfrc") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"InvstPrsctPrprtdCrms") ~ 1
        )
        ,DATART = case_when(
          str_detect(`21AbsenteeVtg`, "BlltTrckngCrtsExpnds") ~ 1
          ,str_detect(`21AbsenteeVtg`, "BlltTrckngElmntRstrt") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrDataUsePrtsnData") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrDataUseRaceData") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "ChngPrcssDataSrcs") ~ 1
          ,str_detect(`21VtngRstrtn`, "PrcssDataShrngAgncs") ~ 1
        )
        ,ELAUTH = case_when(
          str_detect(`21ShftInElctnAthrty`,"IssueRprtInvstgEnfrc") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainAttrnyGeneral") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainLegislature") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossJudiciary") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"IssueLtgtnAuthrOvrst") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"IssueEmrgncyAuthrty") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossChfElctnOffcl") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainStateBoard") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainChfElctnOffcl") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossGovernor") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossOthrOffcl") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctAuthrtyPrcs") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctAppntToElct") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossAttrnyGeneral") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctAppntToElct") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainGovernor") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossStateBoard") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainJudiciary") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossLegislature") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainOthrOffcl") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctElctToAppnt") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"IssueOthrChgElctAdmn") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainPoltclPrties") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainIndvdlsVtrs") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossPolltclPrties") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossIndvdlsVtrs") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PowerGrabLegislature") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"StiflingEmrgncyPwrs") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"IntrfrncMgmtLitigatn") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwLedByNonExprt") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwLckClrtyPrcss") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PnlzOffclLawfulPrctc") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"BfrctnSttFdrlElx") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PrtsnAptmntElxnOffcl") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"UnjstfdBrdnsOnOffcls") ~ 1
        )
        ,EMEDIS = case_when(
          str_detect(`21AbsenteeVtg`,"EmrgVtngFacilitates") ~ 1
          ,str_detect(`21AbsenteeVtg`,"EmrgVtngRestricts") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"IssueEmrgncyAuthrty") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"StiflingEmrgncyPwrs") ~ 1
          ,str_length(`21COVIDSttsEmrgncy`) > 0 ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnEmrgClsrRlct") ~ 1
        )
        ,EOLOCA = case_when(
          str_detect(`21ElctnCrms`, "ApplcblElctnOffcl") ~ 1
          ,str_detect(`21ElctnCrms`, "OtcmElxOffclIncrsCrm") ~ 1
          ,str_detect(`21ElctnCrms`, "OtcmElxOffclDcrsCrm") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VrfCntSgntrTraining") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainLocalOffcls") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"IssueOvrstLoclOffcls") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctAuthrtyPrcs") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctAppntToElct") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctElctToAppnt") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"IntrfrncLocalAmdmins") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PnlzJobPrfrmnc") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PnlzOffclLawfulPrctc") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"RskVoterWrkrIntmdtn") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PnlzListMntncInfrctn") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PrtsnAptmntElxnOffcl") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"UnjstfdBrdnsOnOffcls") ~ 1
          ,str_detect(`21EmergingIssues`, "ElxnWrkrPrtctns") ~ 1
        )
        ,EOSTWD = case_when(
          str_detect(`21ShftInElctnAthrty`,"PwrLossChfElctnOffcl") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainStateBoard") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrGainChfElctnOffcl") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossOthrOffcl") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctAppntToElct") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"PwrLossStateBoard") ~ 1
          ,str_detect(`21ShftInElctnAthrty`,"OffclSlctElctToAppnt") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"UsurpngSttElxnAdmin") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PnlzJobPrfrmnc") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"StiflingEmrgncyPwrs") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"IntrfrncMgmtLitigatn") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwLedByNonExprt") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwLckClrtyPrcss") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PnlzOffclLawfulPrctc") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"BfrctnSttFdrlElx") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PrtsnAptmntElxnOffcl") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"UnjstfdBrdnsOnOffcls") ~ 1
          ,str_detect(`21EmergingIssues`, "ElxnWrkrPrtctns") ~ 1
        )
        ,EOGENR = max(EOLOCA,EOSTWD)
        ,INVOTE = case_when(
          str_detect(`21BlltRtrnVfctnCure`, "OnlnVtngAvlblAllVtrs") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "OnlnVtngAvlblSmVtrs") ~ 1
        )
        ,LNGACC = case_when(
          str_detect(`21AbsenteeVtg`, "AcsblLanguage") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "AcsblLanguage") ~ 1
          ,str_detect(`21InPrsnVtng`, "AcsblLanguage") ~ 1
          ,str_detect(`21VtrRgstrn`, "AcsblLanguage") ~ 1
          ,str_detect(`21VBMElections`, "AcsblLanguage") ~ 1
        )
        ,MAILVO = case_when(
          str_detect(`21BlltRtrnVfctnCure`, "ApplcbltyVBMElctn") ~ 1
          ,str_detect(`21VBMElections`, "AvlBOAllElctn") ~ 1
          ,str_detect(`21VBMElections`, "AvlBOSpcfcElctns") ~ 1
          ,str_detect(`21VBMElections`, "AvlBOJdxOptn") ~ 1
          ,str_detect(`21VBMElections`, "AvlBOMndtrySttwd") ~ 1
          ,str_detect(`21VBMElections`, "AvlCrtVBM") ~ 1
          ,str_detect(`21VBMElections`, "AvlElxAllwRqrVBMExpn") ~ 1
          ,str_detect(`21VBMElections`, "AvlElxAllwRqrVBMNrrw") ~ 1
          ,str_detect(`21VBMElections`, "AvlEliminatesVBM") ~ 1
          ,str_detect(`21VBMElections`, "AvlJdxAllwRqrVBMExpn") ~ 1
          ,str_detect(`21VBMElections`, "AvlJdxAllwRqrVBMNrrw") ~ 1
          ,str_detect(`21VBMElections`, "AvlPilotProject") ~ 1
          ,str_detect(`21VBMElections`, "AvlProhibitsSmAllVBM") ~ 1
          ,str_detect(`21VBMElections`, "BlltDlvrAltrntAddrss") ~ 1
          ,str_detect(`21VBMElections`, "BlltDlvrTmlnEarlier") ~ 1
          ,str_detect(`21VBMElections`, "BlltDlvrTmlnLater") ~ 1
          ,str_detect(`21VBMElections`, "BlltRtrnVrfctnCure") ~ 1
          ,str_detect(`21VBMElections`, "InPrsnVtngOptnCrtExp") ~ 1
          ,str_detect(`21VBMElections`, "InPrsnVtngOptnElmRst") ~ 1
          ,str_detect(`21VBMElections`, "IncarceratedVoting") ~ 1
          ,str_detect(`21VBMElections`, "Other") ~ 1
          ,str_detect(`21VBMElections`, "RplcmntBlltRules") ~ 1
          ,str_detect(`21VBMElections`, "RsdntlFacilities") ~ 1
          ,str_detect(`21VBMElections`, "UOCAVA") ~ 1
          ,str_detect(`21VBMElections`, "VoteCenterRqrmnts") ~ 1
          ,str_detect(`21VBMElections`, "AcsblLanguage") ~ 1
          ,str_detect(`21VBMElections`, "AcsblPhysDisablty") ~ 1
          ,str_detect(`21VBMElections`, "AcsblThirdPrtyAsst") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailAppAvlbltyExpnd") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailAppAvlbltyRstrct") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailAppPrcssRstrcts") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailAppPrcssFcltates") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailBlltDlvryFcltate") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailBlltDlvryRstrcts") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailBlltRtrnFcltates") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailBlltRtrnRstrcts") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailElgbltyCrtExp") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailElgbltyElmnRstr") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailEmrgRqstCrtExpnd") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailEmrgRqstElmRstrc") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailNtryWtnssRqFcltt") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailNotcCureFcltates") ~ 1
        )
        ,VOTSEC = case_when(
          str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLDrpBxScty') ~ 1
        )
        ,POLWAT = case_when(
          str_detect(`21ElctnCrms`, "ApplcblPollWtchrs") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PPnlzOvsghtPollWtchrs") ~ 1
          ,str_detect(`21InPrsnVtng`,"PllWtchrNmbrAuthObsr") ~ 1
          ,str_detect(`21InPrsnVtng`,"PllWtchrObsrvtnPrcss") ~ 1
          ,str_detect(`21InPrsnVtng`,"PllWtchrObsrvrQlfctn") ~ 1
        )
        ,PPACES = case_when(
          str_detect(`21InPrsnVtng`, "AcsblPhysDisablty") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnAccsbltyRqrmnts")~1
          ,str_detect(`21ElctnDyVtngSts`,"LctnAccsbltyRqrmnts") ~ 1
        )
        ,PPLOCA = case_when(
          str_detect(`21ErlyVtngAvlblty`, "LctnDrvThruOutdrVtng")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnEmrgClsrRlct")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnHghCpctyVtCntr")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnVtrEdctn")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnAccsbltyRqrmnts")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnLctnCharCrtria")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnNmbrLctns")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnWaitTimes")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnSlctnPrcssTmln")~1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnVtgMchnsStations")~1
          ,str_detect(`21ElctnDyVtngSts`,"LctnEmrgClsrRlct") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrHghCpctyVtCntr") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnNmbrRqrdDcrs") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnNmbrRqrdIncr") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnVoterEducation") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"Other") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"PrcnctMpCnsldtnPrcss") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"PrcnctMpCnsldtnTmln") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"PrcnctMpDrwngPrcss") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"PrcnctMpDrwngTmln") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"PrcnctMpSpltChngPrcs") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"PrcnctMpSpltChngTmln") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"PrcnctBsdPllPlcCrts") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"PrcnctBsdPllPlcElmnt") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrBORqAuthAllElx") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrBORqAuthCrtnElx") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrBORqAuthCrtnJdx") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrBORqAuthSttwd") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrCrts") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrElxRqrAllwExpnd") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrElxRqrAllwRstrc") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrEliminates") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrJdxRqrAllwExpnd") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrJdxRqrAllwRstrc") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnAccsbltyRqrmnts") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnSlctnPrcssTmln") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnVtgMchnsStations") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnDrvThruOutdrVtng") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"LctnLctnCharCrtria") ~ 1
        )
        ,PPVCEN = case_when(
          str_detect(`21ElctnDyVtngSts`,"VtCtrHghCpctyVtCntr") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrBORqAuthAllElx") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrBORqAuthCrtnElx") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrBORqAuthCrtnJdx") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrBORqAuthSttwd") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrCrts") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrElxRqrAllwExpnd") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrElxRqrAllwRstrc") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrEliminates") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrJdxRqrAllwExpnd") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtCtrJdxRqrAllwRstrc") ~ 1
        )
        ,PPVHRS = case_when(
          str_detect(`21ElctnDyVtngSts`,"VtHrsChngPrcssExtnd") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtHrsExpnds") ~ 1
          ,str_detect(`21ElctnDyVtngSts`,"VtHrsRdcs") ~ 1
        )
        ,PPGENR = max(PPACES,PPLOCA,PPVCEN,PPVHRS)
        ,PREDEF = case_when(
          str_detect(`21ElctnDyVtngSts`, "PrcnctMpCnsldtnPrcss") ~ 1
          ,str_detect(`21ElctnDyVtngSts`, "PrcnctMpCnsldtnTmln") ~ 1
          ,str_detect(`21ElctnDyVtngSts`, "PrcnctMpDrwngPrcss")~ 1
          ,str_detect(`21ElctnDyVtngSts`, "PrcnctMpDrwngTmln")~ 1
          ,str_detect(`21ElctnDyVtngSts`, "PrcnctMpSpltChngPrcs")~ 1
          ,str_detect(`21ElctnDyVtngSts`, "PrcnctMpSpltChngTmln")~ 1
        )
        ,PRIMIS = case_when(
          str_detect(`21VtrRgstrn`,"PrmryVtngMinorChng") ~ 1
          ,str_detect(`21VtrRgstrn`,"PrmryVtngMinorCrts") ~ 1
          ,str_detect(`21VtrRgstrn`,"PrmryVtngMinorElmnts") ~ 1
        )
        ,PROVOT = case_when(
          str_detect(`21InPrsnVtng`,"PrvsnlVtngGrnds") ~ 1
          ,str_detect(`21InPrsnVtng`,"PrvsnlVtngBlltVrfctn") ~ 1
          ,str_detect(`21InPrsnVtng`,"PrvsnlVtngPrcss") ~ 1
          ,str_detect(`21InPrsnVtng`,"PrvsnlVtngNtfctRsltn") ~ 1
          ,str_detect(`21SDR`, "VrfctnRqrCstPrvBllt") ~ 1
          ,str_detect(`21VoterID`, "AltsPrvBlltChngPrcss") ~ 1
        )
        ,PWCOMP = case_when(
          str_detect(`21InPrsnVtng`,"PllWrkrCmpstnChngUI") ~ 1
          ,str_detect(`21InPrsnVtng`,"PllWrkCmpstnDcrss") ~ 1
          ,str_detect(`21InPrsnVtng`,"PllWrkCmpstnIncrss") ~ 1
          ,str_detect(`21InPrsnVtng`,"PllWrkCmpstnChgBnft") ~ 1
        )
        ,PWMISC = case_when(
          str_detect(`21IntrfrncElctnAdmin`,"RskVoterWrkrIntmdtn") ~ 1
          ,str_detect(`21EmergingIssues`, "ElxnWrkrPrtctns") ~ 1
        )
        ,PWQUAL = case_when(
          str_detect(`21InPrsnVtng`,"PllWrkQlfctnCrtStrng") ~ 1
          ,str_detect(`21InPrsnVtng`,"PllWrkQlfctnElmntRlx") ~ 1
          ,str_detect(`21InPrsnVtng`,"PllWrkRcrtmtStratgs") ~ 1
        )
        ,REDIST = case_when(
          str_detect(`21Rdstrctng`, "ApplcbltyFedCngrsDst") ~ 1
          ,str_detect(`21Rdstrctng`, "ApplcbltyMncplLclDst") ~ 1
          ,str_detect(`21Rdstrctng`, "ApplcbltySttLgDstrct") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrDataUsePrtsnData") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrDataUseRaceData") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrMndtryPrmsvCrt") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrMndtryPrmsvElmnt") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrPrhbtdCrt") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrPrhbtdElmnt") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrRanking") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcCmmntsOfIntrs") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcCmpctnss") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcCmptvnss") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcCntgty") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcCorePriorDstr") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcIncmbncy") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcNesting") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcOther") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcPrtyOrCnddt") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcPltclSbdvsn") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcPpltn") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrTpcPrprtnlty") ~ 1
          ,str_detect(`21Rdstrctng`, "IncrCntNonRsdFedPrsn") ~ 1
          ,str_detect(`21Rdstrctng`, "IncrCntNonRsdSttPrsn") ~ 1
          ,str_detect(`21Rdstrctng`, "IncrCntSttRsdFedPrsn") ~ 1
          ,str_detect(`21Rdstrctng`, "IncrCntSttRsdSttPrsn") ~ 1
          ,str_detect(`21Rdstrctng`, "IncrRllctnPrcssCrtCh") ~ 1
          ,str_detect(`21Rdstrctng`, "Other") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsBckupPrcssElmnt") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsBprtAprvCrtStrRq") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsBprtAprvElmWknRq") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsGovVetoCrtExpnd") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsGovVetoElmnRstrc") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsJdclRvwCrtExpnd") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsJdclRvwElmRstrc") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsLegSprmjCrtStrRq") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsLegSprmjElmWknRq") ~ 1
          ,str_detect(`21Rdstrctng`, "PblcAccsPrtcptCrtExp") ~ 1
          ,str_detect(`21Rdstrctng`, "PblcAccsPrtcptElmRst") ~ 1
          ,str_detect(`21Rdstrctng`, "TmlnIntrmFnlDldnErlr") ~ 1
          ,str_detect(`21Rdstrctng`, "TmlnIntrmFnlDldnLtr") ~ 1
          ,str_detect(`21Rdstrctng`, "TmlnStrtPrcsErlr") ~ 1
          ,str_detect(`21Rdstrctng`, "TmlnStrtPrcsLtr") ~ 1
          ,str_detect(`21Rdstrctng`, "MapDrawingLegisltn") ~ 1
          ,str_detect(`21Rdstrctng`, "ApplcbltyOther") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthCmsnCrtsExpds") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthLegElmRstrAuth") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthLegCrtExpAuth") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthCmsnChgMbrApt") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthBOLegAprvsMps") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthBOLegDrwsMps") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthBOCmsnDrwsMps") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsBckupPrcssLeg") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthBOCmsnAprvsMps") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsBckupPrcssJdcl") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsBckupPrcssCmsn") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthBOVtrsAprvMps") ~ 1
          ,str_detect(`21Rdstrctng`, "AuthCmsnElmRst") ~ 1
          ,str_detect(`21Rdstrctng`, "MapDarwingLegisltn") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsBckupPrcssCrts") ~ 1
          ,str_detect(`21Rdstrctng`, "PrmryAuthCmmssCrtExp") ~ 1
          ,str_detect(`21Rdstrctng`, "PrmryAuthLegCrtExpnd") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsAdvsryCmssnCrt") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcsBckupPrcssChng") ~ 1
          ,str_detect(`21Rdstrctng`, "PrmryAuthLegElmnRstr") ~ 1
          ,str_detect(`21Rdstrctng`, "PrmryAuthLegChng") ~ 1
          ,str_detect(`21Rdstrctng`, "PrmryAuthCmmssMbrApt") ~ 1
          ,str_detect(`21Rdstrctng`, "PrcssOthrChng") ~ 1
          ,str_detect(`21Rdstrctng`, "CrtrMndtryPrmsvChng") ~ 1
        )
        ,REGAGY = case_when(
          str_detect(`21VtrRgstrn`,"RegAgncyAddAgncy") ~ 1
          ,str_detect(`21VtrRgstrn`,"RegAgncyChngPrcssDMV") ~ 1
          ,str_detect(`21VtrRgstrn`,"RegAgncyChngPrcsOthr") ~ 1
          ,str_detect(`21VtrRgstrn`,"RegAgncyRmvsAgncy") ~ 1
        )
        ,REGAPP = case_when(
          str_detect(`21VtrRgstrn`,"FrmCntntDsgn") ~ 1
          ,str_detect(`21IncrcrtdVtng`, "RgstrFrmAvlbltyExpnd") ~ 1
          ,str_detect(`21IncrcrtdVtng`, "RgstrFrmAvlbltyRstrc") ~ 1
        )
        ,REGATO = ifelse(str_length(`21AVR`) > 0, 1, 0)
        ,REGDRV = case_when(
          str_detect(`21VtrRgstrn`,"RegDrvAsstncAuthrzs") ~ 1
          ,str_detect(`21VtrRgstrn`,"RegDrvAsstncChngRq") ~ 1
          ,str_detect(`21VtrRgstrn`,"RegDrvAsstncPrhbt") ~ 1
        )
        ,REGDTE = case_when(
          str_detect(`21VtrRgstrn`,"DdlnNewRgstrnErlr") ~ 1
          ,str_detect(`21VtrRgstrn`,"DdlnNewRgstrnLtr") ~ 1
          ,str_detect(`21VtrRgstrn`,"DdlnUpdtPltcAfflErlr") ~ 1
          ,str_detect(`21VtrRgstrn`,"DdlnUpdtPltcAfflLtr") ~ 1
          ,str_detect(`21VtrRgstrn`,"DdlnUpdtRgstrnErlr") ~ 1
          ,str_detect(`21VtrRgstrn`,"DdlnUpdtRgstrnLtr") ~ 1
        ),
        REGEDY = case_when(
          str_detect(`21SDR`,"AvlEVCrtsExpnds") ~ 1
          ,str_detect(`21SDR`,"AvlEVElmntRstrct") ~ 1
          ,str_detect(`21SDR`,"AvlElctDyCrtExpnd") ~ 1
          ,str_detect(`21SDR`,"AvlElctDyElmntRstrct") ~ 1
          ,str_detect(`21SDR`,"LngthRsdncRqCrtLngth") ~ 1
          ,str_detect(`21SDR`,"LngthRsdncRqElmShrtn") ~ 1
          ,str_detect(`21SDR`,"LctnChngDrngEV") ~ 1
          ,str_detect(`21SDR`,"LctnChngElctnDy") ~ 1
          ,str_detect(`21SDR`,"Other") ~ 1
          ,str_detect(`21SDR`,"PrfIDCrtExpndRqNnPht") ~ 1
          ,str_detect(`21SDR`,"PrfIDCrtExpndRqPht") ~ 1
          ,str_detect(`21SDR`,"PrfIDElmntRlxRqrmnts") ~ 1
          ,str_detect(`21SDR`,"PrfIDNewSDRNotRqrd") ~ 1
          ,str_detect(`21SDR`,"PrfRsdncCrtExpndRqr") ~ 1
          ,str_detect(`21SDR`,"PrfRsdncElmntRlxRqr") ~ 1
          ,str_detect(`21SDR`,"PrfRsdncNewSDRNotRqr") ~ 1
          ,str_detect(`21SDR`,"VrfctnAllwCstRegBllt") ~ 1
          ,str_detect(`21SDR`,"VrfctnChngPrcss") ~ 1
          ,str_detect(`21SDR`,"VrfctnRqrCstPrvBllt") ~ 1
        )
        ,REGELE = case_when(
          str_detect(`21VtrRgstrn`,"OVRAllwUpdtOnlyChng") ~ 1
          ,str_detect(`21VtrRgstrn`,"OVRAllwUpdtOnlyNew") ~ 1
          ,str_detect(`21VtrRgstrn`,"OVRBODMVRcrdNotRqrd") ~ 1
          ,str_detect(`21VtrRgstrn`,"OVRBODMVRcrdRqrd") ~ 1
          ,str_detect(`21VtrRgstrn`,"OVRChngExstngSystm") ~ 1
          ,str_detect(`21VtrRgstrn`,"OVRCrtsNewSystm") ~ 1
        )
        ,REGLST = case_when(
          str_detect(`21ShftInElctnAthrty`,"IssueVoterListMntnc") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PnlzListMntncInfrctn") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "ChngPrcssDataSrcs") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "ChngPrcssMtchngCrtr") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "ChngPrcssNtcRmvl") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "ChngPrcssOffclOvrsng") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "ChngPrcssRmdyImpRmvl") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "ChngPrcssRmvlTmng") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "IntrsttPgmCrssChckJn") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "IntrsttPgmCrssChckLv") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "IntrsttPgmERICJn") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "IntrsttPgmERICLv") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "IntrsttPgmOthr") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "Other") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "RsnRmvlChngAddrss") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "RsnRmvlCtznshpStts") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "RsnRmvlDth") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "RsnRmvlFlnyCnvctn") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "RsnRmvlMntlIncpcty") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "RsnRmvlNonFlnyCvctn") ~ 1
          ,str_detect(`21VtrLstMntncPrgs`, "RsnRmvlNonVtng") ~ 1
        )
        ,REGMSC = case_when(
          str_detect(`21VtrRgstrn`,"AddrssCnfdntltyPrgms") ~ 1
          ,str_detect(`21IncrcrtdVtng`, "RgstrPrcssFcltate") ~ 1
          ,str_detect(`21IncrcrtdVtng`, "RgstrPrcssRstrct") ~ 1
          ,str_detect(`21IncrcrtdVtng`, "RgstrRsdncyRls") ~ 1
        )
        ,REGPRE = case_when(
          str_detect(`21VtrRgstrn`,"PreRgstrMinorChng") ~ 1
          ,str_detect(`21VtrRgstrn`,"PreRgstrMinorCrt") ~ 1
          ,str_detect(`21VtrRgstrn`,"PreRgstrMinorElmn") ~ 1
        )
        ,REGSDL = case_when(
          str_detect(`21VtrRgstrn`,"RulesSaleDstrbVtrLst") ~ 1
        )
        ,REGGEN = max(REGAGY,REGAPP,REGATO,REGDRV,REGDTE,REGEDY,REGELE,REGMSC,REGPRE)
        ,REPRES = case_when(
          str_detect(`21ShftInElctnAthrty`,"IssueElctnRslts") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"IntrfrncCrtfctnRslts") ~ 1
          ,str_detect(`21EmergingIssues`, "RvwCrtfd2020ElctRslt") ~ 1
        )
        ,VCOUNT = case_when(
          str_detect(`21BlltRtrnVfctnCure`, "VrfCntGrndsRjctnChng") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VrfCntGrndsRjctnCrt") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VrfCntGrndsRjctnElmn") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VrfCntObsrvrNmbr") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VrfCntObsrvrPrcss") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VrfCntObsrvrQlfctn") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VrfCntTmlnCntngLssTm") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VrfCntTmlnCntngMrTm") ~ 1
          ,str_detect(`21IntrfrncElctnAdmin`,"PrhbtUseTabulators") ~ 1
        )
        ,VEDINF = case_when(
          str_detect(`21ElctnDyVtngSts`,"LctnVoterEducation") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnVtrEdctn") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "TmngVtngHoursRdcs") ~ 1
          ,str_detect(`21IncrcrtdVtng`,"VoterEducation") ~ 1
        )
        ,VOTAST = case_when(
          str_detect(`21ElctnCrms`, "ApplcblThrdPrty") ~ 1
          ,str_detect(`21ElctnCrms`, "OtcmAsstVtrsIncrsCrm") ~ 1
          ,str_detect(`21ElctnCrms`, "OtcmAsstVtrsDcrsCrm") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "Acsbl3dPtyAsstBltCmp") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "AcsblPhysDisablty") ~ 1
          ,str_detect(`21VtrRgstrn`, "AcsblPhysDisablty") ~ 1
          ,str_detect(`21InPrsnVtng`, "AcsblPhysDisablty") ~ 1
          ,str_detect(`21InPrsnVtng`,"AcsblThdPtyAsstnce") ~ 1
          ,str_detect(`21VBMElections`, "AcsblPhysDisablty") ~ 1
          ,str_detect(`21VBMElections`, "AcsblThirdPrtyAsst") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "LctnAccsbltyRqrmnts")~1
          ,str_detect(`21ElctnDyVtngSts`,"LctnAccsbltyRqrmnts") ~ 1
        )
        ,VOTFVR = case_when(
          str_detect(`21AbsenteeVtg`, "IncarceratedVoting") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "IncarceratedVoting") ~ 1
          ,str_detect(`21VBMElections`, "IncarceratedVoting") ~ 1
          ,str_detect(`21ErlyVtngAvlblty`, "IncarceratedVoting") ~ 1
          ,str_detect(`21VtngRstrtn`, "BOAutoAftrIncrcrtn") ~ 1
          ,str_detect(`21VtngRstrtn`, "BOAutoAftrPrlPrbtn") ~ 1
          ,str_detect(`21VtngRstrtn`, "BOAutoAftrPyFncOblg") ~ 1
          ,str_detect(`21VtngRstrtn`, "BOAutoAftrWtngPrd") ~ 1
          ,str_detect(`21VtngRstrtn`, "BOAutoAftrNoExcptns") ~ 1
          ,str_detect(`21VtngRstrtn`, "BOAutoAftrWExcptns") ~ 1
          ,str_detect(`21VtngRstrtn`, "ChgMthdApp2Auto") ~ 1
          ,str_detect(`21VtngRstrtn`, "ChgMthdAuto2App") ~ 1
          ,str_detect(`21VtngRstrtn`, "DsnfrnchCnvctsExpnd") ~ 1
          ,str_detect(`21VtngRstrtn`, "DsnfrnchCnvctsNrrws") ~ 1
          ,str_detect(`21VtngRstrtn`, "DsnfrnchEndAllCnvcts") ~ 1
          ,str_detect(`21VtngRstrtn`, "ExcptExpndInlgbCnvct") ~ 1
          ,str_detect(`21VtngRstrtn`, "ExcptNrrwInlgbCnvct") ~ 1
          ,str_detect(`21VtngRstrtn`, "FnclRqAltInbltyToPy") ~ 1
          ,str_detect(`21VtngRstrtn`, "FnclRqPrcsIdntfyOblg") ~ 1
          ,str_detect(`21VtngRstrtn`, "FnclRqCrtExpnd") ~ 1
          ,str_detect(`21VtngRstrtn`, "FnclRqNrrwElmnt") ~ 1
          ,str_detect(`21VtngRstrtn`, "FnclRqPrvdCnfrmPymen") ~ 1
          ,str_detect(`21VtngRstrtn`, "FnclRqRstrOnPymntPln") ~ 1
          ,str_detect(`21VtngRstrtn`, "FnclRqThrdPrtyPymnt") ~ 1
          ,str_detect(`21VtngRstrtn`, "Other") ~ 1
          ,str_detect(`21VtngRstrtn`, "PrcssChngAppPrcss") ~ 1
          ,str_detect(`21VtngRstrtn`, "PrcssDataShrngAgncs") ~ 1
          ,str_detect(`21VtngRstrtn`, "PrcssPreRgstrn") ~ 1
          ,str_detect(`21VtngRstrtn`, "PrcssPrfRgCrtStrngRq") ~ 1
          ,str_detect(`21VtngRstrtn`, "PrcssPrfRgElmntRlxRq") ~ 1
          ,str_detect(`21VtngRstrtn`, "PrcssPrvdRstRegInfo") ~ 1
          ,str_detect(`21VtngRstrtn`, "TmngErlrRntryPrcss") ~ 1
          ,str_detect(`21VtngRstrtn`, "TmngLtrRntryPrcss") ~ 1
          ,str_detect(`21VtngRstrtn`, "TmngWtngPrdCrtLngthn") ~ 1
          ,str_detect(`21VtngRstrtn`, "TmngWtngPrdElmntShrt") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailAppAvlbltyExpnd") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailAppAvlbltyRstrct") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailAppPrcssRstrcts") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailAppPrcssFcltates") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailBlltDlvryFcltate") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailBlltDlvryRstrcts") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailBlltRtrnFcltates") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailBlltRtrnRstrcts") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailElgbltyCrtExp") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailElgbltyElmnRstr") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailEmrgRqstCrtExpnd") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailEmrgRqstElmRstrc") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailNtryWtnssRqFcltt") ~ 1
          ,str_detect(`21VtngRstrtn`, "MailNotcCureFcltates") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStBOAthrzdCrtnLoc") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStBOAthrzdSttwd") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStBOAvlErlyVtng") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStBOAvlAllElctn") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStBOAvlSpcfcElctn") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStBOAvlElctnDay") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStBOMndtrySttwd") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStBORqrdCrtnLoc") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStCrtExpnd") ~ 1
          ,str_detect(`21VtngRstrtn`, "OnStElmntRstrct") ~ 1
          ,str_detect(`21VtngRstrtn`, "Other") ~ 1
          ,str_detect(`21VtngRstrtn`, "RgstrJlPrsnRgstAgncy") ~ 1
          ,str_detect(`21VtngRstrtn`, "RgstrFrmAvlbltyExpnd") ~ 1
          ,str_detect(`21VtngRstrtn`, "RgstrFrmAvlbltyRstrc") ~ 1
          ,str_detect(`21VtngRstrtn`, "RgstrPrcssFcltate") ~ 1
          ,str_detect(`21VtngRstrtn`, "RgstrPrcssRstrct") ~ 1
          ,str_detect(`21VtngRstrtn`, "RgstrRsdncyRls") ~ 1
          ,str_detect(`21VtngRstrtn`, "VoterEducation") ~ 1
          ,str_detect(`21VtngRstrtn`, "VtIDRqrAddJlPrsnID") ~ 1
          ,str_detect(`21VtngRstrtn`, "VtIDRqrExmptIDRqr") ~ 1
          ,str_detect(`21VtngRstrtn`, "VtIDRqrPrvdsCmplntID") ~ 1
        )
        ,VOTMQU = case_when(
          str_detect(`21VtrRgstrn`,"VtrRqrInfoRqrmnts") ~ 1
          ,str_detect(`21VtrRgstrn`,"VtrRqrRsdncyRqrmnts") ~ 1
          ,str_detect(`21VtrRgstrn`,"VtrRqrDcmntnRqrmnts") ~ 1
          ,str_detect(`21PrfCtznshp`, "ChngRqrmnt") ~ 1
          ,str_detect(`21PrfCtznshp`, "CrtRqrmnt") ~ 1
          ,str_detect(`21PrfCtznshp`, "ElmntRqrmnt") ~ 1
          ,str_detect(`21PrfCtznshp`, "Other") ~ 1
        )
        ,VOTRID = case_when(
          str_detect(`21AbsenteeVtg`, "AppRqIDCrtExpnd") ~ 1
          ,str_detect(`21AbsenteeVtg`, "AppRqIDElmntRlx") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "BlltRtrnInPrsnIDRqrs") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VtrReqIDCrtExpnd") ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`, "VtrReqIDElmntRlx") ~ 1
          ,str_detect(`21SDR`, "PrfIDCrtExpndRqNnPht") ~ 1
          ,str_detect(`21SDR`, "PrfIDCrtExpndRqPht") ~ 1
          ,str_detect(`21SDR`, "PrfIDElmntRlxRqrmnts") ~ 1
          ,str_detect(`21SDR`, "PrfIDNewSDRNotRqrd") ~ 1
          ,str_detect(`21InPrsnVtng`,"VtrRqrmntIDCrtExpnd") ~ 1
          ,str_detect(`21InPrsnVtng`,"VtrRqrmntIDElmntRlx") ~ 1
          ,str_detect(`21VoterID`,"AltsCrtsExpnds") ~ 1
          ,str_detect(`21VoterID`,"AltsElmntsRstrcts") ~ 1
          ,str_detect(`21VoterID`,"AltsPrvBlltChngPrcss") ~ 1
          ,str_detect(`21VoterID`,"ApplcbltyAbsntVtng") ~ 1
          ,str_detect(`21VoterID`,"ApplcbltyErlyVtng") ~ 1
          ,str_detect(`21VoterID`,"ApplcbltyElctnDyVtng") ~ 1
          ,str_detect(`21VoterID`,"BOPhotoIDNotRqrd") ~ 1
          ,str_detect(`21VoterID`,"BOPhotoIDRqrd") ~ 1
          ,str_detect(`21VoterID`,"IDRqrmntCrts") ~ 1
          ,str_detect(`21VoterID`,"FreeIDCrtsExpnds") ~ 1
          ,str_detect(`21VoterID`,"FreeIDElmntRstrct") ~ 1
          ,str_detect(`21VoterID`,"Other") ~ 1
          ,str_detect(`21VoterID`,"IDRqrmntRpls") ~ 1
          ,str_detect(`21VoterID`,"TypeAccptdAdds") ~ 1
          ,str_detect(`21VoterID`,"TypeAccptdElmnts") ~ 1
          ,str_detect(`21VoterID`,"ApplcbltyVBMElctns") ~ 1
          ,str_detect(`21VoterID`,"IDNmbrRqrmntCrts") ~ 1
          ,str_detect(`21VoterID`,"IDNmbrRqrmntCrts") ~ 1
          ,str_detect(`21IncrcrtdVtng`, "VtIDRqrAddJlPrsnID") ~ 1
          ,str_detect(`21IncrcrtdVtng`, "VtIDRqrExmptIDRqr") ~ 1
          ,str_detect(`21IncrcrtdVtng`, "VtIDRqrPrvdsCmplntID") ~ 1
        )
        ,VTDROP = case_when(
          str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLAvlExpnd') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLAvlRstrct') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLDysHrsOps') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLDrpBxScty') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLLctnRqrmt') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLNmbrRqrmt') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'BlltRtrnDOLUnofDrpBx') ~ 1
          
        )
        ,VTRCHA = case_when(
          str_detect(`21BlltRtrnVfctnCure`,'VrfCntBlltChllgPrcss') ~ 1
          ,str_detect(`21BlltRtrnVfctnCure`,'VrfCntBlltChllgGrnds') ~ 1
          ,str_detect(`21InPrsnVtng`,"ChllngChllngPrcss") ~ 1
          ,str_detect(`21InPrsnVtng`,"ChllngGrnds") ~ 1
        )
      ) %>%
      mutate_at(vars(AVAPPL:VTRCHA),replace_na, 0) %>%
      mutate_at(vars(NCOAUTHORS,NDEMCOAUTHORS,NREPCOAUTHORS),replace_na, 0) %>%
      select(-starts_with("21"))
    
    topic_cols = sort(colnames(vrl_bill_database)[44:99])
    
    # Produce final output
    vrl_bill_database <- vrl_bill_database %>%
      mutate(UUID = str_c(state,YEAR,BILLNUM)) %>%
      select(UUID
             ,YEAR
             ,STATE=state
             ,BILLNUM
             ,BILLSTATUS
             ,AUTHORNAME
             ,AUTHORPARTY
             ,INTRODUCEDDATE
             ,LASTACTIONDATE
             ,NCOAUTHORS
             ,NDEMCOAUTHORS
             ,NREPCOAUTHORS
             ,all_of(topic_cols)
             ,COAUTHORS
             ,HISTORY
             ,VRLRATING
             ,BILLTEXTURL
             ,BILLSUMMARY) %>%
      mutate(
        STATE = as.factor(STATE)
        ,AUTHORPARTY = as.factor(AUTHORPARTY))
    
    ####### CREATE VRL PROVISIONS TABLE #######
    print("creating provisions table")
    vrl_provisions <- bills %>%
      mutate(
        year = year(intro_date)
        ,UUID = str_c(state, year, sprintf("%s%i", legtype, bill_number))) %>%
      unnest(tags,keep_empty = T) %>%
      mutate_at(
        vars(starts_with("21"),`-Impact`),
        funs(map_chr(.,~.[[1]] %>% str_c(collapse = ", ")))) %>%
      mutate_at(vars(starts_with("21"),`-Impact`),
                funs(
                  case_when(
                    str_detect(., str_c(anti_voter_tags, collapse = "|")) ~ "R",
                    str_detect(., str_c(pro_voter_tags, collapse = "|")) ~ "E",
                    str_detect(., str_c(neutral_tags, collapse = "|")) ~ "N",
                    str_detect(., str_c(mixed_tags, collapse = "|")) ~ "M"
                  )
                )) %>%
      select(UUID,VRLRATING=`-Impact`,starts_with("21")) 
  
    colnames(vrl_provisions) <- str_remove_all(colnames(vrl_provisions),"21")
  
    ####### CREATE VRL PROCESS CHECK  #######
    print("creating process check table")
    vrl_process_check <- bills %>%
      mutate(
        year = year(intro_date)
        ,UUID = str_c(state, year, sprintf("%s%i", legtype, bill_number))) %>%
      unnest(tags,keep_empty = T) %>%
      mutate_at(
        vars(starts_with("21"),`-Impact`),
        funs(map_chr(.,~.[[1]] %>% str_c(collapse = ", ")))) %>%
      mutate(
        RESTRICT_AVVBM_SHRTAPP = ifelse(str_detect(`21AbsenteeVtg`,"AppDdlnErlr"),1,0)
        ,RESTRICT_AVVBM_SHRTSUB = ifelse(str_detect(`21AbsenteeVtg`,"BlltRtrnDdlnEarlier"),1,0)
        ,RESTRICT_AVVBM_ELIGIB = ifelse(str_detect(`21AbsenteeVtg`,"ElgblExcsAccptNrrw") |
                                        str_detect(`21AbsenteeVtg`,"ElgblNoExcsVtngElmnt") |
                                        str_detect(`21AbsenteeVtg`,"ElgblExcsAccptNrrw")|
                                        str_detect(`21PAVL`,"RmvlFrmLstMksHrdr")|
                                        str_detect(`21PAVL`,"AppDdlnErlr")|
                                        str_detect(`21PAVL`,"AvlbltyBOCrtnVoters")|
                                        str_detect(`21PAVL`,"AvlbltyEliminates")|
                                        str_detect(`21PAVL`,"AvlbltyProhibits")|
                                        str_detect(`21PAVL`,"AvlbltyRestricts")|
                                          str_detect(`21PAVL`,"PrmntLstAppElmtRstr")
                                        ,1,0)
        ,RESTRICT_AVVBM_SNDAPP = ifelse(str_detect(`21AbsenteeVtg`,"AppMailedAutoProhbts"),1,0)
        ,RESTRICT_AVVBM_SNDBLT = ifelse(str_detect(`21AbsenteeVtg`,"AppMailedAutoProhbts"),1,0)
        ,RESTRICT_AVVBM_LIMAST = ifelse(str_detect(`21BlltRtrnVfctnCure`,"BlltRtrnThrdPrtyRstrs"),1,0)
        ,RESTRICT_AVVBM_LIMCUR = ifelse(str_detect(`21BlltRtrnVfctnCure`,"NtcCrCrblDfctRstrLst") |
                                          str_detect(`21BlltRtrnVfctnCure`,"NtcCrDdlnMvsEarlier") |
                                          str_detect(`21BlltRtrnVfctnCure`,"NtcCrMthdsRstrcts") |
                                          str_detect(`21BlltRtrnVfctnCure`,"NtcCrElmntPrcss") |
                                          str_detect(`21BlltRtrnVfctnCure`,"NtcCrNtfctnRcvsLtr")
                                        ,1,0)
        ,RESTRICT_AVVBM_SIGREQ = ifelse(str_detect(`21BlltRtrnVfctnCure`,"VrfCntSgntrCrt"),1,0)
        ,RESTRICT_DROPBOX = ifelse(str_detect(`21BlltRtrnVfctnCure`,"BlltRtrnDOLAvlRstrct"),1,0)
        ,RESTRICT_VTRID = ifelse(
          str_detect(`21AbsenteeVtg`,"AppRqIDCrtExpnd") |
            str_detect(`21InPrsnVtng`,"VtrRqrmntIDCrtExpnd") |
            str_detect(`21VoterID`, "AltsElmntsRstrcts") |
            str_detect(`21VoterID`, "ApplcbltyAbsntVtng") |
            str_detect(`21VoterID`, "ApplcbltyErlyVtng") |
            str_detect(`21VoterID`, "ApplcbltyElctnDyVtng") |
            str_detect(`21VoterID`, "ApplcbltyThrdPrtyRtr") |
            str_detect(`21VoterID`, "BOPhotoIDRqrd") |
            str_detect(`21VoterID`, "IDRqrmntCrts") |
            str_detect(`21VoterID`, "FreeIDElmntRstrct")|
            str_detect(`21VoterID`, "TypeAccptdElmnts")|
            str_detect(`21SDR`,"PrfRsdncCrtExpndRqr")|
            str_detect(`21SDR`,"PrfIDCrtExpndRqPht")|
            str_detect(`21SDR`,"PrfIDCrtExpndRqNnPht")|
            str_detect(`21IncrcrtdVtng`,"VtIDRqrPrvdsCmplntID")
          ,1,0)
        ,RESTRICT_VTRREG_EXPREM = ifelse(
          str_detect(`21VtrLstMntncPrgs`,"RsnRmvlChngAddrss") |
          str_detect(`21VtrLstMntncPrgs`,"RsnRmvlCtznshpStts") |
          str_detect(`21VtrLstMntncPrgs`,"RsnRmvlDth") |
          str_detect(`21VtrLstMntncPrgs`,"RsnRmvlFlnyCnvctn") |
          str_detect(`21VtrLstMntncPrgs`,"RsnRmvlMntlIncpcty") |
          str_detect(`21VtrLstMntncPrgs`,"RsnRmvlNonFlnyCvctn") |
          str_detect(`21VtrLstMntncPrgs`,"RsnRmvlNonVtng")
          ,1,0
        )
        ,RESTRICT_VTRREG_DIFICT = ifelse(
          str_detect(`21AVR`,"ChngPrtcptAgncyRmvs") |
          str_detect(`21AVR`,"ChngREALIDCtznshpPrf") |
          str_detect(`21AVR`,"EliminatesAVR") |
          str_detect(`21IncrcrtdVtng`,"PrcssPrfRgCrtStrngRq") |
          str_detect(`21IncrcrtdVtng`,"RgstrFrmAvlbltyRstrc") |
          str_detect(`21IncrcrtdVtng`,"RgstrPrcssRstrct") |
          str_detect(`21VtrRgstrn`,"DdlnNewRgstrnErlr")|
          str_detect(`21VtrRgstrn`,"DdlnUpdtRgstrnErlr")|
          str_detect(`21VtrRgstrn`,"PreRgstrMinorElmn")|
          str_detect(`21VtrRgstrn`,"RegAgncyRmvsAgncy")|
          str_detect(`21VtrRgstrn`,"RegDrvAsstncPrhbt"),
          1,0
        )
        ,RESTRICT_VTTREG_NOSDYR = ifelse(
          str_detect(`21SDR`, "AvlEVElmntRstrct") |
          str_detect(`21SDR`, "AvlElctDyElmntRstrct")
          ,1,0
        )
        ,RESTRICT_IPV_PPLOCA = ifelse(
          str_detect(`21ElctnDyVtngSts`,"PrcnctBsdPllPlcElmnt") |
          str_detect(`21ElctnDyVtngSts`,"LctnNmbrRqrdIncr") |
          str_detect(`21ElctnDyVtngSts`,"VtCtrEliminates") |
          str_detect(`21ElctnDyVtngSts`,"VtHrsRdcs")
          ,1,0
        )
        ,RESTRICT_ERLVTG_LIMIT = ifelse(
          str_detect(`21ErlyVtngAvlblty`,"TmngVtngHoursRdcs") |
          str_detect(`21ErlyVtngAvlblty`,"TmngSunVtngElmntNrrw") |
          str_detect(`21ErlyVtngAvlblty`,"TmngSatVtngElmntNrrw") |
          str_detect(`21ErlyVtngAvlblty`,"TmngLstDyMvsErlr") |
          str_detect(`21ErlyVtngAvlblty`,"TmngEVPrdShrtns")
          ,1,0)
        ,RESTRICT_EOS_WEAKEN = ifelse(
          str_detect(`21IntrfrncElctnAdmin`,"BfrctnSttFdrlElx") |
          str_detect(`21IntrfrncElctnAdmin`,"PrtsnAptmntElxnOffcl") |
          str_detect(`21IntrfrncElctnAdmin`,"IntrfrncLocalAmdmins") |
          str_detect(`21IntrfrncElctnAdmin`,"IntrfrncMgmtLitigatn") |
          str_detect(`21IntrfrncElctnAdmin`,"PnlzJobPrfrmnc") |
          str_detect(`21IntrfrncElctnAdmin`,"PnlzListMntncInfrctn") |
          str_detect(`21IntrfrncElctnAdmin`,"PnlzOffclLawfulPrctc") |
          str_detect(`21IntrfrncElctnAdmin`,"PowerGrabLegislature") |
          str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwLckClrtyPrcss") |
          str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwLedByNonExprt") |
          str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwTrgts2020Elxn") |
          str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwTrgtsJrsdxn") |
          str_detect(`21IntrfrncElctnAdmin`,"ElxnRvwUnfndInvstgtn") |
          str_detect(`21IntrfrncElctnAdmin`,"RskVoterWrkrIntmdtn") |
          str_detect(`21IntrfrncElctnAdmin`,"StiflingEmrgncyPwrs") |
          str_detect(`21IntrfrncElctnAdmin`,"UnjstfdBrdnsOnOffcls") |
          str_detect(`21IntrfrncElctnAdmin`,"PrhbtUseTabulators") |
          str_detect(`21IntrfrncElctnAdmin`,"RstrctGrantMoney") |
          str_detect(`21IntrfrncElctnAdmin`,"UsurpngSttElxnAdmin")
          ,1,0
        )
        ,RESTRICT_CRIMES = ifelse(
          str_detect(`21ElctnCrms`,"ApplcblThrdPrty")|
          str_detect(`21ElctnCrms`,"ApplcblVoters")|
          str_detect(`21ElctnCrms`,"CivilPenaltiesCrts")|
          str_detect(`21ElctnCrms`,"CivilPenaltiesExpnds")|
          str_detect(`21ElctnCrms`,"FelonyCreates")|
          str_detect(`21ElctnCrms`,"MisdemeanorCreates")|
          str_detect(`21ElctnCrms`,"OtcmAsstVtrsIncrsCrm")|
          str_detect(`21ElctnCrms`,"OtcmVotersIncrsCrm")|
          str_detect(`21ElctnCrms`,"PnltyIncrsSvrtyPnlty")
          ,1,0
        )
        ,EXPAND_ABVVBM_ESYMVT = ifelse(
          str_detect(`21ErlyVtngAvlblty`,"AvlbBOInPrsnNoExc")|
          str_detect(`21PAVL`,"AppAddRgstrnApp")|
          str_detect(`21PAVL`,"AppDdlnLtr")|
          str_detect(`21PAVL`,"AvlbltyCreates")|
          str_detect(`21PAVL`,"AvlbltyBOAllVoters")|
          str_detect(`21PAVL`,"AvlbltyExpands")|
          str_detect(`21PAVL`,"PrmntLstAppCrtExpnd")|
          str_detect(`21AbsenteeVtg`,"AppMailedAutoAllVtr")|
          str_detect(`21AbsenteeVtg`,"AppOnlnCrtOnlnPrtl")|
          str_detect(`21AbsenteeVtg`,"AppOnlnCrtPrntOnln")|
          str_detect(`21AbsenteeVtg`,"AppRqIDElmntRlx")|
          str_detect(`21AbsenteeVtg`,"AppRqNtryWtnsElmRlx")|
          str_detect(`21AbsenteeVtg`,"AppRtrnPstgPdByGov")|
          str_detect(`21AbsenteeVtg`,"BlltDlvrOnlnAvlblAll")|
          str_detect(`21AbsenteeVtg`,"BlltDlvrOnlnAvlblSm")|
          str_detect(`21AbsenteeVtg`,"BlltDlvrTmlnLater")|
          str_detect(`21AbsenteeVtg`,"ElgblExcsAccptExpnd")|
          str_detect(`21AbsenteeVtg`,"ElgblNoExcsVtngCrts")|
          str_detect(`21AbsenteeVtg`,"InPrsnWthExcsCrtExpn")|
          str_detect(`21AbsenteeVtg`,"VtInPrsAftrBltCrtExp")|
          str_detect(`21BlltRtrnVfctnCure`,"BlltRtrnDdlnLater")|
          str_detect(`21BlltRtrnVfctnCure`,"BlltRtrnDOLAvlExpnd")|
  
          str_detect(`21VBMElections`," AvlCrtVBM")|
          str_detect(`21VBMElections`,"AvlElxAllwRqrVBMExpn")
          ,1,0
        )
        ,EXPAND_ABVVBM_ESYBCP = ifelse(
          str_detect(`21BlltRtrnVfctnCure`,"NtcCrCrblDfctExpnLst")|
          str_detect(`21BlltRtrnVfctnCure`,"NtcCrDdlnMvsLater")|
          str_detect(`21BlltRtrnVfctnCure`,"NtcCrMthdsExpnds")|
          str_detect(`21BlltRtrnVfctnCure`,"NtcCrNtfctnRcvsErlr")
          ,1,0
        )
        ,EXPAND_ABVVBM_MBAST = ifelse(
          str_detect(`21BlltRtrnVfctnCure`,"BlltRtrnThrdPrtyExpd")|
          str_detect(`21BlltRtrnVfctnCure`,"Acsbl3dPtyAsstBltCmp")|
          str_detect(`21VBMElections`,"AcsblThirdPrtyAsst")
          ,1,0
        )
        ,EXPAND_ABVVBM_INTVTG = ifelse(
          str_detect(`21BlltRtrnVfctnCure`,"OnlnVtngAvlblAllVtrs")|
          str_detect(`21BlltRtrnVfctnCure`,"OnlnVtngAvlblSmVtrs")
          ,1,0
        )
        ,EXPAND_VTRID = ifelse(
          str_detect(`21VoterID`,"AltsCrtsExpnds")|
          str_detect(`21VoterID`,"BOPhotoIDNotRqrd")|
          str_detect(`21VoterID`,"FreeIDCrtsExpnds")|
          str_detect(`21VoterID`,"IDNmbrRqrmntRpls")|
          str_detect(`21VoterID`,"TypeAccptdAdds")|
          str_detect(`21InPrsnVtng`,"VtrRqrmntIDElmntRlx")|
          str_detect(`21AbsenteeVtg`,"AppRqIDElmntRlx")|
          str_detect(`21BlltRtrnVfctnCure`,"VtrReqIDElmntRlx") |
          str_detect(`21SDR`,"PrfIDCrtExpndRqNnPht")|
          str_detect(`21SDR`,"PrfIDNewSDRNotRqrd")|
          str_detect(`21SDR`,"PrfIDElmntRlxRqrmnts")
          ,1,0
        )
        ,EXPAND_VTRREG_REGESY = ifelse(
          str_detect(`21VtrRgstrn`, 'RegDrvAsstncAuthrzs') |
          str_detect(`21VtrRgstrn`, 'RegAgncyAddAgncy') |
          str_detect(`21VtrRgstrn`, 'PreRgstrMinorCrt') |
          str_detect(`21VtrRgstrn`, 'DdlnUpdtRgstrnLtr') |
          str_detect(`21VtrRgstrn`, 'DdlnNewRgstrnLtr') |
          str_detect(`21AVR`, 'CrtAVR') |
          str_detect(`21AVR`, 'ChngPrtcptAgncyAdds')
          ,1,0
        )
        ,EXPAND_VTRREG_SMEDAY = ifelse(
          str_detect(`21SDR`,"PrfRsdncNewSDRNotRqr")|
          str_detect(`21SDR`,"LngthRsdncRqElmShrtn")|
          str_detect(`21SDR`,"AvlElctDyCrtExpnd")|
          str_detect(`21SDR`,"AvlEVCrtsExpnds")
          ,1,0
        )
        ,EXPAND_DISABLE = ifelse(
          str_detect(`21VBMElections`, "AcsblPhysDisablty")|
          str_detect(`21VtrRgstrn`, "AcsblPhysDisablty")|
          str_detect(`21BlltRtrnVfctnCure`, "AcsblPhysDisablty")|
          str_detect(`21AbsenteeVtg`, "AcsblPhysDisablty")|
          str_detect(`21InPrsnVtng`, "AcsblPhysDisablty")|
          str_detect(`21ErlyVtngAvlblty`, "LctnAccsbltyRqrmnts")|
          str_detect(`21ElctnDyVtngSts`, "LctnAccsbltyRqrmnts")
          ,1,0
        )
        ,EXPAND_IPV = ifelse(
          str_detect(`21ElctnCrms` ,"ApplcblPollWtchrs")|
          str_detect(`21ElctnDyVtngSts`,"VtHrsExpnds")|
          str_detect(`21ElctnDyVtngSts`,"VtCtrJdxRqrAllwExpnd")|
          str_detect(`21ElctnDyVtngSts`,"VtCtrHghCpctyVtCntr")|
          str_detect(`21ElctnDyVtngSts`,"VtCtrElxRqrAllwExpnd")|
          str_detect(`21ElctnDyVtngSts`,"VtCtrCrts")|
          str_detect(`21ElctnDyVtngSts`,"LctnNmbrRqrdIncr")|
          str_detect(`21ElctnDyVtngSts`,"PrcnctBsdPllPlcCrts")
          ,1,0
        )
        ,EXPAND_EARLY = ifelse(
          str_detect(`21ErlyVtngAvlblty`,"TmngVtngHoursExpnds")|
          str_detect(`21ErlyVtngAvlblty`,"TmngSunVtngCrtExpnd")|
          str_detect(`21ErlyVtngAvlblty`,"TmngSatVtngCrtExpnd")|
          str_detect(`21ErlyVtngAvlblty`,"TmngLstDyMvsLtr")|
          str_detect(`21ErlyVtngAvlblty`,"TmngFrstDyMvsErlr")|
          str_detect(`21ErlyVtngAvlblty`,"TmngEVPrdLgnthns")|
          str_detect(`21ErlyVtngAvlblty`,"AvlbCreates")|
          str_detect(`21ErlyVtngAvlblty`,"AvlbBOMndtrySttwd")|
          str_detect(`21ErlyVtngAvlblty`,"AvlbBOJrsdctnOptn")|
          str_detect(`21ErlyVtngAvlblty`,"AvlbBOInPrsnNoExc")|
          str_detect(`21ErlyVtngAvlblty`,"AvlbBOAllElctns")|
          str_detect(`21ErlyVtngAvlblty`,"AvlbBOCrtnElctns")
          ,1,0
        )
        ,EXPAND_LNGACS = ifelse(
          str_detect(`21VBMElections`,"AcsblLanguage")|
            str_detect(`21VtrRgstrn`,"AcsblLanguage")|
            str_detect(`21BlltRtrnVfctnCure`,"AcsblLanguage")|
            str_detect(`21AbsenteeVtg`,"AcsblLanguage")|
            str_detect(`21AbsenteeVtg`,"AcsblLanguage")
          ,1,0
        )
        ,EXPAND_STVRA = ifelse(str_detect(`21EmergingIssues`,"StateVtngRghtsPrtctn"),1,0)
        ,EXPAND_FELON_RSTVTR = ifelse(str_detect(`21VtngRstrtn`,"BOAutoAftrIncrcrtn"),1,0)
        ,EXPAND_FELON_EASY = ifelse(
          str_detect(`21VBMElections`,"IncarceratedVoting")|
          str_detect(`21BlltRtrnVfctnCure`,"IncarceratedVoting")|
          str_detect(`21AbsenteeVtg`,"IncarceratedVoting")|
          str_detect(`21IncrcrtdVtng`,"VtIDRqrExmptIDRqr")|
          str_detect(`21IncrcrtdVtng`, "VtIDRqrAddJlPrsnID")|
          str_detect(`21IncrcrtdVtng`, "RgstrPrcssFcltate")|
          str_detect(`21IncrcrtdVtng`, "RgstrFrmAvlbltyExpnd")|
          str_detect(`21IncrcrtdVtng`, "OnStCrtExpnd")|
          str_detect(`21IncrcrtdVtng`, "OnStBORqrdCrtnLoc")|
          str_detect(`21IncrcrtdVtng`, "OnStBOMndtrySttwd")|
          str_detect(`21IncrcrtdVtng`, "OnStBOAvlElctnDay")|
          str_detect(`21IncrcrtdVtng`, "OnStBOAvlSpcfcElctn")|
          str_detect(`21IncrcrtdVtng`, "OnStBOAvlAllElctn")|
          str_detect(`21IncrcrtdVtng`, "OnStBOAvlErlyVtng")|
          str_detect(`21IncrcrtdVtng`, "OnStBOAthrzdSttwd")|
          str_detect(`21IncrcrtdVtng`, "OnStBOAthrzdCrtnLoc")|
          str_detect(`21IncrcrtdVtng`, "MailNotcCureFcltates")|
          str_detect(`21IncrcrtdVtng`, "MailNtryWtnssRqFcltt")|
          str_detect(`21IncrcrtdVtng`, "MailEmrgRqstCrtExpnd")|
          str_detect(`21IncrcrtdVtng`, "MailElgbltyCrtExp")|
          str_detect(`21IncrcrtdVtng`, "MailBlltRtrnFcltates")|
          str_detect(`21IncrcrtdVtng`, "MailBlltDlvryFcltate")|
          str_detect(`21IncrcrtdVtng`, "MailAppPrcssFcltates")|
          str_detect(`21IncrcrtdVtng`, "MailAppAvlbltyExpnd")|
          str_detect(`21ErlyVtngAvlblty`, "IncarceratedVoting")
          ,1,0
        )
      ) %>%
      select(UUID,starts_with("EXPAND"),starts_with("RESTRICT"))
    
    
    return(list(vrl_bill_database=vrl_bill_database
                ,vrl_provisions=vrl_provisions
                ,vrl_process_check=vrl_process_check))
  }
  
  db <- build_vrl_bill_database()
  
vrl_bill_database <- db$vrl_bill_database
write.csv(vrl_bill_database, file = "output/vrl_bill_database.csv",row.names = FALSE)
save(vrl_bill_database, file = "output/vrl_bill_database.Rdata")

vrl_provisions <- db$vrl_provisions
write.csv(vrl_provisions, file = "output/vrl_provisions.csv",row.names = FALSE)
save(vrl_provisions, file = "output/vrl_provisions.Rdata")

vrl_process_check <- db$vrl_process_check
write.csv(vrl_process_check, file = "output/vrl_process_check",row.names = FALSE)
save(vrl_process_check, file = "output/vrl_process_check.Rdata")
