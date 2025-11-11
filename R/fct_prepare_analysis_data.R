#' Prepare Analysis Dataset from Survey Data
#'
#' @description Prepares analysis dataset from raw survey data
#'
#' @param CountryInfo R6 object containing country and survey information
#' @param AnalysisInfo R6 object for storing analysis results
#' @param session Shiny session object for sending spinner messages
#'
#' @return TRUE if successful, FALSE otherwise
#' @noRd
prepare_analysis_data <- function(CountryInfo, AnalysisInfo, session, ref_tab_all) {
  ## #following two lines works on local server not deployed
  ## data("indicatorList", package = "surveyPrev", envir = environment())
  ## ref_tab_all <- indicatorList
  ## # sol 2 failed
  ##ref_tab_all <- get("ref_tab_all", envir = parent.frame())
  # Check if already prepared
  print(head(ref_tab_all))
  indicatorList <- ref_tab_all
  if(!is.null(CountryInfo$svy_analysis_dat())) {
    return(TRUE)
  }
  
  # Define recode lists
  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  
  # Get required recodes
  required_recode <- recode_list_abbrev[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                          recode_list_abbrev]==T)]
  
  # Check if data is complete
  recode_status_check <- CountryInfo$check_svy_dat_upload(required_recode, CountryInfo$svy_dat_list())
  GPS_status_check <- is.null(CountryInfo$svy_GPS_dat())
  
  if(!all(c(!recode_status_check, !GPS_status_check))) {
    return(FALSE)  # Data not complete
  }
  
  session$sendCustomMessage('controlSpinner', list(action = "show",
                                                   message = "Preparing analysis dataset, please wait..."))
  
  success <- tryCatch({
    svy_dat_list <- CountryInfo$svy_dat_list()
    
    # Prepare recode data
    if(length(required_recode) > 1){
      svy_dat_recode <- svy_dat_list[required_recode]
      names(svy_dat_recode) <- as.character(get_recode_names(required_recode))
    } else {
      svy_dat_recode <- svy_dat_list[[required_recode]]
    }
    
    # Correct Nigeria data if needed
    if(CountryInfo$country() == 'Nigeria' &&
       CountryInfo$svyYear_selected() == 2018 &&
       ('PR' %in% required_recode || 'HR' %in% required_recode)){
      message('correcting Nigeria 2018 HR recode')
      svy_dat_recode[svy_dat_recode$hv001=="1270","hv025"] = 2
    }
    
    # Generate analysis dataset
    analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                                indicator = CountryInfo$svy_indicator_var())
    
    CountryInfo$svy_analysis_dat(analysis_dat)
    
    # Generate national estimates
    gadm.list <- CountryInfo$GADM_list()
    cluster.geo <- CountryInfo$svy_GPS_dat()
    
    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',1)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',1)]],
                                            by.adm1 = paste0("NAME_",1),
                                            by.adm2 = paste0("NAME_",1))
    
    res_adm <- tryCatch({
      surveyPrev::directEST(data = analysis_dat,
                            cluster.info = cluster.info,
                            admin = 0,
                            strata = "all",
                            alt.strata = 'v022')
    }, error = function(e) {
      tryCatch({
        surveyPrev::directEST(data = analysis_dat,
                              cluster.info = cluster.info,
                              admin = 0,
                              strata = "all",
                              alt.strata = NULL)
      }, error = function(e) {
        NULL
      })
    })
    
    if (!is.null(res_adm)) {
      AnalysisInfo$Natl_res(res_adm$res.admin0)
    }
    
    TRUE
    
  }, error = function(e) {
    message("Error preparing analysis dataset: ", e$message)
    FALSE
  })
  
  session$sendCustomMessage('controlSpinner', list(action = "hide"))
  
  return(success)
}