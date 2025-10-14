#' Load Survey Data from Server
#'
#' @description Loads DHS survey data from the server for a given country, year, and indicator
#'
#' @param CountryInfo R6 object containing country and survey information
#' @param session Shiny session object for sending spinner messages
#'
#' @return Number of new datasets loaded
#' @noRd
load_server_data <- function(CountryInfo, session) {
  if(is.null(golem::get_golem_options()$server_link)) {
    message("Server link not available - skipping server data load")
    return(0)
  }
  
  # Define recode lists
  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  
  # Get required recodes for current indicator
  required_recode <- recode_list_abbrev[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                          recode_list_abbrev]==T)]
  
  # Check if data already complete
  recode_status_check <- CountryInfo$check_svy_dat_upload(required_recode, CountryInfo$svy_dat_list())
  GPS_status_check <- is.null(CountryInfo$svy_GPS_dat())
  
  data.upload.complete <- (all(c(!recode_status_check, !GPS_status_check)))
  if(data.upload.complete){
    return(0)  # No new data to load
  }
  
  # Set parameters
  country_code = CountryInfo$country_code_DHS()
  svy_year = CountryInfo$svyYear_selected()
  
  new_dat_num <- 0
  
  ## Load survey recode data
  for (i in 1:length(required_recode)){
    
    # Check whether the data was already uploaded
    if(!CountryInfo$check_svy_dat_upload(required_recode[i], CountryInfo$svy_dat_list())){
      session$sendCustomMessage('controlSpinner', list(action = "show",
                                                       message = paste0(required_recode[i]," Recode already loaded, skip.")))
      Sys.sleep(0.5)
      session$sendCustomMessage('controlSpinner', list(action = "hide"))
      next
    }
    
    session$sendCustomMessage('controlSpinner', list(action = "show",
                                                     message = paste0("Loading ", required_recode[i]," Recode. Please wait...")))
    
    Sys.sleep(1)
    
    recode.path <- paste0(golem::get_golem_options()$server_link, '/DHS_survey_dat/',
                          country_code, '/DHS_', svy_year, '/',
                          required_recode[i], '.rds')
    
    recode.data <- tryCatch(
      {
        readRDS(url(recode.path))
      },
      error = function(e) {
        message("Failed to retrieve ", required_recode[i], ' from the server.')
        message("Error: ", e$message)
        
        session$sendCustomMessage('controlSpinner', list(action = "show",
                                                         message = paste0("Fail to retrieve ",
                                                                          required_recode[i],
                                                                          " recode from the server. Please contact us.")))
        Sys.sleep(2.5)
        NULL
      }
    )
    
    if(!is.null(recode.data)){
      CountryInfo$update_svy_dat(recode_abbrev=required_recode[i], new_dat=recode.data)
      new_dat_num <- new_dat_num + 1
    }
    
    session$sendCustomMessage('controlSpinner', list(action = "hide"))
  }
  
  ### Load survey GPS data
  if(is.null(CountryInfo$svy_GPS_dat())){
    
    session$sendCustomMessage('controlSpinner', list(action = "show",
                                                     message = paste0('Loading Geographic Data. Please wait...')))
    
    GPS.path <- paste0(golem::get_golem_options()$server_link, '/DHS_survey_dat/',
                       country_code, '/DHS_', svy_year, '/',
                       'Geographic_Data.rds')
    
    GPS.dat <- tryCatch(
      {
        readRDS(url(GPS.path))
      },
      error = function(e) {
        message("Failed to retrieve GPS data from the server.")
        message("Error: ", e$message)
        
        session$sendCustomMessage('controlSpinner', list(action = "show",
                                                         message = paste0("Fail to retrieve GPS data from the server. Please contact us.")))
        Sys.sleep(2.5)
        NULL
      }
    )
    
    Sys.sleep(1)
    
    if(!is.null(GPS.dat)){
      CountryInfo$svy_GPS_dat(GPS.dat)
      new_dat_num <- new_dat_num + 1
    }
    
    session$sendCustomMessage('controlSpinner', list(action = "hide"))
  }
  
  if(new_dat_num == 0){
    message('No new update found.')
    session$sendCustomMessage('controlSpinner', list(action = "show",
                                                     message = paste0('No other required recode found on the server for this indicator. ',
                                                                      "Please contact us if there is any recode missing.")))
    Sys.sleep(3)
    session$sendCustomMessage('controlSpinner', list(action = "hide"))
  }
  
  return(new_dat_num)
}