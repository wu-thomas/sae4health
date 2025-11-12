#' dat_input_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
###############################################################
###  Given a country, find the available surveys (years)
###############################################################

ref_tab_all <- surveyPrev::indicatorList

############################################
## Load meta data and DHS API estimates (no R6, no reactive)
############################################


get_survey_year <- function(country=NULL){
  if(is.null(country)){return(NULL)}
  surveys <-get("DHS.survey.meta", envir = parent.frame())
  nrow(surveys)

  # To see the structure of the returned surveys data frame
  country_svy <- surveys[surveys$CountryName == country, ]
  country_svy_years <- unique(country_svy$SurveyYear)
  if(length(country_svy_years)==0){return(NULL)}
  return(country_svy_years)
}


###############################################################
###  Given a country, year and recode, find survey ID
###############################################################


#tmp.list.file <- find_DHS_dat_name(country= 'Malawi',
#                                    svy_year = 2015,
#                                    recode='Individual Recode')



find_DHS_dat_name <- function(country,svy_year,
                              recode){
  DHS.country.meta <- get("DHS.country.meta", envir = parent.frame())  
  DHS.dataset.meta <- get("DHS.dataset.meta", envir = parent.frame())                            
  tryCatch({
    DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == country,]$DHS_CountryCode

    if(!recode== 'Geographic Data'){
      survey_list <- DHS.dataset.meta %>% dplyr::filter(
                       FileFormat=='Stata dataset (.dta)' &
                       DHS_CountryCode == DHS_country_code &
                       SurveyYear == svy_year&
                       FileType == recode)


    #survey_list <- rdhs::dhs_datasets(countryIds =DHS_country_code, surveyYear = svy_year)%>%
    #  dplyr::filter( FileFormat=='Stata dataset (.dta)')%>%
    #  dplyr::filter(FileType == recode)
    }else{
      survey_list <- DHS.dataset.meta %>% dplyr::filter(
          DHS_CountryCode == DHS_country_code &
          SurveyYear == svy_year&
          FileType == recode)
      #survey_list <- rdhs::dhs_datasets(countryIds =DHS_country_code, surveyYear = svy_year)%>%
      #dplyr::filter( FileType== recode)

    }

    if(dim(survey_list)[1]==0){
      return('Not available')
    }else{
    return(survey_list$FileName)}

  }, error = function(e) {
    # What to do in case of error
    return(NULL)
  })
}




######################################################################
###  Process downloaded DHS .zip (in a single file), for each recode
######################################################################


find_recode_path <- function(recode_file=NULL,
                             file_path=NULL,
                             extensions='DTA'){
  tryCatch({
    ### utils::unzip
    temp <- tempfile()
    utils::unzip(file_path, exdir = temp)

    ### specify the pattern according to DHS data naming system
    recode_file_prefix <- unlist(strsplit(recode_file,  '[.]'))[1]


    ### find the subfolder has the DHS data set name
    allPaths <- list.files(temp, pattern =  recode_file_prefix, full.names = TRUE, recursive = T, include.dirs = TRUE,
                           ignore.case = TRUE)

    validPaths <- allPaths[sapply(allPaths, function(x) file.info(x)$isdir)]

    #dirPaths <- allPaths[sapply(allPaths, function(x) file.info(x)$isdir)][1]
    if (length(validPaths) > 0) {
      dirPaths <- validPaths[which.min(nchar(validPaths))]
    } else {
      dirPaths <- NULL  # If no matching directories are found
    }



    files_list <- lapply(extensions, function(ext) {
      # Create regex pattern for the current extension
      pattern <- paste0(gsub("\\.", "\\\\.", ext), "$") # Escape dot for regex
      list.files(dirPaths, recursive = TRUE, pattern = pattern, full.names = TRUE,ignore.case = TRUE)
    })

    # Flatten the list and remove empty elements
    files <- unlist(files_list)
    files <- files[files != ""]

    if (length(files) < 1) {
      return(NULL)
    }

    return(files[1])

  }, error = function(e) {
    # What to do in case of error
    return(NULL)
  })

}

#country= CountryInfo$country()
#svy_year = CountryInfo$svyYear_selected()
#recode_dat_list=recode_for_ind_names()

#find_DHS_dat_name('Zambia',2018,recode='Geographic Data')
#find_DHS_dat_name('Zambia',2018,recode='Individual Recode')


#file_prefix <- find_DHS_dat_name('Kenya','2022',recode ='Births Recode')
#tmp_path <-'C:/Users/wu-th/Downloads/KE_2022_DHS_05082024_189_143411.zip'
#tmp_path <- 'E:/Dropbox/YunhanJon/JonData/KE_2022_DHS_05082024_1428_132518.zip'
#find_recode_path(recode_file=file_prefix,file_path=tmp_path,extensions='DTA')




###############################################################
###  load GADM files
###############################################################

get_country_GADM <- function(country,resolution=1) {
  DHS.country.meta <- get("DHS.country.meta", envir = parent.frame())
  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==country,'ISO3_CountryCode']

  gadm_list <- list()
  levels <- 0
  repeat {
    tmp.gadm <- geodata::gadm(country = country_iso3, resolution=resolution,
                              level = levels,
                              path = tempdir())
    if (is.null(tmp.gadm)) {
      break
    } else {
      tmp.gadm <- sf::st_as_sf(tmp.gadm)
      tmp.gadm <- sf::st_set_crs(tmp.gadm, 4326)

      n_region <- dim(tmp.gadm)[1]
      #message(paste0('n region: ',n_region))
      if(n_region >1000){break}


      if(levels==0){      gadm_list[['National']]  <- tmp.gadm
}else{
      gadm_list[[paste0('Admin-',levels)]]  <- tmp.gadm}
      levels <- levels + 1
    }
  }

  if(country=='Nigeria'){
    tmp_adm2 <- gadm_list[['Admin-2']]
    tmp_adm2=tmp_adm2[tmp_adm2$ENGTYPE_2=="Local Authority",]
    gadm_list[['Admin-2']]<- tmp_adm2
  }


  return(gadm_list)
}



###############################################################
###  load shapefile depending on source
###############################################################

get_country_shapefile <- function(country,source=NULL,...) {
  DHS.country.meta <- get("DHS.country.meta", envir = parent.frame())
  if(country=='Sierra Leone'&&(source!='WHO-download')){
    source ='WHO-preload'
  }

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==country,'ISO3_CountryCode']

  if(is.null(source)){source='GADM-download'}


  #################
  ### WHO preload
  #################
  if(source =='WHO-preload'){
  #
    WHO_shp_path <- system.file("WHO_shp", country_iso3, paste0(country_iso3,"_shp.rds"),
                                package = "sae4health")

    if(WHO_shp_path==''){
      message('No WHO shapefile, use GADM instead.')
      source <- 'GADM-preload'

    }else{

      message('Loading WHO shapefile.')

      country_shp <- readRDS(file=WHO_shp_path)
      country_shp <- lapply(country_shp, function(x) {
        sf::st_set_crs(x, 4326)
      })

      country_shp_analysis <- country_shp
      country_shp_smoothed <- country_shp
    }

  }

  #################
  ### WHO download
  #################
  if(source =='WHO-download'){

    WHO_shp_prepared <- tryCatch({
      prepare_WHO_country_shp(country.ISO3=country_iso3,...
                                            )
    },error = function(e) {
      message(e$message)
      return(NULL)
    })

    if(is.null(WHO_shp_prepared)){
      message('No WHO shapefile, use GADM instead.')
      source <- 'GADM'

    }else{

      message('Loading WHO shapefile.')

      country_shp_analysis <- WHO_shp_prepared
      country_shp_smoothed <- WHO_shp_prepared
    }




  }


  #################
  ### GADM
  #################

  if(source %in% c('GADM-preload','GADM-download')){

    ### check whether raw (for analysis) and smoothed (for display) shapefile exists

    GADM_analysis_shp_path=''
    GADM_display_shp_path=''

    if(source =='GADM-preload'){

      GADM_analysis_shp_path <- system.file("GADM_shp", country_iso3, paste0(country_iso3,"_GADM_analysis.rds"),
                                            package = "sae4health")
      GADM_display_shp_path <- system.file("GADM_shp", country_iso3, paste0(country_iso3,"_GADM_display.rds"),
                                           package = "sae4health")
    }

    ### check whether preloaded raw shapefile exists

    if(GADM_analysis_shp_path==''){
      message('No preloaded GADM shapefile for analysis, downloading from source.')
      country_shp_analysis <- get_country_GADM(country)

    }else{

      message('loading prepared GADM shapefile for analysis.')
      country_shp_analysis <- readRDS(file=GADM_analysis_shp_path)
      country_shp_analysis <- lapply(country_shp_analysis, function(x) {
        sf::st_set_crs(x, 4326)
      })

    }

    ### check whether preloaded smoothed shapefile exists
    if(GADM_display_shp_path==''){
      message('No preloaded GADM shapefile for display, downloading from source.')
      country_shp_smoothed <- get_country_GADM(country,resolution=2)

    }else{

      message('loading prepared GADM shapefile for analysis.')
      country_shp_smoothed <- readRDS(file=GADM_display_shp_path)
      country_shp_smoothed <- lapply(country_shp_smoothed, function(x) {
        sf::st_set_crs(x, 4326)
      })

    }


  }


  return.obj <- list('country_shp_analysis'=country_shp_analysis,
                     'country_shp_smoothed'=country_shp_smoothed)

}


#tmp.mdg <- get_country_shapefile(country='Zambia',source='GADM-download')

###############################################################
###  check number of regions at each admin level
###############################################################

check_gadm_levels <- function(gadm_list) {
  if(is.null(gadm_list)){
    df <- data.frame(Admin_Level=c('National','Admin-1','Admin-2'),
                     Num_Regions=c(1,NA,NA))

    row.names(df) <- NULL
    colnames(df) <- c("Admin Level", "Number of Regions")

    transposed_df <- as.data.frame(t(df))

    colnames(transposed_df) <- transposed_df[1, ]
    transposed_df <- transposed_df[-1, ]

    return(transposed_df)
  }


  df <- data.frame(
    Admin_Level = names(gadm_list),
    Num_Regions = sapply(gadm_list, function(x) nrow(x))
  )

  #df$Num_Regions <- as.numeric(as.character(df$Num_Regions))

  df$Admin_Level <- ifelse(df$Admin_Level == "Admin-0", "National", df$Admin_Level)
  row.names(df) <- NULL
  colnames(df) <- c("Admin Level", "Number of Regions")

  transposed_df <- as.data.frame(t(df))

  colnames(transposed_df) <- transposed_df[1, ]
  transposed_df <-  transposed_df[-1, , drop = FALSE]
  transposed_df[] <- lapply(transposed_df, function(x) as.integer(as.character(x)))

  return(transposed_df)
}





###############################################################
###  check whether survey support indicator calculation
###############################################################

#tmp.check <- check_dat_avail(country = 'Zambia' , svy_year = 2018 , indicator = 'HA_HIVP_B_HIV')
#tmp.check <- check_dat_avail(country = 'Kenya' , svy_year = 2022 , indicator = 'HA_HIVP_B_HIV')
#tmp.check <- check_dat_avail(country = 'Zambia' , svy_year = 1992 , indicator = 'WS_SRCE_P_BAS')

check_dat_avail <- function(country,svy_year,indicator){
  DHS.country.meta <- get("DHS.country.meta", envir = parent.frame())
  DHS.dataset.meta <- get("DHS.dataset.meta", envir = parent.frame())
  ### initialize all recode names

  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')

  recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                         "Births Recode","Household Recode","Men's Recode",
                         "HIV Test Results Recode","Couples' Recode")

  return.obj <- list()

  ### find country code
  DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == country,]$DHS_CountryCode

  ### determine needed  recode names
  needed_recode <- recode_list_names[which(ref_tab_all[ref_tab_all$ID==indicator,
                                                        recode_list_abbrev]==T)]
  needed_recode_abbrev <- recode_list_abbrev[which(ref_tab_all[ref_tab_all$ID==indicator,
                                                                recode_list_abbrev]==T)]

  return.obj[['needed_recode']] <- needed_recode
  return.obj[['needed_recode_abbrev']] <- needed_recode_abbrev

  ### check whether any is missing
  survey_list <- DHS.dataset.meta%>%
    dplyr::filter( FileFormat=='Stata dataset (.dta)' &
                     DHS_CountryCode == DHS_country_code &
                     SurveyYear == svy_year)

  available_recode <-survey_list$FileType

  return.obj[['missing_recode']] <- needed_recode[!needed_recode %in% available_recode]


  return(return.obj)

}


###############################################################
###  recode abbreviation to full name
###############################################################


# Create a function to map abbreviations to names
get_recode_names <- function(abbreviations) {
  recode_list_abbrev <- c('IR', 'PR', 'KR', 'BR', 'HR', 'MR', 'AR', 'CR')
  recode_list_names <- c("Individual Recode", "Household Member Recode", "Children's Recode",
                         "Births Recode", "Household Recode", "Men's Recode",
                         "HIV Test Results Recode", "Couples' Recode")
  # Create a named vector
  name_lookup <- stats::setNames(recode_list_names, recode_list_abbrev)

  # Return the names corresponding to the input abbreviations
  return(name_lookup[abbreviations])
}


###############################################################
###  function to measure API response time
###############################################################

# if(FALSE){
# # Example function to measure API response time
# measure_response_time <- function() {
#   start_time <- Sys.time()
#   # Example API call using rdhs
#   all.country <- rdhs::dhs_countries()
#   all.survey <- rdhs::dhs_surveys()
#
#
#   end_time <- Sys.time()
#   response_time <- end_time - start_time
#   return(as.numeric(response_time, units = "secs"))
# }
#
# response_time <- measure_response_time()
# }







