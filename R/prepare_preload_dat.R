
### compress data
# tools::resaveRdaFiles("data/")

###############################################################
### load DHS meta data
###############################################################
ref_tab_all <- surveyPrev::indicatorList
DHS_api_timeout = F

# if(FALSE){
# tryCatch({
#     R.utils::withTimeout({
#
#       DHS.country.meta <- rdhs::dhs_countries()
#       DHS.survey.meta <- rdhs::dhs_surveys()
#       DHS.dataset.meta <- rdhs::dhs_datasets()
#       #Sys.sleep(20)  # Simulating a delay
#
#       message('DHS API is working fine. Use most up-to-date info.')
#     },
#     timeout = 3)  # Timeout in seconds
#     },
#     TimeoutException = function(ex) {
#
#       message("DHS API call timed out, using backup data.")
#       DHS_api_timeout = T
#
#       ### use backup
#       pkg_files <- list.files('data')
#       filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'
#
#       load(file=paste0('data/',filtered_files))
#
#
#       DHS.country.meta <- DHS.country.meta.preload
#       DHS.survey.meta <- DHS.survey.meta.preload
#       DHS.dataset.meta <- DHS.dataset.meta.preload
#
#     },
#     error = function(e) {
#
#       message("Error loading 'rdhs' library: ", e$message)
#       DHS_api_timeout = T
#
#       ### use backup
#       pkg_files <- list.files('data')
#       filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'
#
#       load(file=paste0('data/',filtered_files))
#
#       DHS.country.meta <- DHS.country.meta.preload
#       DHS.survey.meta <- DHS.survey.meta.preload
#       DHS.dataset.meta <- DHS.dataset.meta.preload
#
# })
# }
### use pre-stored meta data
# pkg_files <- list.files('data')
# filtered_files <- grep("^DHS_meta_preload_", pkg_files, value = TRUE)# Filter files that start with 'DHS_meta_preload_'
#
# load(file=paste0('data/',filtered_files))
# #
# DHS.country.meta <- DHS.country.meta.preload
# DHS.country.meta[DHS.country.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
# DHS.country.meta[DHS.country.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'
#
#
# DHS.survey.meta <- DHS.survey.meta.preload
# DHS.survey.meta[DHS.survey.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
# DHS.survey.meta[DHS.survey.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'
#
# DHS.dataset.meta <- DHS.dataset.meta.preload
# DHS.dataset.meta[DHS.dataset.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
# DHS.dataset.meta[DHS.dataset.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'

#message(list.files('data'))
#message(list.files('data/GADM_shp'))
#load('data/GADM_shp/zmb_example_GADM.rda')


###############################################################
### prepare preloaded DHS meta data
###############################################################

### only executed when preparing data

if(FALSE){

  ### load dhs meta data
  DHS.country.listing <- rdhs::dhs_countries()
  DHS.survey.listing <- rdhs::dhs_surveys()
  DHS.dataset.listing <- rdhs::dhs_datasets()

  ##################################################
  ### Determine the surveys eligible for analysis
  ##################################################
  ### criteria: conducted later than 2000; is DHS standard survey (not MIS etc.); contains GPS data

  ### subset to >2000 and standard DHS
  DHS.survey.listing <- rdhs::dhs_surveys()
  DHS.survey.listing <- DHS.survey.listing[as.numeric(DHS.survey.listing$SurveyYear)>2000 & DHS.survey.listing$SurveyType== 'DHS',]

  ### check GPS data set availability
  check_GPS_avail <- function(surveyID){
    svy.tmp.list <- DHS.dataset.listing[DHS.dataset.listing$SurveyId==surveyID,]
    return( ('Geographic Data' %in% unique(svy.tmp.list$FileType)))
  }

  ### subset to surveys with GPS
  DHS.survey.listing$GPS_avail <-  sapply(DHS.survey.listing$SurveyId,check_GPS_avail)
  DHS.survey.listing <- DHS.survey.listing[DHS.survey.listing$GPS_avail==T,]

  DHS.survey.meta.preload <- DHS.survey.listing

  ##################################################
  ### Determine data sets according to surveys
  ##################################################

  DHS.dataset.meta.preload <- DHS.dataset.listing[DHS.dataset.listing$SurveyId %in%
                                                    unique(DHS.survey.meta.preload$SurveyId),]
  DHS.country.meta.preload <- DHS.country.listing[DHS.country.listing$CountryName %in%
                                                    unique(DHS.survey.meta.preload$CountryName),]

  #DHS.country.meta.preload <- rdhs::dhs_countries()
  #DHS.survey.meta.preload <- rdhs::dhs_surveys()
  #DHS.dataset.meta.preload <- rdhs::dhs_datasets()


  ### post process
  DHS.country.meta <- DHS.country.meta.preload
  DHS.country.meta[DHS.country.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
  DHS.country.meta[DHS.country.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'


  DHS.survey.meta <- DHS.survey.meta.preload
  DHS.survey.meta[DHS.survey.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
  DHS.survey.meta[DHS.survey.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'

  DHS.dataset.meta <- DHS.dataset.meta.preload
  DHS.dataset.meta[DHS.dataset.meta$CountryName=='Tanzania',]$CountryName <-'United Republic of Tanzania'
  DHS.dataset.meta[DHS.dataset.meta$CountryName=='Congo Democratic Republic',]$CountryName <-'Democratic Republic of the Congo'
  
  DHS.survey.meta <- DHS.survey.meta %>%
    filter(SurveyType == "DHS") %>%
    filter(as.integer(SurveyYear) > 1999)
  formatted_date <- format(Sys.Date(), "%m%d%Y")

  saveRDS(
    list(
      DHS.country.meta = DHS.country.meta,
      DHS.survey.meta  = DHS.survey.meta,
      DHS.dataset.meta = DHS.dataset.meta
    ),
    file = "C:/Users/lucyx/Desktop/DHS/DHS_meta_data/DHS_meta_preload.rds"
  )


}

###############################################################
### prepare preloaded GADM shapefile
###############################################################


if(FALSE){

  all_country_names <- sort(DHS.country.meta[['CountryName']])

  for(country in all_country_names){
    #country = 'Ethiopia'
   #country='Armenia'
    message(paste0('working on ',country))

    country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==country,'ISO3_CountryCode']

    #country_iso3='ARM'
    dir.create(file.path('data/GADM_shp', country_iso3), showWarnings = FALSE)

    if(length(list.files(paste0('data/GADM_shp/', country_iso3)))==2){
      message(paste0(country,' already downloaded, skip.'))
      next}

    ### store fine level GADM for analysis (neighborhood structure etc.)
    one_country_GADM <- get_country_GADM(country,resolution=1)

    adm_region_nums <- check_gadm_levels(one_country_GADM)

    ### only keep admin levels that have less than 1000 regions
    model_levels <- names(adm_region_nums[,adm_region_nums<1000])

    country.GADM.list.fine <- one_country_GADM[model_levels]

    #saveRDS(country.GADM.list.fine,file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))

    ### store smoothed GADM for display
    one_country_GADM <- get_country_GADM(country,resolution=2)

    adm_region_nums <- check_gadm_levels(one_country_GADM)
    model_levels <- names(adm_region_nums[,adm_region_nums<1000])

    country.GADM.list.smoothed <- one_country_GADM[model_levels]

    #saveRDS(country.GADM.list.smoothed,file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))
  }

}





###############################################################
### process WHO shapefile
###############################################################

WHO.app.countries <- c('Benin', 'Burkina Faso',
                       "Democratic Republic of the Congo",
                       'Rwanda', 'Senegal', 'Sierra Leone',
                       'United Republic of Tanzania','Zambia')


if(FALSE){
### WHO requested countries
WHO.app.countries.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName %in% WHO.app.countries,]$ISO3_CountryCode


### save linkage file for WHO boundaries across admin levels

  #adm2.link.all <- read.csv("/WHO_admin_names.csv", fileEncoding = "Windows-1252")
  adm2.link.all <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE %in% WHO.app.countries.ISO3,]
  #save(adm2.link.all,file='WHO_shp_linkage.rda')

}


### function to read WHO shapefiles and constrain to WHO countries
read_WHO_shp <- function(file_path,adm_level=0){
  WHO.app.countries.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName %in% WHO.app.countries,]$ISO3_CountryCode

  temp <- tempfile()
  utils::unzip(file_path, exdir = temp)

  file_name <- paste0('GLOBAL_ADM',adm_level,'.shp')
  allPaths <- list.files(temp, #pattern =  '(GLOBAL_ADM0.shp|GLOBAL_ADM1.shp|GLOBAL_ADM2.shp)',
                         pattern = file_name,
                         full.names = TRUE, recursive = T, include.dirs = TRUE,
                         ignore.case = TRUE)

  if (length(allPaths) > 0) {
    shp_Paths <- allPaths[which.min(nchar(allPaths))]
  } else {
    shp_Paths <- NULL  # If no matching directories are found
    return(NULL)
  }

  WHO.shp = NULL
  tryCatch({
    WHO.shp <- suppressWarnings(sf::st_read(shp_Paths))
    WHO.shp <- WHO.shp[WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]

    WHO.shp <- sf::st_as_sf(WHO.shp)
    WHO.shp <- sf::st_transform(WHO.shp,4326)
  },error = function(e) {
    message(e$message)
    return(NULL)
  })



  return(WHO.shp)

}

### save national (only national boudaries can be stored within this version of the app)

if(FALSE){
  #natl.WHO.shp <- read_WHO_shp(adm_level=0,
  #  file_path ='C:/Users/wu-th/Downloads/Detailed_Boundary_ADM0_565521753006392799.zip')

  #save(natl.WHO.shp,file='natl_WHO_shp.rda')

  #adm1.WHO.shp <- read_WHO_shp(adm_level=1,
  #  file_path ='C:/Users/wu-th/Downloads/Detailed_Boundary_ADM1_-6406508520251409053.zip')

  #save(adm1.WHO.shp,file='adm1_WHO_shp.rda')

  #adm2.WHO.shp <- read_WHO_shp(adm_level=2,
  #  file_path = 'C:/Users/wu-th/Downloads/Detailed_Boundary_ADM2_1272841991903683246.zip')
  #save(adm2.WHO.shp,file='adm2_WHO_shp.rda')


}


### Function to produce country specific shapefile
prepare_WHO_country_shp <- function(country.ISO3,
                                    natl.WHO.shp,
                                    adm1.WHO.shp,
                                    adm2.WHO.shp){

  return.shp.list <- list()

  ### initialize
  #country.ISO3 <-  DHS.country.meta[DHS.country.meta$CountryName==country.fullname,'ISO3_CountryCode']
  country.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE==country.ISO3,]


  ### national shapefile
  natl.GUID <- unique(country.adm2.link$GUID.LEVEL.0)
  natl.GUID <-  gsub("^\\{|\\}$", "", natl.GUID)
  country.natl.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(natl.WHO.shp$GUID) %in% natl.GUID,]

  if(dim(country.natl.shp)[1]>0){

    adm0.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','GUID.LEVEL.0')])
    country.natl.shp$NAME_0 <- adm0.name.ref.list$ADM0_VIZ_NAME[1]

    return.shp.list[['National']]<-country.natl.shp

  }else{return(NULL)}



  ### Admin-1 shapefile
  adm1.GUID <- unique(country.adm2.link$GUID.LEVEL.1..)
  adm1.GUID <-  gsub("^\\{|\\}$", "", adm1.GUID)

  country.adm1.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(adm1.WHO.shp$GUID) %in% adm1.GUID,]

  adm1.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME','GUID.LEVEL.1..')])
  adm1.name.ref.list$GUID.LEVEL.1.. <-  gsub("^\\{|\\}$", "", adm1.name.ref.list$GUID.LEVEL.1..)

  adm1.match.order <- match(toupper(country.adm1.shp$GUID),adm1.name.ref.list$GUID.LEVEL.1..)


  if(dim(country.adm1.shp)[1]>0){

    country.adm1.shp$NAME_0 <- adm1.name.ref.list$ADM0_VIZ_NAME[adm1.match.order]
    country.adm1.shp$NAME_1 <- adm1.name.ref.list$ADM1_VIZ_NAME[adm1.match.order]

    ### make valid for shapefile
    if(!all(sf::st_is_valid(country.adm1.shp))){

      wrong_row <- which(sf::st_is_valid(country.adm1.shp)==F)
      country.adm1.shp[wrong_row,] <- sf::st_make_valid(country.adm1.shp[wrong_row,])
      country.adm1.shp <- sf::st_as_sf(country.adm1.shp)
      country.adm1.shp <- sf::st_transform(country.adm1.shp,4326)

      message('Fixed invalid shapefile.')

    }

    return.shp.list[['Admin-1']]<-country.adm1.shp

  }else{return(return.shp.list)}


  ### Admin-2 shapefile
  adm2.GUID <- unique(country.adm2.link$GLOBAL.UNIQUE.IDENTIFIER..)
  adm2.GUID <-  gsub("^\\{|\\}$", "", adm2.GUID)

  country.adm2.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE==country.ISO3&
                                     toupper(adm2.WHO.shp$GUID) %in% adm2.GUID,]

  adm2.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME',
                                                    'ADM2_VIZ_NAME','GLOBAL.UNIQUE.IDENTIFIER..')])

  adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER.. <-  gsub("^\\{|\\}$", "", adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)

  adm2.match.order <- match(toupper(country.adm2.shp$GUID),adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)


  if(dim(country.adm2.shp)[1]>0){

    country.adm2.shp$NAME_0 <- adm2.name.ref.list$ADM0_VIZ_NAME[adm2.match.order]
    country.adm2.shp$NAME_1 <- adm2.name.ref.list$ADM1_VIZ_NAME[adm2.match.order]
    country.adm2.shp$NAME_2 <- adm2.name.ref.list$ADM2_VIZ_NAME[adm2.match.order]

    ### make valid for shapefile
    if(!all(sf::st_is_valid(country.adm2.shp))){

      wrong_row <- which(sf::st_is_valid(country.adm2.shp)==F)
      country.adm2.shp[wrong_row,] <- sf::st_make_valid(country.adm2.shp[wrong_row,])
      country.adm2.shp <- sf::st_as_sf(country.adm2.shp)
      country.adm2.shp <- sf::st_transform(country.adm2.shp,4326)

      message('Fixed invalid shapefile.')

    }

    ### fix wrong shapefile for congo, overlapping
    if(country.ISO3 =='COD'){

      country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry <-
        sf::st_difference(country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry ,
                          country.adm2.shp[country.adm2.shp$ADM2_NAME=='BILI2',]$geometry)

      message('Fixed overlapping shapefile.')

    }

    return.shp.list[['Admin-2']]<-country.adm2.shp


  }else{return(return.shp.list)}




  ### return shapefile list
  return(return.shp.list)

}


if(FALSE){
  tmp.cod.shp.list <- prepare_WHO_country_shp(country.fullname='Democratic Republic of the Congo',
                                              natl.WHO.shp=natl.WHO.shp,
                                              adm1.WHO.shp=adm1.WHO.shp,
                                              adm2.WHO.shp=adm2.WHO.shp)

}



###

# download link: https://gis-who.hub.arcgis.com/pages/detailedboundary





###############################################################
### prepare the WHO shapefiles for online version
###############################################################

if(FALSE){


  ### load linkage meta data
  #adm2.link.all <- read.csv("C:/Users/wu-th/Dropbox/YunhanJon/saeforhealth/WHO_admin_names.csv", fileEncoding = "Windows-1252")
  adm2.link.all <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE %in% WHO.app.countries.ISO3,]


  #######################
  ### process national
  #######################

  #natl.WHO.shp.path <- 'E:/rshiny_history/sae4health_0511/raw_data/Detailed_Boundary_ADM0/GLOBAL_ADM0.shp'
  natl.WHO.shp <- suppressWarnings(sf::st_read(natl.WHO.shp.path))
  natl.WHO.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]

  natl.WHO.shp <- sf::st_as_sf(natl.WHO.shp)
  natl.WHO.shp <- sf::st_transform(natl.WHO.shp,4326)

  #######################
  ### process Admin-1
  #######################

  #adm1.WHO.shp.path <- 'E:/rshiny_history/sae4health_0511/raw_data/Detailed_Boundary_ADM1/GLOBAL_ADM1.shp'
  adm1.WHO.shp <- suppressWarnings(sf::st_read(adm1.WHO.shp.path))
  adm1.WHO.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]

  adm1.WHO.shp <- sf::st_as_sf(adm1.WHO.shp)
  adm1.WHO.shp <- sf::st_transform(adm1.WHO.shp,4326)

  #######################
  ### process Admin-2
  #######################

  #adm2.WHO.shp.path <- 'E:/rshiny_history/sae4health_0511/raw_data/Detailed_Boundary_ADM2/GLOBAL_ADM2.shp'
  adm2.WHO.shp <- suppressWarnings(sf::st_read(adm2.WHO.shp.path))
  adm2.WHO.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE %in% WHO.app.countries.ISO3,]

  adm2.WHO.shp <- sf::st_as_sf(adm2.WHO.shp)
  adm2.WHO.shp <- sf::st_transform(adm2.WHO.shp,4326)

  #tmp.adm2.ctry <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE=='BFA',]
  #tmp.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE=='BFA',]


  country.fullname <- 'Burkina Faso'

  prepare_WHO_country_shp <- function(country.fullname){

    return.shp.list <- list()

    ### initialize
    country.ISO3 <-  DHS.country.meta[DHS.country.meta$CountryName==country.fullname,'ISO3_CountryCode']
    country.adm2.link <- adm2.link.all[adm2.link.all$ISO.3.DIGIT.COUNTRY.CODE==country.ISO3,]


    ### national shapefile
    natl.GUID <- unique(country.adm2.link$GUID.LEVEL.0)
    natl.GUID <-  gsub("^\\{|\\}$", "", natl.GUID)
    country.natl.shp <- natl.WHO.shp[natl.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(natl.WHO.shp$GUID) %in% natl.GUID,]

    if(dim(country.natl.shp)[1]>0){

      adm0.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','GUID.LEVEL.0')])
      country.natl.shp$NAME_0 <- adm0.name.ref.list$ADM0_VIZ_NAME[1]

      return.shp.list[['National']]<-country.natl.shp

    }else{return(NULL)}



    ### Admin-1 shapefile
    adm1.GUID <- unique(country.adm2.link$GUID.LEVEL.1..)
    adm1.GUID <-  gsub("^\\{|\\}$", "", adm1.GUID)

    country.adm1.shp <- adm1.WHO.shp[adm1.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(adm1.WHO.shp$GUID) %in% adm1.GUID,]

    adm1.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME','GUID.LEVEL.1..')])
    adm1.name.ref.list$GUID.LEVEL.1.. <-  gsub("^\\{|\\}$", "", adm1.name.ref.list$GUID.LEVEL.1..)

    adm1.match.order <- match(toupper(country.adm1.shp$GUID),adm1.name.ref.list$GUID.LEVEL.1..)


    if(dim(country.adm1.shp)[1]>0){

      country.adm1.shp$NAME_0 <- adm1.name.ref.list$ADM0_VIZ_NAME[adm1.match.order]
      country.adm1.shp$NAME_1 <- adm1.name.ref.list$ADM1_VIZ_NAME[adm1.match.order]

      ### make valid for shapefile
      if(!all(sf::st_is_valid(country.adm1.shp))){

        wrong_row <- which(sf::st_is_valid(country.adm1.shp)==F)
        country.adm1.shp[wrong_row,] <- sf::st_make_valid(country.adm1.shp[wrong_row,])
        country.adm1.shp <- sf::st_as_sf(tmp.adm)
        country.adm1.shp <- sf::st_transform(country.adm1.shp,4326)

        message('Fixed invalid shapefile.')

      }

      return.shp.list[['Admin-1']]<-country.adm1.shp

    }else{return(return.shp.list)}


    ### Admin-2 shapefile
    adm2.GUID <- unique(country.adm2.link$GLOBAL.UNIQUE.IDENTIFIER..)
    adm2.GUID <-  gsub("^\\{|\\}$", "", adm2.GUID)

    country.adm2.shp <- adm2.WHO.shp[adm2.WHO.shp$ISO_3_CODE==country.ISO3&
                                       toupper(adm2.WHO.shp$GUID) %in% adm2.GUID,]

    adm2.name.ref.list <- unique(country.adm2.link[,c('ADM0_VIZ_NAME','ADM1_VIZ_NAME',
                                                      'ADM2_VIZ_NAME','GLOBAL.UNIQUE.IDENTIFIER..')])

    adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER.. <-  gsub("^\\{|\\}$", "", adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)

    adm2.match.order <- match(toupper(country.adm2.shp$GUID),adm2.name.ref.list$GLOBAL.UNIQUE.IDENTIFIER..)


    if(dim(country.adm2.shp)[1]>0){

      country.adm2.shp$NAME_0 <- adm2.name.ref.list$ADM0_VIZ_NAME[adm2.match.order]
      country.adm2.shp$NAME_1 <- adm2.name.ref.list$ADM1_VIZ_NAME[adm2.match.order]
      country.adm2.shp$NAME_2 <- adm2.name.ref.list$ADM2_VIZ_NAME[adm2.match.order]

      ### make valid for shapefile
      if(!all(sf::st_is_valid(country.adm2.shp))){

        wrong_row <- which(sf::st_is_valid(country.adm2.shp)==F)
        country.adm2.shp[wrong_row,] <- sf::st_make_valid(country.adm2.shp[wrong_row,])
        country.adm2.shp <- sf::st_as_sf(tmp.adm)
        country.adm2.shp <- sf::st_transform(country.adm2.shp,4326)

        message('Fixed invalid shapefile.')

      }

      ### fix wrong shapefile for congo, overlapping
      if(country.ISO3 =='COD'){

      country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry <-
        sf::st_difference(country.adm2.shp[country.adm2.shp$ADM2_NAME=='BOSOBOLO',]$geometry ,
                          country.adm2.shp[country.adm2.shp$ADM2_NAME=='BILI2',]$geometry)
      }

      return.shp.list[['Admin-2']]<-country.adm2.shp


    }else{return(return.shp.list)}




    ### return shapefile list
    return(return.shp.list)

  }

  tmp.rwa.shp.list <- prepare_WHO_country_shp('Rwanda')

  ##################################
  ### save WHO country shapefiles
  ##################################


  for(WHO.country in WHO.app.countries){

    message(WHO.country)

    WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
    message(WHO.country.ISO3)

    ### make directory
    dir.create(file.path('data/WHO_shp', WHO.country.ISO3), showWarnings = FALSE)

    if(length(list.files(paste0('data/WHO_shp/', WHO.country.ISO3)))>0){
      message(paste0(country,' already downloaded, skip.'))
      next
    }

    ### prepare shapefile
    country.shp.list <- prepare_WHO_country_shp(country.fullname=WHO.country)


    ### save shapefile
    #saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))



  }


  ### make valid
  for(WHO.country in WHO.app.countries){

    # WHO.country = 'Democratic Republic of the Congo'
    message(WHO.country)

    WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
    message(WHO.country.ISO3)

    country.shp.list<- readRDS(file=paste0('data/WHO_shp/',WHO.country.ISO3,
                                           '/',WHO.country.ISO3,'_shp.rds'))

    for (tmp.name in names(country.shp.list)){

      tmp.adm <- country.shp.list[[tmp.name]]

      if(!all(sf::st_is_valid(tmp.adm))){

        tmp.adm <- sf::st_make_valid(tmp.adm)
        tmp.adm <- sf::st_as_sf(tmp.adm)
        tmp.adm <- sf::st_transform(tmp.adm,4326)

        country.shp.list[[tmp.name]] <- tmp.adm

        message('Country: ',WHO.country, ' ',tmp.name,' shapefile not valid.')

        #saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))

        message('Fixed.')

      }

    }

  }


  ### fix wrong shapefile for congo
  WHO.country <- 'Democratic Republic of the Congo'
  WHO.country.ISO3 <- DHS.country.meta[DHS.country.meta$CountryName==WHO.country,'ISO3_CountryCode']
  message(WHO.country.ISO3)

  country.shp.list<- readRDS(file=paste0('data/WHO_shp/',WHO.country.ISO3,
                                         '/',WHO.country.ISO3,'_shp.rds'))

  poly.adm2 <- country.shp.list[['Admin-2']]

  poly.adm2[poly.adm2$ADM2_NAME=='BOSOBOLO',]$geometry <-
    sf::st_difference(poly.adm2[poly.adm2$ADM2_NAME=='BOSOBOLO',]$geometry ,
                      poly.adm2[poly.adm2$ADM2_NAME=='BILI2',]$geometry)


  country.shp.list[['Admin-2']] <- poly.adm2
  #saveRDS(country.shp.list,file=paste0('data/WHO_shp/',WHO.country.ISO3,'/',WHO.country.ISO3,'_shp.rds'))


}




###############################################################
###  create DHS chapter Info
###############################################################

if(FALSE){

  dhs_chapters <- data.frame(
    Chapter = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
    Title = c("Housing Characteristics And Household Population",
              "Characteristics Of Respondents",
              "Marriage And Sexual Activity",
              "Fertility",
              "Fertility Preferences",
              "Family Planning",
              "Infant And Child Mortality",
              "Maternal Health",
              "Child Health",
              "Nutrition Of Children And Adults",
              "Malaria",
              "HIV/AIDS-Related Knowledge, Attitudes, And Behaviour",
              "HIV Prevalence",
              "Women's Empowerment",
              "Adult And Maternal Mortality",
              "Domestic Violence",
              "Female Genital Cutting",
              "Fistula"),
    Acronym = c("PH", "RC", "MS", "FE", "FF", "FP", "CM", "RH", "CH", "NT", "ML", "HK", "HV", "WE", "AM", "DV", "FG", "FS")
  )

}





###############################################################
###  create ref tab for newly added indicators
###############################################################


if(FALSE){

  #library(surveyPrev)
  #library(dplyr)

  #match_all_result <- surveyPrev::match_all_result


  match_all_result <- match_all_result %>% mutate(ID_first_two_letters = substr(indicator_ID_DHS, start = 1, stop = 2))

  match_all_result <- match_all_result %>%
    mutate(indicator_chapter = case_when( !is.na(indicator_chapter)~indicator_chapter,
                                          indicator_ID_DHS == 'HA_KAID_W_HRD' ~ 'Chap13_HK',
                                          indicator_ID_DHS == 'HA_STIS_W_STI' ~ 'Chap13_HK',
                                          indicator_ID_DHS == 'AN_NUTS_W_OVW' ~ 'Chap11_NT',
                                          ID_first_two_letters=='HC'~ 'Chap02_PH',
                                          ID_first_two_letters=='DV'~ 'Chap17_DV',
                                          ID_first_two_letters=='MA'~ 'Chap04_MS',
                                          TRUE~indicator_chapter)
                                          )%>%
    mutate(indicator_chapter = case_when(ID_first_two_letters=='WS'~ 'Chap02_PH',
                                         TRUE~indicator_chapter))%>%
    mutate(batch_recode_group= case_when(indicator_ID_DHS %in% c('FP_CUSM_W_MOD','FP_NADM_W_UNT',
                                                                 'FG_PFCC_W_WCC','FG_KFCC_W_HFC')~ 'IR',
                                         TRUE~batch_recode_group))

  Chap_abbrev <- sapply(match_all_result$indicator_chapter, function(x) strsplit(x, "_")[[1]][2], USE.NAMES=FALSE)

  ###  keep track of match_all_result from surveyPrev
  #save(match_all_result,file='data/match_all_result.rda')



  ID <- match_all_result$indicator_ID_DHS

  ref_tab_new <- data.frame(ID=ID,
                            #Description= Description,
                            #Full_definition=Full_definition,
                            Chap_abbrev= Chap_abbrev,
                            recode = match_all_result$batch_recode_group
  )

  #ref_tab_new$Chap_abbrev <- ifelse(is.na(ref_tab_new$Chap_abbrev), substr(ref_tab_new$ID, 1, 2), ref_tab_new$Chap_abbrev)


  ref_tab_new <- merge(ref_tab_new,dhs_chapters,by.x='Chap_abbrev',by.y='Acronym',all.x=T)
  ref_tab_new$Topic <- ref_tab_new$Title

  ref_tab_new$IR <- grepl('IR', ref_tab_new$recode)
  ref_tab_new$PR <- grepl('PR', ref_tab_new$recode)
  ref_tab_new$KR <- grepl('KR', ref_tab_new$recode)
  ref_tab_new$BR <- grepl('BR', ref_tab_new$recode)
  ref_tab_new$HR <- grepl('HR', ref_tab_new$recode)
  ref_tab_new$MR <- grepl('MR', ref_tab_new$recode)
  ref_tab_new$AR <- grepl('AR', ref_tab_new$recode)
  ref_tab_new$CR <- grepl('CR', ref_tab_new$recode)


  tmp_dict <- sae4health::DHS_ind_dictionary[,c('DHS Standard Indicator ID','Full Definition','Label')]
  tmp_dict$ID <- tmp_dict$`DHS Standard Indicator ID`
  tmp_dict$Full_definition <- tmp_dict$`Full Definition`
  tmp_dict$Description <- tmp_dict$Label

  tmp_dict <- tmp_dict[!duplicated(tmp_dict[,c('ID')]),]


  ref_tab_new <- merge(ref_tab_new,tmp_dict[,c('ID','Full_definition','Description')],by='ID',all.x=T)

  ref_tab_new <- ref_tab_new[,c('ID', "Description","Full_definition","Topic" , "Chap_abbrev",
                                "IR", "PR" ,"KR", "BR", "HR","MR", "AR", "CR")]

  ref_tab_new[ref_tab_new$ID=='CM_ECMR_C_NNF',]$Description <- 'Probability of dying in the first month of life in the five years preceding the survey'
  ref_tab_new[ref_tab_new$ID=='CM_ECMR_C_NNF',]$Full_definition <-'Probability of dying in the first month of life in the five or ten years preceding the survey, per 1,000 live births. Estimates are given for ten year periods for all characteristics, but for five year periods only for the national total, by residence, and by sex.'


  ref_tab_new[ref_tab_new$ID=='FP_CUSM_W_MOD',]$Description <- 'Current use of any modern method of contraception (married women)'
  ref_tab_new[ref_tab_new$ID=='FP_NADM_W_UNT',]$Description <- 'Unmet need for family planning (married women)'

  #ref_tab_new[ref_tab_new$ID=='FP_CUSM_W_MOD',]$Description <- 'Current use of any modern method of contraception (married women)'
  #ref_tab_new[ref_tab_new$ID=='FP_NADM_W_UNT',]$Description <- 'Unmet need for family planning (married women)'

  #save(ref_tab_new,file='data/indicator_list_new.rda')

  #recode_list <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  #recode_list[which(wide_format[7,recode_list]==T)]
  #recode_list[which(full_ind_des[full_ind_des$ID=='HA_HIVP_B_HIV',recode_list]==T)]
}


###############################################################
###  create ref tab for initial 22 indicators
###############################################################

if(FALSE){

  IR_Individual <- c( "RH_ANCN_W_N4P",  "AN_ANEM_W_ANY",
                      "FP_NADA_W_UNT", "FP_CUSA_W_MOD", "AN_NUTS_W_THN","HA_HIVP_B_HIV")
  PR_Household_Member <- c("CN_ANMC_C_ANY", "CN_NUTS_C_WH2", "CN_NUTS_C_HA2",
                           "WS_TLET_H_IMP", "WS_TLET_P_BAS",
                           "WS_SRCE_P_BAS")
  KR_Children <- c("CH_DIAT_C_ORT", "CH_VACC_C_DP3", "CH_VACC_C_DP1",
                   "CH_VACC_C_BAS", "CH_VACC_C_NON", "CN_BRFS_C_EXB", "CH_VACC_C_MSL"
  )
  BRdata_Birth <- c("RH_DELA_C_SKP", "CM_ECMR_C_NNR")
  HRdata_Household <- c("ML_NETP_H_IT2")

  MR_men <- c("HA_HIVP_B_HIV")
  AR_HIV<- c("HA_HIVP_B_HIV")
  CR_couple<- NA


  # Combine all indicators into a single vector and create a data frame
  all_indicators <- unique(c(IR_Individual, PR_Household_Member, KR_Children, BRdata_Birth, HRdata_Household,MR_men))
  wide_format <- data.frame(ID = all_indicators, stringsAsFactors = FALSE)

  # Create columns for each data type and check if the indicator belongs to that type
  wide_format$IR <- wide_format$ID %in% IR_Individual
  wide_format$PR <- wide_format$ID %in% PR_Household_Member
  wide_format$KR <- wide_format$ID %in% KR_Children
  wide_format$BR <- wide_format$ID %in% BRdata_Birth
  wide_format$HR <- wide_format$ID %in% HRdata_Household
  wide_format$MR <- wide_format$ID %in% MR_men
  wide_format$AR <- wide_format$ID %in% AR_HIV
  wide_format$CR <- wide_format$ID %in% CR_couple

  # merge back with the information data frame
  surveyPrev_ind_list <-  surveyPrev::surveyPrevIndicators
  #full_ind_des <- merge(surveyPrev_ind_list,wide_format,by='ID',all.x=T)
  full_ind_des[full_ind_des$ID=='FP_CUSA_W_MOD',]$Description <-  "Modern contraceptive prevalence rate (all women currently using any modern method of contraception)"

  #save(full_ind_des,file='indicator_list.rda')

  #recode_list <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  #recode_list[which(wide_format[7,recode_list]==T)]
  #recode_list[which(full_ind_des[full_ind_des$ID=='HA_HIVP_B_HIV',recode_list]==T)]
}


if(FALSE){

  ref_tab_22 <- sae4health::ref_tab_22

  #ref_tab_22$Full_definition <- ref_tab_22$Description
  ref_tab_22$Full_definition <- NULL


  ref_tab_22$Chap_abbrev <- c(rep('NT',times=2),
                              rep('CH',times=6),
                              rep('CM',times=1),
                              rep('NT',times=4),
                              rep('FP',times=2),
                              rep('HV',times=1),
                              rep('ML',times=1),
                              rep('RH',times=2),
                              rep('PH',times=3))

  ref_tab_22$Topic <- c(rep('Nutrition Of Children And Adults',times=2),
                        rep('Child Health',times=6),
                        rep('Infant And Child Mortality',times=1),
                        rep('Nutrition Of Children And Adults',times=4),
                        rep('Family Planning',times=2),
                        rep('HIV Prevalence',times=1),
                        rep('Malaria',times=1),
                        rep('Maternal Health',times=2),
                        rep('Housing Characteristics And Household Population',times=3))


  tmp_dict <- sae4health::DHS_ind_dictionary[,c('DHS Standard Indicator ID','Full Definition')]
  tmp_dict$ID <- tmp_dict$`DHS Standard Indicator ID`
  tmp_dict$Full_definition <- tmp_dict$`Full Definition`
  tmp_dict <- tmp_dict[!duplicated(tmp_dict[,c('ID')]),]

  ref_tab_22 <- merge(ref_tab_22,tmp_dict[,c('ID','Full_definition')],by='ID')

  ref_tab_22 <- ref_tab_22[,c('ID', "Description","Full_definition","Topic" , "Chap_abbrev",
                                "IR", "PR" ,"KR", "BR", "HR","MR", "AR", "CR")]



  ### modify labels
  ref_tab_22[ref_tab_22$ID=='FP_CUSA_W_MOD',]$Description <- 'Current use of any modern method of contraception (all women)'
  ref_tab_22[ref_tab_22$ID=='FP_NADA_W_UNT',]$Description <- 'Unmet need for family planning, total (all women)'



  #save(ref_tab_22,file='data/indicator_list_22.rda')

  #recode_list <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  #recode_list[which(wide_format[7,recode_list]==T)]
  #recode_list[which(full_ind_des[full_ind_des$ID=='HA_HIVP_B_HIV',recode_list]==T)]
}


###############################################################
###  merge new and old indicators
###############################################################

if(FALSE){
  library(surveyPrev)
}


if(FALSE){

  ref_tab_new_no_dup <- ref_tab_new[!ref_tab_new$ID %in% ref_tab_22$ID,]
  ref_tab_all <- rbind(ref_tab_22,ref_tab_new_no_dup)


  ref_tab_all <- merge(ref_tab_all,dhs_chapters,
                       by.x='Chap_abbrev',
                       by.y='Acronym',
                       all.x=T)

  #ref_tab_all <- ref_tab_all[ref_tab_all$Chap_abbrev!='DV',]


  ref_tab_all$Topic <- paste0('Chapter ',formatC(ref_tab_all$Chapter, width = 2, format = "d", flag = "0"),
                              ' - ',ref_tab_all$Title)

  #save(ref_tab_all,file='data/indicator_list_all.rda')

  #write.csv(ref_tab_all[,c(1:5)],row.names = F,file='indicator_list_0916.csv')
}



#######################################################################
###  prepare DHS estimates for all surveys for supported indicators
#######################################################################

if(FALSE){
  #dhs_survey_list <- rdhs::dhs_surveys()
  dhs_survey_list <- DHS.survey.meta
  dhs_survey_list <- dhs_survey_list[dhs_survey_list$SurveyYear>2000 & dhs_survey_list$SurveyType== 'DHS',]

  res_ind_supported <- data.frame()

  for (i in 1:dim(dhs_survey_list)[1]){
    #message(i)
    tmp_cty_code <- dhs_survey_list$DHS_CountryCode[i]
    tmp_svy_year <- dhs_survey_list$SurveyYear[i]
    #message(paste0(dhs_survey_list$DHS_CountryCode[i],dhs_survey_list$SurveyYear[i]))


    # cty_svy_res_file <- paste0('E:/Dropbox/YunhanJon/DHS-indicators/Step_3_Data/all_survey_est/',
    #                            tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda')
    cty_svy_res_file <- paste0('C:/Users/wu-th/Dropbox/YunhanJon/DHS-indicators/Step_3_Data/all_survey_est/',
                               tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda')

    if(!file.exists(cty_svy_res_file)){


      tmp_call <- paste0("https://api.dhsprogram.com/rest/dhs/data?countryIds=",tmp_cty_code,"&surveyYear=",tmp_svy_year,"&perpage=10000&f=csv")
      tmp_res <- read.csv(tmp_call)


      #message('done')
      #save(tmp_res,file=paste0('E:/Dropbox/YunhanJon/DHS-indicators/Step_3_Data/all_survey_est/',
      #                         tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda'))

    }else{

      load(cty_svy_res_file)
    }

    tmp_res <- tmp_res[tmp_res$IndicatorId %in% ref_tab_all$ID,  ]
    res_ind_supported <- rbind(res_ind_supported,tmp_res)

  }


  DHS_api_est <- res_ind_supported[,c('IndicatorId','Indicator','Value','DHS_CountryCode',
                                      'CountryName','SurveyYear','ByVariableLabel')]

  colnames(DHS_api_est) <- c('DHS Standard ID','Definition','Estimate','Country Code','Country','Survey Year','By Variable Label')
  DHS_api_est <- DHS_api_est[,c('Country','Country Code','Survey Year','DHS Standard ID','Definition','Estimate','By Variable Label')]
  save(DHS_api_est,file='data/new/DHS_api_est.rda')
  saveRDS(DHS_api_est,'C:/Users/lucyx/Desktop/DHS/DHS_meta_data/DHS_api_est.rds')


}



###############################################################
###  update shapefile for nigeria GADM
###############################################################
if(FALSE){
  #NGA_GADM_display <- readRDS("E:/Dropbox/YunhanJon/sae4health/inst/GADM_shp/NGA/NGA_GADM_display.rds")
  #NGA_GADM_analysis <- readRDS("E:/Dropbox/YunhanJon/sae4health/inst/GADM_shp/NGA/NGA_GADM_analysis.rds")

  tmp_adm2_display <- NGA_GADM_display[['Admin-2']]
  tmp_adm2_display=tmp_adm2_display[tmp_adm2_display$ENGTYPE_2=="Local Authority",]
  NGA_GADM_display[['Admin-2']]<- tmp_adm2_display


  tmp_adm2_analysis <- NGA_GADM_analysis[['Admin-2']]
  tmp_adm2_analysis=tmp_adm2_analysis[tmp_adm2_analysis$ENGTYPE_2=="Local Authority",]
  NGA_GADM_analysis[['Admin-2']]<- tmp_adm2_analysis

  #saveRDS(NGA_GADM_display,"E:/Dropbox/YunhanJon/sae4health/inst/GADM_shp/NGA/NGA_GADM_display.rds")
  #saveRDS(NGA_GADM_analysis,"E:/Dropbox/YunhanJon/sae4health/inst/GADM_shp/NGA/NGA_GADM_analysis.rds")

}

###############################################################
###  prepare Nigeria covariate
###############################################################

if(FALSE){

  NGA_adm2_cov <- Nigeria_Admin_2_cov_template
  NGA_adm2_cov <- cbind(NGA_adm2_cov,cmatA2[,c(6:11)]|> sf::st_drop_geometry())

  NGA_adm1_cov <- Nigeria_Admin_1_cov_template
  NGA_adm1_cov <- cbind(NGA_adm1_cov,cmatA1[,c(3:8)]|> sf::st_drop_geometry())

  readr::write_csv(NGA_adm1_cov, file='Nigeria_adm1_cov.csv')
  readr::write_csv(NGA_adm2_cov, file='Nigeria_adm2_cov.csv')

}
