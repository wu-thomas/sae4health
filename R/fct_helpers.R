#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import R6
#'
#' @noRd
#'
#'
#'
###############################################################
###  meta data of countries
###############################################################

MetaInfo <- R6::R6Class("MetaInfo",
  public = list(
    DHS.country.meta = NULL,
    DHS.survey.meta  = NULL,
    DHS.dataset.meta = NULL,
    DHS_api_est      = NULL,
    
    initialize = function() {
      self$DHS.country.meta <- reactiveVal(NULL)
      self$DHS.survey.meta  <- reactiveVal(NULL)
      self$DHS.dataset.meta <- reactiveVal(NULL)
      self$DHS_api_est      <- reactiveVal(NULL)
    }
  )
)


###############################################################
###  country meta info and data
###############################################################

CountryInfo <- R6::R6Class(
  "CountryInfo",
  public = list(

    use_preloaded_Zambia = NULL,
    use_preloaded_Madagascar = NULL,

    website_link = NULL,

    use_basemap = NULL,
    WHO_version = NULL, ### whether to use WHO version of the app (difference in countries)
    shapefile_source = NULL, ### WHO-preload, WHO-download, GADM-preload, GADM-download

    server_version = NULL, ### whether this is a version deployed on the server
    legend_color_reverse = NULL, ### whether to reverse the color scheme on the legend of leaflet plot (fix bug)

    country = NULL,
    country_code_DHS = NULL,
    svyYear_list = NULL,
    svyYear_selected = NULL,


    display_interactive = NULL,

    GADM_list = NULL,
    GADM_list_smoothed = NULL,



    GADM_display_selected = NULL,
    GADM_display_selected_level=NULL,
    GADM_analysis_levels=NULL,
    GADM_strata_level=NULL,

    svy_indicator_var = NULL,
    svy_indicator_des = NULL,

    svy_dat_list = NULL,
    svy_GPS_dat = NULL,

    svy_analysis_dat = NULL,




    initialize = function() {

      self$use_preloaded_Zambia <- reactiveVal(NULL)
      self$use_preloaded_Madagascar <- reactiveVal(NULL)

      self$website_link <- reactiveVal(NULL)

      self$use_basemap <- reactiveVal(NULL)
      self$WHO_version <- reactiveVal(NULL)
      self$shapefile_source <- reactiveVal(NULL)

      self$server_version <- reactiveVal(F)
      self$legend_color_reverse <- reactiveVal(NULL)

      self$country <- reactiveVal(NULL)
      self$country_code_DHS <- reactiveVal(NULL)

      self$svyYear_list <- reactiveVal(NULL)
      self$svyYear_selected <- reactiveVal(NULL)

      self$display_interactive <- reactiveVal(T)


      self$GADM_list <- reactiveVal(NULL)
      self$GADM_list_smoothed <- reactiveVal(NULL)

      self$GADM_display_selected <- reactiveVal(NULL)
      self$GADM_display_selected_level <- reactiveVal(NULL)
      self$GADM_analysis_levels <- reactiveVal(NULL)

      self$GADM_strata_level <- reactiveVal(1)

      self$svy_indicator_var <- reactiveVal(NULL)
      self$svy_indicator_des <- reactiveVal(NULL)

      self$svy_dat_list <-  reactiveVal(list(IR = NULL, PR = NULL, KR = NULL, BR = NULL, HR = NULL, MR = NULL, AR = NULL, CR = NULL))
      self$svy_GPS_dat <- reactiveVal(NULL)

      self$svy_analysis_dat <- reactiveVal(NULL)

    },

    reset_val = function() {

      #self$use_preloaded_Zambia(NULL)
      #self$use_preloaded_Madagascar(NULL)

      #self$use_basemap(NULL)

      self$country(NULL)

      self$svyYear_list(NULL)
      self$svyYear_selected(NULL)

      self$display_interactive(T)


      #self$GADM_list(NULL)
      #self$GADM_list_smoothed(NULL)

      #self$GADM_display_selected(NULL)
      #self$GADM_display_selected_level(NULL)
      #self$GADM_analysis_levels(NULL)

      self$GADM_strata_level(1)

      #self$svy_indicator_var(NULL)
      #self$svy_indicator_des(NULL)

      self$svy_dat_list(list(IR = NULL, PR = NULL, KR = NULL, BR = NULL, HR = NULL, MR = NULL, AR = NULL, CR = NULL))
      self$svy_GPS_dat(NULL)

      self$svy_analysis_dat(NULL)

    },


    ### reset if change another indicator
    reset_dat = function() {

      self$svy_analysis_dat(NULL)

    },

    ### update survey data
    update_svy_dat = function(recode_abbrev, new_dat) {
      # Ensure modifications to svy_dat_list are reactive
      current_list <- self$svy_dat_list()  # Access current value
      if (recode_abbrev %in% names(current_list)) {
        current_list[[recode_abbrev]] <- new_dat  # Update the list
        self$svy_dat_list(current_list)  # Set the modified list
      }
    },


    ### check wheter the data for recodes exist
    check_svy_dat_upload = function(requiredRecodes,svy_dat) {
      sapply(requiredRecodes, function(recode) {
        is.null(svy_dat[[recode]])
      })
    }





  )
)








###############################################################
###  analysis object
###############################################################

AnalysisInfo <- R6::R6Class(
  "AnalysisInfo",

  public = list(

    country = NULL,
    GADM_list = NULL,


    cluster_admin_info_list = NULL,

    model_selection_mat = NULL,

    model_screen_list = NULL, ### screening check
    model_screen_ind_list = NULL, ### screening check

    Natl_res = NULL, ### separately store national results for additional usage
    model_res_list = NULL, ### fitted objects and results
    model_res_tracker_list = NULL, ### error message and success indicator

    ad_options_list = NULL, ### advanced modeling options, nested model etc.

    initialize = function() {

      self$country <- reactiveVal(NULL)
      self$GADM_list <- reactiveVal(NULL)


      self$cluster_admin_info_list <- reactiveVal(NULL)

      self$model_selection_mat <- reactiveVal(NULL)

      self$model_screen_list <- reactiveVal(NULL)
      self$model_screen_ind_list <- reactiveVal(NULL)

      self$Natl_res <- reactiveVal(NULL)
      self$model_res_list <- reactiveVal(NULL)
      self$model_res_tracker_list <- reactiveVal(NULL)

      self$ad_options_list <-  reactiveVal(list(nested=F,
                                                adm_cov_list = NULL))

    },


    reset_results = function() {

      self$cluster_admin_info_list(NULL)

      self$model_selection_mat(NULL)

      self$model_screen_list(NULL)
      self$model_screen_ind_list(NULL)

      self$Natl_res(NULL)
      self$model_res_list(NULL)
      self$model_res_tracker_list(NULL)

      self$ad_options_list(list(nested=F,adm_cov_list = NULL))

    },



    ### set cluster and admin info
    set_info_list = function(adm.level,info_obj) {

      tmp.list <- self$cluster_admin_info_list()

      tryCatch({
        tmp.list[[adm.level]] <- info_obj
      },error= function(e) {
        message(paste0('not succesful at: ',adm.level))
        return()})
      self$cluster_admin_info_list(tmp.list)


    },

    ### set screening results
    set_screen_Check = function(method,adm.level,screen_obj) {

      tmp.matrix <- self$model_screen_list()

      tryCatch({
        tmp.matrix[[method]][[adm.level]] <- screen_obj
      },error= function(e) {
        message(paste0('not succesful at ',method,':',adm.level))
        return()})
      self$model_screen_list(tmp.matrix)


    },

    ### set fitted models and results
    set_fitted_res = function(method,adm.level,model_res) {
      #message(dim(self$model_res_tracker_mat))

      tmp.list <- self$model_res_list()
      #message(paste0('trying at ',method,':',adm.level))

      tryCatch({
        tmp.list[[method]][[adm.level]] <- model_res
      },error= function(e) {
        message(paste0('not succesful at ',method,':',adm.level))
        return()})
      self$model_res_list(tmp.list)


    },


    ### set results tracker, i.e. whether model fitted successful
    set_track_res = function(method,adm.level,model_res) {
      #message(dim(self$model_res_tracker_mat))

      tmp.list <- self$model_res_tracker_list()
      #message(paste0('trying at ',method,':',adm.level))

      tryCatch({
        tmp.list[[method]][[adm.level]] <- model_res
      },error= function(e) {
        message(paste0('not succesful at ',method,':',adm.level))
        return()})
      self$model_res_tracker_list(tmp.list)


    },



    ### set advanced options selections
    set_ad_options = function(ad_option_name, choice_selected) {

      tmp.list <- self$ad_options_list()
      tryCatch({
        tmp.list[[ad_option_name]] <- choice_selected
      },error= function(e) {
        message(paste0('not succesful at ',ad_option_name,':',choice_selected))
        return()})
      self$ad_options_list(tmp.list)
    },

    ### get advanced options selections
    get_ad_options = function(ad_option_name) {
      tmp.list <- self$ad_options_list()
      return(tmp.list[[ad_option_name]])
    }


  )

)

