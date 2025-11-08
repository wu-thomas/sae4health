#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


options(shiny.maxRequestSize=150*1024^2) ## make the maximum size 150Mb for data input

app_server <- function(input, output, session) {


  #############################################################
  #### setup global options via R6 object
  #############################################################

  shinyjs::useShinyjs()

  ### initialize R6 objects
  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()
  meta_list <- load_meta_data()
  
  ### home page for the website
  CountryInfo$website_link('https://sae4health.stat.uw.edu')


  ### whether to be deployed on the server
  CountryInfo$server_version(!(is.null(golem::get_golem_options()$server_link)))


  ### WHO/non-WHO version related parameters
  # determine which version will be deployed
  app_deploy_version <- golem::get_golem_options()$version
  if(is.null(app_deploy_version)){
    app_deploy_version = 'DHS-General'
  }

  if(!app_deploy_version %in% c('DHS-General','DHS-WHO')){
    stop('Currently not supporting ',app_deploy_version,' version of the app.')
  }else{
    message("Deploying '" ,app_deploy_version,"' version of the app.")
  }

  # DHS-general version
  if(app_deploy_version=='DHS-General'){
    CountryInfo$WHO_version(F)
    CountryInfo$use_basemap('OSM')
    CountryInfo$shapefile_source('GADM-preload')
    #CountryInfo$shapefile_source('GADM-download')
  }


  # DHS-WHO version
  if(app_deploy_version=='DHS-WHO'){
    CountryInfo$WHO_version(T)
    CountryInfo$use_basemap('None')
    CountryInfo$shapefile_source('WHO-download') ### docker/R package
    #CountryInfo$shapefile_source('WHO-preload') ### server
  }


  ### other parameters for this version of the app

  #CountryInfo$legend_color_reverse(T)
  leaflet_color_reverse_ind <- identical(golem::get_golem_options()$legend_color_reverse, TRUE)
  if(leaflet_color_reverse_ind){message('Reversing color scales in leaflet plots.')}
  CountryInfo$legend_color_reverse(!leaflet_color_reverse_ind)

  CountryInfo$use_preloaded_Zambia(F)
  CountryInfo$use_preloaded_Madagascar(F)


  ### load modules
  mod_landing_page_server("landing_page_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_country_specify_server("country_specify_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_survey_dat_input_server("survey_dat_input_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_model_selection_server("model_selection_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_result_tabulate_server("result_tabulate_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_report_preparation_server("report_preparation_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)

  mod_res_visual_prev_map_server("res_visual_prev_map_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_res_visual_multiple_maps_server("res_visual_multiple_maps_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)

  mod_res_visual_scatter_server("res_visual_scatter_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)
  mod_res_visual_ridge_server("res_visual_ridge_1",CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo)

  mod_indicator_in_app_server("indicator_in_app_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_indicator_dictionary_server("indicator_dictionary_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)
  mod_DHS_API_est_server("DHS_API_est_1", CountryInfo = CountryInfo,AnalysisInfo=AnalysisInfo,parent_session=session)

  ### Hide/show Data Upload tab based on server version
is_server_version <- !(is.null(golem::get_golem_options()$server_link))

observe({
  # Only send message if NOT server version (i.e., manual mode)
  if(!is_server_version) {
    session$sendCustomMessage('show_data_upload_tab', list())
  }
})

}
