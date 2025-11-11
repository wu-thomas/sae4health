############################################
## load meta data and DHS api estimates to R6 class MetaInfo
############################################

load_meta_data <- function(MetaInfo) {
  server_link <- golem::get_golem_options()$server_link
  meta_files <- c("DHS_meta_preload", "DHS_api_est")
  
  read_one <- function(name, base_path) {
    tryCatch({
      path <- paste0(base_path, name, ".rds")
      readRDS(url(path))
      
    }, error = function(e) {
      message("Fail to load ", name, ": ", e$message)
      NULL
    })
  }
  
  base_url <- paste0(server_link, "DHS_survey_dat/DHS_meta_data/")
  message("Fetching meta info from server: ", base_url)
  
  meta_list <- lapply(meta_files, read_one, base_path = base_url)
  names(meta_list) <- meta_files
  
  DHS_api_est <- meta_list$DHS_api_est
  DHS.country.meta <- meta_list$DHS_meta_preload$DHS.country.meta
  DHS.survey.meta  <- meta_list$DHS_meta_preload$DHS.survey.meta
  DHS.dataset.meta <- meta_list$DHS_meta_preload$DHS.dataset.meta
  
  MetaInfo$DHS_api_est(DHS_api_est)
  MetaInfo$DHS.country.meta(DHS.country.meta)
  MetaInfo$DHS.survey.meta(DHS.survey.meta)
  MetaInfo$DHS.dataset.meta(DHS.dataset.meta)
}
