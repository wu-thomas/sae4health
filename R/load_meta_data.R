load_meta_data <- function() {
  server_link <- golem::get_golem_options()$server_link
  meta_files <- c("DHS_meta_preload", "DHS_api_est")
  
  read_one <- function(name, base_path, from_server = FALSE) {
    tryCatch({
      path <- paste0(base_path, name, ".rda")
      e <- new.env()
      load(url(path), envir = e)
      obj_names <- ls(e)
      if (length(obj_names) == 1) {
        e[[obj_names]]
      } else {
        as.list(e)
      }
    }, error = function(e) {
      message("Fail to load ", name, ": ", e$message)
      NULL
    })
  }
  
  base_url <- paste0(server_link, "DHS_survey_dat/DHS_meta_data/")
  message("Fetching meta info from server: ", base_url)
  meta_list <- lapply(meta_files, read_one, base_path = base_url, from_server = TRUE)
  
  names(meta_list) <- meta_files
  return(meta_list)
}
