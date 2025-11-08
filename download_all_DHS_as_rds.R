##############################################################################
#########   load libraries
##############################################################################

library(rdhs)
library(dplyr)

##############################################################################
#########   load survey meta data
##############################################################################

 set_rdhs_config(email = "jonno@uw.edu",
                 project = "Mapping of U5MR",
                 global = FALSE)

DHS.country.meta <- rdhs::dhs_countries()
DHS.survey.meta <- rdhs::dhs_surveys()
DHS.dataset.meta <- rdhs::dhs_datasets()

#base_dir <- "/Users/jonno/Dropbox/YunhanJon/DHS-indicators/DHS_raw_dat_rds"
base_dir <- "C:/Users/lucyx/Desktop/sae4health/data/new"
setwd(base_dir)

##############################################################################
#########   load survey meta data
##############################################################################

recode_list <- c("Births Recode"   ,"Couples' Recode" , "Household Recode","Siblings Recode",
                 "Individual Recode" ,"Children's Recode" ,"Men's Recode" , "Household Member Recode",
                 "Pregnancy and Postnatal Care Recode","Pregnancies Recode",
                 "HIV Test Results Recode",
                 "Wealth Index")

# country_list <- c("AO", "BJ" ,"BF", "BU", "CM", "TD" , "CD", "CI", "SZ" ,"ET", "GA",
#                  "KE","LS" , "MD", "MW","ML" ,"MZ"  ,"NI" ,"NG" ,"RW" , "SN" ,"SL" , "TZ","UG" ,"ZM", "ZW")

# country_list <- c(
#   "AO", "BJ", "BO", "BR", "BF", "BI", "CM", "CF", "TD", "CO",
#   "KM", "CG", "CD", "CI", "DO", "EC", "SV", "SZ", "ET", "GA",
#   "GM", "GH", "GT", "GN", "GY", "HT", "HN", "KE", "LS", "LR",
#   "MG", "MW", "ML", "MR", "MX", "MZ", "NA", "NI", "NE", "NG",
#   "NG", "PY", "PE", "RW", "ST", "SN", "SL", "ZA", "SD", "TZ",
#   "TG", "TT", "UG", "ZM", "ZW"
# )

country_list <- unique(DHS.country.meta$DHS_CountryCode)

potential_surveys <-  DHS.dataset.meta %>% dplyr::filter(SurveyType == 'DHS' & #
                                                           DHS_CountryCode %in% country_list & #SurveyYear>1999 &
                                                           ((FileType %in% recode_list &
                                                               FileFormat=='Stata dataset (.dta)') |
                                                              (FileType == 'Geographic Data')|
                                                              (FileType == 'Geospatial Covariates')))

##############################################################################
#########   Generate Folder Structure
##############################################################################

# Get unique combinations of Country & Survey Year
unique_surveys <- potential_surveys %>%
  select(DHS_CountryCode, SurveyYear) %>%
  distinct()

# # Define base directory
# base_dir <- "E:/Dropbox/YunhanJon/DHS-indicators/DHS_raw_dat_rds"
#
# for (i in seq_len(nrow(unique_surveys))) {
#   country <- unique_surveys$DHS_CountryCode[i]
#   survey_year <- unique_surveys$SurveyYear[i]
#
#   # Define paths
#   country_dir <- file.path(base_dir, country)
#   survey_dir <- file.path(country_dir, paste0("DHS_", survey_year))
#
#   # Create directories if they do not exist
#   if (!dir.exists(survey_dir)) {
#     dir.create(survey_dir, recursive = TRUE)
#     message("Created folder: ", survey_dir)
#   }
# }

##############################################################################
#########   Download and Save Datasets
##############################################################################

for (i in seq_len(nrow(unique_surveys))) {

  country <- unique_surveys$DHS_CountryCode[i]
  survey_year <- unique_surveys$SurveyYear[i]

  # Define paths
  country_dir <- file.path(base_dir, country)
  survey_dir <- file.path(country_dir, paste0("DHS_", survey_year))

  # Create directories if they do not exist
  if (!dir.exists(survey_dir)) {
    dir.create(survey_dir, recursive = TRUE)
    message("Created folder: ", survey_dir)
  }

  # Define survey directory
  setwd(survey_dir)

  # Get all datasets available for this survey
  survey_datasets <- potential_surveys %>%
    filter(DHS_CountryCode == country, SurveyYear == survey_year)

  for (j in seq_len(nrow(survey_datasets))) {

    recode_type <- survey_datasets$FileType[j]
    file_id <- survey_datasets$FileName[j]

    # Define output file path
    output_file <- file.path(survey_dir, paste0(gsub(" ", "_", recode_type), ".rds"))

    # Skip if file already exists
    if (file.exists(output_file)) {
      message("Skipping existing file: ", output_file)
      next
    }

    # Try to download and save the dataset
    tryCatch({
      message("Downloading: ", file_id, " (", recode_type, ") for ", country, " - ", survey_year)

      # Download dataset
      data.paths.tmp <- rdhs::get_datasets(survey_datasets$FileName[j], clear_cache = T)

      dataset <- readRDS(paste0(data.paths.tmp))

      if (!is.data.frame(dataset) || nrow(dataset) <= 1) {
        stop("Error: download unsuccessful.")
      }

      # Save as .rds
      saveRDS(dataset, file = output_file)

      message("Saved: ", output_file)

    }, error = function(e) {
      message("Error downloading ", file_id, " for ", country, " - ", survey_year, ": ", e$message)
    })
  }
}
