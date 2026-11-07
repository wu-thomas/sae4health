#' survey_type_helpers
#'
#' @description Helpers for supporting survey types other than the standard
#'   DHS (currently the Malaria Indicator Survey, MIS) without changing how
#'   data are organized on the server. Surveys are still identified by
#'   country + survey year; the survey type is looked up from the survey
#'   meta data (DHS.survey.meta$SurveyType).
#'
#' @noRd


###############################################################
###  Indicators supported for MIS surveys
###############################################################

### surveyPrev indicators that are published by the DHS Program for MIS
### surveys (checked against the DHS API for KE2020MIS). MIS surveys only
### collect a subset of the DHS modules, so only these indicators are offered
### in the indicator menu when an MIS survey is selected.
### To support more indicators for MIS, add their IDs here (after checking the
### app's national estimate against the MIS final report).

MIS_supported_indicators <- c(
  ## Chapter 02 - Housing Characteristics And Household Population
  "HC_AGEG_P_ADL", "HC_ELEC_H_ELC", "HC_FLRM_H_CER", "HC_HEFF_H_MPH", "HC_HEFF_H_RDO",
  "HC_TRNS_H_CAR", "HC_TRNS_H_SCT", "HC_WIXQ_P_2ND", "HC_WIXQ_P_LOW",
  "WS_SRCE_H_BAS", "WS_SRCE_H_IMP", "WS_SRCE_H_LTD", "WS_SRCE_H_USG", "WS_SRCE_P_BAS",
  "WS_TLET_H_BAS", "WS_TLET_H_IMP", "WS_TLET_P_BAS", "WS_TLET_P_IMP",
  ## Chapter 03 - Characteristics Of Respondents
  "CO_INUS_W_EVU", "CO_INUS_W_U12", "CO_INUS_W_WEK",
  "ED_EDUC_W_CPR", "ED_EDUC_W_CSC", "ED_EDUC_W_HGH", "ED_EDUC_W_NED", "ED_EDUC_W_PRI",
  "ED_EDUC_W_SEH", "ED_EDUC_W_SPR", "ED_EDUC_W_SSC",
  "ED_LITR_W_LIT", "ED_LITR_W_RDW", "ED_LITR_W_SCH",
  "ED_MDIA_W_3MD", "ED_MDIA_W_N3M", "ED_MDIA_W_NWS", "ED_MDIA_W_RDO", "ED_MDIA_W_TLV",
  ## Chapter 05 - Fertility
  "FE_FRTR_W_A15", "FE_FRTR_W_TF4", "FE_FRTR_W_TFR",
  ## Chapter 09 - Maternal Health
  "RH_ANCN_W_N01", "RH_ANCN_W_N4P", "RH_ANCN_W_NON",
  "RH_ANCP_W_CHW", "RH_ANCP_W_DOC", "RH_ANCP_W_NON", "RH_ANCP_W_SKP", "RH_ANCP_W_TBA",
  ## Chapter 10 - Child Health
  "CH_FEVR_C_FEV", "CH_FEVT_C_ADV",
  ## Chapter 11 - Nutrition Of Children And Adults
  "CN_ANMC_C_ANY",
  ## Chapter 12 - Malaria
  "ML_CMLT_C_RDT", "ML_FEVA_C_NUM", "ML_FEVT_C_ADV", "ML_NETC_C_ITN",
  "ML_NETP_H_IT2", "ML_NETU_P_ITN", "ML_PMAL_C_RDT"
)


###############################################################
###  Survey type lookup
###############################################################

#' Survey type (e.g. 'DHS', 'MIS') for a country and survey year
#'
#' Falls back to 'DHS' when the meta data or selection is missing, so that
#' existing behavior (preloaded examples, local version) is unchanged. If a
#' country ever has a DHS and another survey in the same year, 'DHS' is
#' returned, since the app (and the server folders) identify a survey by year.
#'
#' @noRd
get_survey_type <- function(country, svy_year, survey_meta) {

  if (is.null(country) || is.null(svy_year) || is.null(survey_meta) ||
      length(country) == 0 || length(svy_year) == 0 ||
      identical(country, '') || identical(as.character(svy_year), '') ||
      !all(c('CountryName', 'SurveyYear', 'SurveyType') %in% colnames(survey_meta))) {
    return('DHS')
  }

  types <- unique(survey_meta$SurveyType[survey_meta$CountryName == country &
                                           as.character(survey_meta$SurveyYear) == as.character(svy_year)])
  types <- types[!is.na(types)]

  if (length(types) == 0 || 'DHS' %in% types) {
    return('DHS')
  }

  return(as.character(types[1]))
}


#' Display label for a survey, e.g. "DHS 2022" or "MIS 2020"
#'
#' @noRd
get_survey_label <- function(svy_type, svy_year) {
  if (is.null(svy_type) || length(svy_type) == 0 || identical(svy_type, '')) {
    svy_type <- 'DHS'
  }
  paste(svy_type, svy_year)
}


#' Named choices for the survey year dropdown
#'
#' Values stay as the survey year (so nothing downstream changes), while the
#' displayed names include the survey type, e.g. "2022 (DHS)", "2020 (MIS)".
#'
#' @noRd
survey_year_choices <- function(country, svy_years, survey_meta) {

  svy_years <- sort(unique(as.character(svy_years)), decreasing = TRUE)
  svy_years <- svy_years[!is.na(svy_years) & svy_years != '']

  if (length(svy_years) == 0) {
    return(character(0))
  }

  svy_types <- vapply(svy_years,
                      function(yr) get_survey_type(country, yr, survey_meta),
                      character(1))

  stats::setNames(svy_years, paste0(svy_years, ' (', svy_types, ')'))
}


###############################################################
###  Indicator menu by survey type
###############################################################

#' Restrict the indicator table to indicators supported for a survey type
#'
#' @noRd
filter_indicators_by_survey_type <- function(ind_tab, svy_type) {

  if (identical(svy_type, 'MIS')) {
    return(ind_tab[ind_tab$ID %in% MIS_supported_indicators, , drop = FALSE])
  }

  return(ind_tab)
}
