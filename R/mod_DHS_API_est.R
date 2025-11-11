#' DHS_API_est UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DHS_API_est_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    div(class = "module-title",
        h4("Comparing with DHS Final Report Estimates")), # Add a title

    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 10px;max-width:1000px",
                 uiOutput(ns("comparison_text"))
             )
      )
    ),
    ### selected indicator, app estimates
    fluidRow(
      column(12,
             #tags$h4("Estimates from models"),
             div(style = " margin-bottom: -5px;float: left;width:100%;max-width:1000px",
                 uiOutput(ns("app_natl_estimate"))
             )
      )
    ),
    ### selected indicator, DHS estimates
    fluidRow(
      column(12,
             #tags$h4("Estimates from models"),
             div(style = " margin-bottom: -5px;float: left;width:100%;max-width:1000px",
                 uiOutput(ns("selected_ind_UI"))
             )
      )
    )
  )

}

#' DHS_API_est Server Functions
#'
#' @noRd
mod_DHS_API_est_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo,parent_session){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    DHS_api_est      <- isolate(MetaInfo$DHS_api_est())
    DHS.country.meta <- isolate(MetaInfo$DHS.country.meta())
    DHS.survey.meta  <- isolate(MetaInfo$DHS.survey.meta())
    DHS.dataset.meta <- isolate(MetaInfo$DHS.dataset.meta())

    ###############################################################
    ### text for why make comparison
    ###############################################################

    output$comparison_text <- renderUI({

      return(HTML(paste0(
        "<div style='font-size: large;'>",
        "<p>",
        "To ensure the accuracy of our indicator coding schemes, we highly recommand users to compare <strong>app-calculated national estimates</strong> with the <strong>DHS final reports</strong>",
        " (through ",
        tags$a("DHS API", href = "https://api.dhsprogram.com/#/index.html",
                 target = "_blank", class = "official-link"),
        "). While our framework is designed to be applicable across various countries and surveys, manual adjustments may occasionally be necessary, especially for the recent DHS-8 surveys.",
        "</p>",
        "<p>",
        "Perfect matches cannot be guaranteed, and ",
        "<span style='background-color: #D0E4F7; padding: 3px; border-radius: 3px;'>",
        "<strong>discrepancies exceeding 2% may indicate coding issues</strong>.",
        "</span>",
        "</p>",
        "<p>",
        "We are constantly fixing survey specific issues to improve reliability. ",
        "If you encounter any discrepancies, please ",
        tags$a("contact us", href = paste0("https://wu-thomas.github.io/surveyPrev_website/team/contact_us/"),
               target = "_blank", class = "official-link"),
        " so we can investigate and refine our indicator coding.",
        "</p>",
        "</div>",
        tags$hr(style="border-top-color: #E0E0E0;")
      )))

    })


    ###############################################################
    ### app national estimate
    ###############################################################

    output$app_natl_estimate <- renderUI({


      gadm.list <- CountryInfo$GADM_list()
      cluster.geo <- CountryInfo$svy_GPS_dat()
      analysis.dat <-   CountryInfo$svy_analysis_dat()


      if( is.null(gadm.list)|is.null(cluster.geo)|is.null(analysis.dat)){

        no_selection_text <- HTML(paste0(
          "<h3 style='font-size: large;margin-top:15px'><strong>National Estimate from the App</strong></h3>",
          "<p style='font-size: large;'>",
          "Please follow the ",
          actionButton(
            ns("switch_instruction_tab"),  # Button ID to trigger the modal
            "instructions",
            style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: large;"  # Larger font
          ),
          " to select a country, survey, and indicator, upload data and obtain national estimates to proceed with this checking.",
          "</p>",
          tags$hr(style="border-top-color: #E0E0E0;")
          )
        )

        return(no_selection_text)

      }else{
        # cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
        #                                         poly.adm1=gadm.list[[paste0('Admin-',1)]],
        #                                         poly.adm2=gadm.list[[paste0('Admin-',1)]],
        #                                         by.adm1 = paste0("NAME_",1),
        #                                         by.adm2 = paste0("NAME_",1))
        #
        #
        # res_adm <- tryCatch({
        #   # First attempt with alt.strata='v022'
        #   surveyPrev::directEST(data = analysis.dat,
        #                         cluster.info = cluster.info,
        #                         admin = 0,
        #                         strata = "all",
        #                         alt.strata = 'v022')
        # }, error = function(e) {
        #   # If the first attempt fails, try with alt.strata=NULL
        #   tryCatch({
        #     surveyPrev::directEST(data = analysis.dat,
        #                           cluster.info = cluster.info,
        #                           admin = 0,
        #                           strata = "all",
        #                           alt.strata = NULL)
        #   }, error = function(e) {
        #     # If both attempts fail, set res_adm to NULL
        #     NULL
        #   })
        # })

        res_adm <- AnalysisInfo$Natl_res()

        # Check the result
        if (is.null(res_adm)) {
          return(HTML(paste0(
            "<h3 style='font-size: large;margin-top:15px'><strong>National Estimate from the App</strong></h3>",
            "<p style='font-size: large;'>",
            "Something is wrong with the national direct estimates. ",
            "</p>",
            tags$hr(style="border-top-color: #E0E0E0;")
            ))
          )
        }

        #message(res_adm[1])

        natl_est = res_adm[1]*100

        if(grepl("dying", CountryInfo$svy_indicator_des())|grepl("ortality", CountryInfo$svy_indicator_des())){
          natl_est = natl_est*10
          description_suffix <- ' per 1000 individuals'
        }else{
          description_suffix <- ' %'
        }

        natl_est <- format(round(natl_est, digits=2), nsmall = 2)

        natl_text <- HTML(paste0(
          # Subject Title
          "<h3 style='font-size: large;margin-top:15px'><strong>National Estimate from the App</strong></h3>",
          "<p style='font-size: large;'>",
          "For the indicator ",
          "<strong style='background-color: #D0E4F7;'>", CountryInfo$svy_indicator_des(), "</strong>,<br>",
          "based on the <strong style='background-color: #D0E4F7;'>DHS ", CountryInfo$svyYear_selected(), "</strong> survey for ",
          "<strong style='background-color: #D0E4F7;'>", CountryInfo$country(), "</strong>,<br>",
          "our app yields a national estimate of ",
          "<strong style='background-color: #F2DF8D;'>", natl_est, description_suffix, "</strong>.",
          "<br>If the results align, please proceed to ",
          actionButton(
            ns("switch_model_fitting"),
            "model fitting",
            style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-weight:bold;font-size: large;"
          ),
          ".</p>",
          tags$hr(style = "border-top-color: #E0E0E0;")
        ))

        return(natl_text)

      }


    })

    observeEvent(input$switch_model_fitting, {
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "model_fit")
    })

    ###############################################################
    ### table for selected indicator
    ###############################################################

    output$selected_ind_UI <- renderUI({


      if(is.null(CountryInfo$svyYear_selected())| CountryInfo$svyYear_selected()==''){
        return (NULL)
      }


      if (is.null(CountryInfo$svy_indicator_var())){
        return (NULL)
      }

      DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

      tmp_indicator_ID  <- CountryInfo$svy_indicator_var()

      {
        ### customized window
        if(tmp_indicator_ID=='CM_ECMR_C_NNF'){tmp_indicator_ID = 'CM_ECMR_C_NNR'}

      }


      ind_api_est <- DHS_api_est[DHS_api_est$`Country Code`==DHS_country_code&
                                   DHS_api_est$`DHS Standard ID`==tmp_indicator_ID&
                                   DHS_api_est$`Survey Year`==CountryInfo$svyYear_selected(),]

      if(dim(ind_api_est)[1]==0){
        return(HTML(paste0(
          "<p style='font-size: large;'>",
          "Estimate from the DHS report is not available through DHS API. Please manually check for consistency. ",
          "</p>")
        ))
      }



      return(
        tagList(HTML(paste0(
          "<h3 style='font-size: large;margin-top:15px'><strong>National Estimate from the DHS Report</strong></h3>",
          "<p style='font-size: large;'>",
          "</p>")),
          DT::DTOutput(ns("selected_ind_DHS_API_est")),
          HTML(paste0(
            "<p style='font-size: medium;margin-top:10px;'>",
            "<span style='font-weight: bold; color: #FF0000;'>*</span> ",
            "For indicators with multiple versions (as indicated in the 'By Variable Label' column), we default to using the ",
            "5-year period preceding the survey (if not otherwise specified) and unstratified age groups (total).",
            "</p>"
          )),
          tags$hr(style="border-top-color: #E0E0E0;")  # Add your customized hr below the table
        )
      )


    })


    observeEvent(input$switch_instruction_tab, {
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "get_start")
    })




    ###############################################################
    ### table for indicator in the app
    ###############################################################

    output$selected_ind_DHS_API_est <- DT::renderDataTable({

      tmp_indicator_ID  <- CountryInfo$svy_indicator_var()

      {
        ### customized window
        if(tmp_indicator_ID=='CM_ECMR_C_NNF'){tmp_indicator_ID = 'CM_ECMR_C_NNR'}

      }


      DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode
      ind_api_est <- DHS_api_est[DHS_api_est$`Country Code`==DHS_country_code&
                                   DHS_api_est$`DHS Standard ID`==tmp_indicator_ID&
                                   DHS_api_est$`Survey Year`==CountryInfo$svyYear_selected(),]

      if(dim(ind_api_est)[1]==0){
        return(NULL)
      }

      ind_api_est <- merge(ind_api_est,
                                  DHS_ind_dictionary[,c("DHS Standard Indicator ID","Full Definition")],
                                  by.x="DHS Standard ID",
                                  by.y="DHS Standard Indicator ID",
                                  all.x=T)


      ind_api_est <- ind_api_est[,c('Country','Survey Year',
                                    'DHS Standard ID','Full Definition','Estimate','By Variable Label')]

      dt <- DT::datatable(ind_api_est,
                          rownames = FALSE,
                          filter = 'none',  # Disable the search bar at the top
                          options = list(
                            dom = 't',  # This option removes all DataTables elements except the table itself
                            paging = FALSE,  # Disable pagination
                            info = FALSE,  # Hide info about the number of entries
                            searching = FALSE,  # Disable searching functionality
                            ordering = FALSE  # Disable column sorting
                          ))

      # Apply formatting styles
      dt <- DT::formatStyle(dt,
                            columns = colnames(ind_api_est),
                            backgroundColor = 'rgba(255, 255, 255, 0.8)',
                            border = '1px solid #ddd',
                            fontSize = '14px',
                            fontWeight = 'normal')

      dt <- DT::formatStyle(dt,
                            columns = 'Estimate',
                            backgroundColor = '#F2DF8D')


      dt
    })




  })
}

## To be copied in the UI
# mod_DHS_API_est_ui("DHS_API_est_1")

## To be copied in the server
# mod_DHS_API_est_server("DHS_API_est_1")
