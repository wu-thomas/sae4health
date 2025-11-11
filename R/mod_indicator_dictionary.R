#' indicator_dictionary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indicator_dictionary_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    div(class = "module-title",
        h4("DHS Standard Indicator Dictionary")), # Add a title
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 10px",
                 uiOutput(ns("text_display"))
             )
      )
    ),
    fluidRow(
      column(12,
             #tags$h4("Estimates from models"),
             hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),
             div(style = " margin: auto;float: left;width:100%;max-width:1000px",
                 DT::dataTableOutput(ns("DHS_ind_dict_tab"))
             )
      ),
      # column(12,
      #        div( style = "width:100%;max-width:1000px; margin-top: 20px; display: flex; justify-content: center;",
      #             uiOutput(ns("download_button_ui"))
      #        )
      # )
    )
    # Place additional UI elements below
  )

}

#' indicator_dictionary Server Functions
#'
#' @noRd
mod_indicator_dictionary_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo,parent_session){
  moduleServer( id, function(input, output, session){

    ns <- session$ns


    ### text

    output$text_display <- renderUI({

      HTML(paste0(
        "<p style='font-size: large;'>",
        "Below is a complete list of indicators as defined by the DHS. <br>",
        "Please note that only a ",
        "<strong style='background-color: #D0E4F7;'>subset</strong>",
        " of these indicators is currently supported by the app (see ",
        actionButton(
          ns("switch_app_ind"),  # Button ID to trigger the modal
          "here",
          style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: large;"  # Enhanced styling
        ),
        "). <br>",
        "If there are specific indicators you would like added to the app, please do not hesitate to ",
        tags$a("contact us", href = paste0(CountryInfo$website_link(),"/team/STAB_group/"),
               target = "_blank",style = "color: blue; "),
        ".</p>",
        "<p style='font-size: medium;margin-top:10px;'>",
        "<span style='font-weight: bold; color: #FF0000;'>*</span> ",
        "For experienced R users, you may create customized indicators directly in the surveyPrev package through getDHSindicator().",
        "</p>"
      ))


    })

    ### switch to tab with indicator supported by our app
    observeEvent(input$switch_app_ind, {
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "indicator_in_app")
      shinyjs::js$activateTab("tool_kit")

    })




    ### table display

    output$DHS_ind_dict_tab <- DT::renderDataTable({

        dt <- DT::datatable(DHS_ind_dictionary,
                            options = list(pageLength = 5,scrollX = TRUE,
                                           scroller = TRUE),
                            filter = 'top', rownames = FALSE)

        # Apply formatting styles
        dt <- DT::formatStyle(dt,
                              columns = colnames(DHS_ind_dictionary),
                              backgroundColor = 'rgba(255, 255, 255, 0.8)',
                              border = '1px solid #ddd',
                              fontSize = '14px',
                              fontWeight = 'normal')
        dt







    })

  })
}

## To be copied in the UI
# mod_indicator_dictionary_ui("indicator_dictionary_1")

## To be copied in the server
# mod_indicator_dictionary_server("indicator_dictionary_1")
