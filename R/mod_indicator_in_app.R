#' indicator_in_app UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indicator_in_app_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    div(class = "module-title",
        h4("Indicators Supported in the App")), # Add a title

    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 10px",
                 uiOutput(ns("selected_ind_text"))
             )
      )
    ),
    ### selected indicator
    fluidRow(
      column(12,
             #tags$h4("Estimates from models"),
             div(style = " margin-bottom: -5px;float: left;width:100%;max-width:1000px",
                 uiOutput(ns("selected_ind_UI"))
             )
      )
    ),

    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 10px",
                 uiOutput(ns("text_app_ind"))
             )
      )
    ),
    fluidRow(
      column(12,
             hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),
             div(style = " margin: auto;float: left;width:100%;max-width:1000px",
                 DT::dataTableOutput(ns("DHS_ind_supported_tab"))
             )
      )
    )
  )

}


#' indicator_in_app Server Functions
#'
#' @noRd
mod_indicator_in_app_server <- function(id,CountryInfo,AnalysisInfo,parent_session){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    ref_tab_all <- surveyPrev::indicatorList

    ###############################################################
    ### text for selected indicator
    ###############################################################

    output$selected_ind_text <- renderUI({

      if(is.null(CountryInfo$svyYear_selected())| CountryInfo$svyYear_selected()==''){
        return (NULL)
      }else{
        if (!is.null(CountryInfo$svy_indicator_var())){

          return(
            HTML(paste0(
              "<h3 style='font-size: large;margin-top:15px'><strong>Selected Indicator</strong></h3>",
              "<p style='font-size: large;'>",
              "Below is the detailed information for the indicator you selected in the ",
              actionButton(
                ns("switch_country_spec"),  # Button ID to trigger the modal
                "country specification panel",
                style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: large;"  # Enhanced styling
              ),
              ".</p>"
              ))
          )
        }
      }

      return(NULL)



    })

    ### switch to country specification
    observeEvent(input$switch_country_spec, {
      #message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "country_spec")
    })


    ###############################################################
    ### table for selected indicator
    ###############################################################
    # "<p style='font-size: medium;margin-top:10px;'>",
    # "<span style='font-weight: bold; color: #FF0000;'>*</span> ",
    # "For indicators that apply to multiple timeframes",
    # "or where the number of years preceding the survey is not explicitly defined,",
    # "we default to using data from the 5-year period preceding the survey, unless otherwise specified.",
    # "For indicators that can be stratified by age groups,",
    # "the default is to use data for the total population or unstratified groups (e.g., women aged 15-49), unless stated differently.",
    # "</p>"
    output$selected_ind_UI <- renderUI({

      if(is.null(CountryInfo$svyYear_selected())| CountryInfo$svyYear_selected()==''){
        return (NULL)
      }else{
        if (!is.null(CountryInfo$svy_indicator_var())){
          tagList(
            DT::DTOutput(ns("selected_ind_tab")),
            HTML(paste0("<p style='font-size: medium; margin-top:10px;'>",
                        "<span style='font-weight: bold; color: #FF0000;'>*</span> ",
                        "For indicators that apply to multiple timeframes ",
                        "and the number of years preceding the survey is not explicitly specified, ",
                        "we default to using data from the 5-year period preceding the survey. ",
                        "For indicators that can be stratified by age groups, ",
                        "we default to total population or unstratified groups (e.g., women aged 15-49), unless stated differently.",
                        "</p>")),
            tags$hr(style="border-top-color: #E0E0E0;")  # Add your customized hr below the table
          )

        }

      }


    })

    ###############################################################
    ### table for indicator in the app
    ###############################################################

    output$selected_ind_tab <- DT::renderDataTable({

      selected_ind_table <- ref_tab_all
      selected_ind_table <- selected_ind_table[,c("ID" ,"Description","Full_definition","Topic")]

      selected_ind_table$Topic <- as.factor(selected_ind_table$Topic)

      tmp_indicator_ID  <- CountryInfo$svy_indicator_var()

      {
        ### customized NNR window, 5/10 yrs using the same definition
        if(tmp_indicator_ID=='CM_ECMR_C_NNF'){tmp_indicator_ID = 'CM_ECMR_C_NNR'}

      }



      selected_ind_table<- selected_ind_table[selected_ind_table$ID ==tmp_indicator_ID,]

      colnames(selected_ind_table) <- c("DHS Standard ID" ,"Label","Full Definition","DHS Report Chapter")

      selected_ind_table <- merge(selected_ind_table[, c("DHS Standard ID" ,"Label","DHS Report Chapter")],
                                  DHS_ind_dictionary[,c("DHS Standard Indicator ID","Full Definition","Denominator",
                                                        "Measurement Type")],
                                  by.x="DHS Standard ID",
                                  by.y="DHS Standard Indicator ID",
                                  all.x=T)

      selected_ind_table <- selected_ind_table[,c("DHS Standard ID" ,"Label","Full Definition","DHS Report Chapter",
                                                  "Denominator","Measurement Type")]

      dt <- DT::datatable(selected_ind_table,
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
                        columns = colnames(selected_ind_table),
                        backgroundColor = 'rgba(255, 255, 255, 0.8)',
                        border = '1px solid #ddd',
                        fontSize = '14px',
                        fontWeight = 'normal')

      dt
    })

    ###############################################################
    ### text for all indicators supported
    ###############################################################

    output$text_app_ind <- renderUI({


      HTML(paste0(
        "<h3 style='font-size: large;margin-top:15px'><strong>All Indicators Supported by the App</strong></h3>",
        "<p style='font-size: large;'>",
        "The app currently supports analysis using over 100 indicators, based on the DHS standard definition. ",
        "Detailed information about these indicators is available in the table below.<br> ",
        "Please note that this represents only a ",
        "<strong style='background-color: #D0E4F7;'>subset</strong>",
        " of the indicators defined by the DHS. For the complete list, refer to the ",
        actionButton(
          ns("switch_DHS_dict"),  # Button ID to trigger the modal
          "DHS Standard Indicator Dictionary",
          style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: large;"  # Enhanced styling
        ),
        ".</p>"
      ))



    })


    ### switch to tab with full list of DHS indicators
    observeEvent(input$switch_DHS_dict, {
      message('switching')
      #shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "tool_kit")
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "indicator_dictionary")
      shinyjs::js$activateTab("tool_kit")

    })




    ### display the list of indicators supported in this app
    output$DHS_ind_supported_tab <- DT::renderDataTable({


      ind_in_app <- ref_tab_all
      ind_in_app <- ind_in_app[,c("ID" ,"Description","Full_definition","Topic")]

      ind_in_app$Topic <- as.factor(ind_in_app$Topic)

      colnames(ind_in_app) <- c("DHS Standard ID" ,"Label","Full Definition","DHS Report Chapter")

      dt <- DT::datatable(ind_in_app,
                          options = list(pageLength = 5,scrollX = TRUE,
                                         scroller = TRUE),
                          filter = 'top', rownames = FALSE)

      # Apply formatting styles
      dt <- DT::formatStyle(dt,
                            columns = colnames(ind_in_app),
                            backgroundColor = 'rgba(255, 255, 255, 0.8)',
                            border = '1px solid #ddd',
                            fontSize = '14px',
                            fontWeight = 'normal')
      dt


    })

  })
}

## To be copied in the UI
# mod_indicator_in_app_ui("indicator_in_app_1")

## To be copied in the server
# mod_indicator_in_app_server("indicator_in_app_1")
