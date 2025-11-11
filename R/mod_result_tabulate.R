#' result_tabulate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_result_tabulate_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    div(class = "module-title",
    h4("Results Tabulation")), # Add a title

    ## country, survey and indicator info
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 5px",
                 uiOutput(ns("info_display"))
             )
      )
    ),

    fluidRow(
      column(4,
             selectInput(ns("selected_method"), "Select Method",
                         choices = c("Direct Estimates"="Direct" ,
                                     "Area-level Model"= "FH" , "Unit-level Model"="Unit" ))
      ),
      column(4, # Another half-width column for the second selection bar
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      )
    ),
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
                 DT::dataTableOutput(ns("Res_tab"))
             )
      ),
      column(12,
      div( style = "width:100%;max-width:1000px; margin-top: 20px; display: flex; justify-content: center;",
           uiOutput(ns("download_button_ui"))
      )
      )
    )
    # Place additional UI elements below
  )
}

#' result_tabulate Server Functions
#'
#' @noRd
mod_result_tabulate_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    DHS_api_est      <- isolate(MetaInfo$DHS_api_est())
    DHS.country.meta <- isolate(MetaInfo$DHS.country.meta())
    DHS.survey.meta  <- isolate(MetaInfo$DHS.survey.meta())
    DHS.dataset.meta <- isolate(MetaInfo$DHS.dataset.meta())

    ###############################################################
    ### display country, survey and indicator info
    ###############################################################

    output$info_display <- renderUI({

      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      req(CountryInfo$svy_analysis_dat())

      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()

      HTML(paste0(
        "<p style='font-size: large;'>",
        "Selected Country: <span style='font-weight:bold;background-color: #D0E4F7;'>", country, "</span>.",
        " Survey Year: <span style='font-weight:bold;background-color: #D0E4F7;'>", svy_year, "</span>.",
        "<br>",
        "Indicator: <span style='font-weight:bold;background-color: #D0E4F7;'>", CountryInfo$svy_indicator_des(),
        "</span>.</p>",
        "<hr style='border-top-color: #E0E0E0;'>"
      ))

    })

    ### update parameters
    row_names <- c("Direct", "FH", "Unit")
    nrows <- length(row_names)

    col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
    ncols <- reactive({ length(col_names()) })

    observeEvent(col_names(), {
      updateSelectInput(inputId = "selected_adm",
                        choices = col_names())
    })


    output$text_display <- renderUI({

      ### return empty map if no subnational level selected
      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### extract selections
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method

      ### initialize parameters
      model_res_all <- AnalysisInfo$model_res_list()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]

      method_match <- c(
        "Direct" = "Direct estimates",
        "Unit" = "Unit-level",
        "FH" = "Area-level"
      )

      method_des <- method_match[selected_method]

      if(is.null(model_res_selected)){

        model_res_tab(NULL)

        HTML(paste0(
          "<p style='font-size: large;'>",
          "Results for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span>",
          " level are not available. Please make sure the model has been successfully fitted.",
          "</p>"
        ))

      }else{

        HTML(paste0(
          "<p style='font-size: large;'>",
          "Tabulating estimates for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> level.",
          "</p>"
        ))

      }


    })



    ### tabulate
    model_res_tab <- reactiveVal(NULL)


    output$Res_tab <- DT::renderDataTable({

      req(input$selected_adm)
      req(input$selected_method)

      selected_adm <- input$selected_adm
      selected_method <- input$selected_method


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}

      model_res_all <- AnalysisInfo$model_res_list()

      #model_res_all <- mdg.ex.model.res

      strat.gadm.level <- CountryInfo$GADM_strata_level()

      if(admin_to_num(selected_adm) > strat.gadm.level){pseudo_level=2}else{
        if(admin_to_num(selected_adm)==0){pseudo_level=0}else{pseudo_level=1}}


      #message(pseudo_level)
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]][[paste0('res.admin',pseudo_level)]]

      if(is.null(model_res_selected)){return()
      }else{

        res.to.tabulate <- harmonize_all_cols(survey.res=model_res_selected)
        #res.to.tabulate <- format_tab_num(survey.res=res.to.tabulate)
        res.to.tabulate <- subset(res.to.tabulate, select=-c(var))


        names(res.to.tabulate)[names(res.to.tabulate) == "mean"] <- "Mean"
        names(res.to.tabulate)[names(res.to.tabulate) == "sd"] <- "Standard_Error"
        names(res.to.tabulate)[names(res.to.tabulate) == "lower"] <- "Lower_CI"
        names(res.to.tabulate)[names(res.to.tabulate) == "upper"] <- "Upper_CI"
        names(res.to.tabulate)[names(res.to.tabulate) == "cv"] <- "Coefficient_of_Variation"
        names(res.to.tabulate)[names(res.to.tabulate) == "CI.width"] <- "Width_95_CI"



        if("median" %in% names(res.to.tabulate)){
        names(res.to.tabulate)[names(res.to.tabulate) == "median"] <- "Median"
        }
        if("region.name" %in% names(res.to.tabulate)){
          names(res.to.tabulate)[names(res.to.tabulate) == "region.name"] <- "Region_Name"
        }
        if("upper.adm.name" %in% names(res.to.tabulate)){
          names(res.to.tabulate)[names(res.to.tabulate) == "upper.adm.name"] <- "Upper_Admin_Name"
        }
        if("region.name.full" %in% names(res.to.tabulate)){
          names(res.to.tabulate)[names(res.to.tabulate) == "region.name.full"] <- "Region_Name_Full"
        }

        model_res_tab(res.to.tabulate)

        dt <- DT::datatable(res.to.tabulate,
                            options = list(pageLength = 5,scrollX = TRUE,
                                           scroller = TRUE,autoWidth = TRUE),
                            filter = 'top', rownames = FALSE)

        numeric_columns <- sapply(res.to.tabulate, is.numeric)

        # store results before rounding

        # format numerical
        dt <- DT::formatRound(dt, columns = numeric_columns, digits = 3)

        # format cv as %
        dt$Coefficient_of_Variation <- dt$Coefficient_of_Variation * 100
        dt <- DT::formatPercentage(dt,columns='Coefficient_of_Variation', digits=1)

        # Apply formatting styles
        dt <- DT::formatStyle(dt,
                              columns = names(res.to.tabulate),
                              backgroundColor = 'rgba(255, 255, 255, 0.8)',
                              border = '1px solid #ddd',
                              fontSize = '14px',
                              fontWeight = 'normal',
                              lineHeight = '1.42857143')
        dt




      }


    })


    ### download button

    output$download_button_ui <- renderUI({

      prepared.res <- model_res_tab()

      if (!is.null(prepared.res)) {  # HTML download
        downloadButton(ns("download_csv"), "Download as csv", icon = icon("download"),
                       class = "btn-primary")
      } else {
        NULL
      }
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

        file.prefix <- paste0(DHS_country_code,CountryInfo$svyYear_selected(),'_',
                              CountryInfo$svy_indicator_var(),'_',
                              input$selected_adm,'_',
                              input$selected_method)
        file.prefix <- gsub("[-.]", "_", file.prefix)

        return(paste0(file.prefix,'.csv'))
      },
      content = function(file) {
        prepared.res <- as.data.frame(model_res_tab())
        readr::write_csv(prepared.res, file)
      }
    )





  })
}

## To be copied in the UI
# mod_result_tabulate_ui("result_tabulate_1")

## To be copied in the server
# mod_result_tabulate_server("result_tabulate_1")
