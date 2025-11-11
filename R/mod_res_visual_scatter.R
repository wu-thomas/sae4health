#' res_visual_scatter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_visual_scatter_ui <- function(id){
  ns <- NS(id)

  fluidPage(

    tags$head(
      # Custom CSS for styling
      tags$style(HTML("
      .button-container {
        display: flex;           /* Use flexbox to center the button */
        justify-content: center; /* Center button horizontally */
        width: max(50%, 600px);  /* Max width same as map */
        margin: 20px auto;       /* Centering the container itself horizontally */
      }
    "))
    ),

    div(class = "module-title",
        h4("Subnational Estimate Comparison - Scatter Plot")
    ),
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
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      ),
      column(4,
             selectInput(ns("selected_measure"), "Select Statistics",
                         choices = c("Mean"="mean",
                                     "Coefficient of Variation"= "cv",
                                     "Width of 95% Credible Interval"="CI.width"))
      )
    ),
    fluidRow(
      column(4,
             selectInput(ns("method_x"), "Select Method on X-axis",
                         choices = c("Direct Estimates"="Direct",
                                     "Area-level Model"= "FH", "Unit-level Model"="Unit"))
      ),
      column(4,
             selectInput(ns("method_y"), "Select Method on Y-axis",
                         choices = c("Direct Estimates"="Direct",
                                     "Area-level Model"= "FH", "Unit-level Model"="Unit"))
      )

    ),
    fluidRow(
      column(12,
             tags$h4("Scatter plot comparing estimates from fitted models for the same Admin level"),
             hr(style="border-top-color: #E0E0E0;"), # More subtle horizontal line
             shinyWidgets::materialSwitch(inputId = ns("Interactive_Ind"), label = "Interactive Plot Enabled",
                                          status = "success",value =T),

             div(
               id = "map-container",
               style = "width: max(50%, 600px); margin-top: 20px;",
               uiOutput(ns("Plot_Canvas"))
               #leaflet::leafletOutput(ns("prev_map"))
             ),
             div( style = "width: max(50%, 600px); margin-top: 20px; display: flex; justify-content: center;",
                  uiOutput(ns("download_button_ui"))
             )
      )
    ),



  )
}

#' res_visual_scatter Server Functions
#'
#' @noRd
mod_res_visual_scatter_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    DHS_api_est      <- isolate(MetaInfo$DHS_api_est())
    DHS.country.meta <- isolate(MetaInfo$DHS.country.meta())
    DHS.survey.meta  <- isolate(MetaInfo$DHS.survey.meta())
    DHS.dataset.meta <- isolate(MetaInfo$DHS.dataset.meta())

    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Package 'plotly' is required for this function. Please install it with install.packages('plotly').")
    }

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

    ### update Admin choices
    observeEvent(CountryInfo$GADM_analysis_levels(), {
      adm.choice <- CountryInfo$GADM_analysis_levels()
      adm.choice <- adm.choice[adm.choice!='National']
      updateSelectInput(inputId = "selected_adm",
                        choices = adm.choice)
    })

    ###############################################################
    ### determine interactive vs static map based on user selection
    ###############################################################

    observeEvent(input$Interactive_Ind,{

      CountryInfo$display_interactive(input$Interactive_Ind)

    })

    observeEvent(CountryInfo$display_interactive(),{

      interactive_map <- CountryInfo$display_interactive()
      shinyWidgets::updateMaterialSwitch(session=session, inputId="Interactive_Ind", value = interactive_map)

    })

    ### determine which UI to present plot

    output$Plot_Canvas <- renderUI({
      if (input$Interactive_Ind) {  # if TRUE, show interactive map
        plotly::plotlyOutput(ns("plot_interactive"))
      } else {  # if FALSE, show static map
        plotOutput(ns("plot_static"))
      }
    })

    output$download_button_ui <- renderUI({
      if (input$Interactive_Ind) {  # HTML download
        return(NULL)
      } else {
        downloadButton(ns("download_static"), "Download as PDF", icon = icon("download"),
                       class = "btn-primary")
      }
    })


    ###############################################################
    ### prepare maps
    ###############################################################

    output$plot_interactive <- plotly::renderPlotly({

      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### initialize parameters
      selected_adm <- input$selected_adm
      selected_measure <- input$selected_measure
      selected_method_x <- input$method_x
      selected_method_y <- input$method_y


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}


      ### load results
      model_res_all <- AnalysisInfo$model_res_list()

      strat.gadm.level <- CountryInfo$GADM_strata_level()

      model_res_x <- model_res_all[[selected_method_x]][[selected_adm]]
      model_res_y <- model_res_all[[selected_method_y]][[selected_adm]]

      ### plot
      if(is.null(model_res_x)|selected_adm=='National'|is.null(model_res_y)){

        return(NULL)

      }else{

        method_match <- c(
          "Direct" = "Direct estimates",
          "Unit" = "Unit-level model estimates",
          "FH" = "Area-level model estimates"
        )

        label_x <- method_match[selected_method_x]
        label_y <- method_match[selected_method_y]

        plot.interactive <- scatter.plot( res.obj.x = model_res_x,
                                     res.obj.y = model_res_y,
                                     value.to.plot = selected_measure,
                                     model.gadm.level = admin_to_num(selected_adm),
                                     strata.gadm.level = CountryInfo$GADM_strata_level(),
                                     label.x = label_x,
                                     label.y = label_y,
                                     plot.title=NULL,
                                     interactive=T)

      }
      #prev.map.static.output(prev.static.plot)
      #message(paste0(input$prev_map$lng,'_',input$map_center$lat,'_', input$map_zoom))
      return(plot.interactive)

    })



    static.plot.to.download <- reactiveVal(NULL)

    output$plot_static <- renderPlot({

      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### initialize parameters
      selected_adm <- input$selected_adm
      selected_measure <- input$selected_measure
      selected_method_x <- input$method_x
      selected_method_y <- input$method_y


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}


      ### load results
      model_res_all <- AnalysisInfo$model_res_list()

      strat.gadm.level <- CountryInfo$GADM_strata_level()

      model_res_x <- model_res_all[[selected_method_x]][[selected_adm]]
      model_res_y <- model_res_all[[selected_method_y]][[selected_adm]]

      ### plot
      if(is.null(model_res_x)|selected_adm=='National'|is.null(model_res_y)){

        return(NULL)

      }else{

        method_match <- c(
          "Direct" = "Direct estimates",
          "Unit" = "Unit-level model estimates",
          "FH" = "Area-level model estimates"
        )

        label_x <- method_match[selected_method_x]
        label_y <- method_match[selected_method_y]


        plot.static <- scatter.plot( res.obj.x = model_res_x,
                                          res.obj.y = model_res_y,
                                          value.to.plot = selected_measure,
                                          model.gadm.level = admin_to_num(selected_adm),
                                          strata.gadm.level = CountryInfo$GADM_strata_level(),
                                          label.x = label_x,
                                          label.y = label_y,
                                          plot.title=NULL,
                                          interactive=F)

        static.plot.to.download(plot.static)
      }
      #prev.map.static.output(prev.static.plot)
      #message(paste0(input$prev_map$lng,'_',input$map_center$lat,'_', input$map_zoom))
      return(plot.static)

    })

    output$download_static <- downloadHandler(
      filename = function() {

        ### informative file name
        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

        file.prefix <- paste0(DHS_country_code,CountryInfo$svyYear_selected(),'_',
                              CountryInfo$svy_indicator_var(),'_',
                              input$selected_adm,'_',
                              input$selected_measure)
        file.prefix <- gsub("[-.]", "_", file.prefix)

        return(paste0(file.prefix,'_scatter.pdf'))

      },

      content = function(file) {
        # Create the PDF
        grDevices::pdf(file, width = 10, height = 10)  # Set width and height of the PDF
        print(static.plot.to.download())  # Print the plot to the PDF
        grDevices::dev.off()  # Close the PDF
      }
    )


  })
}

## To be copied in the UI
# mod_res_visual_scatter_ui("res_visual_scatter_1")

## To be copied in the server
# mod_res_visual_scatter_server("res_visual_scatter_1")
