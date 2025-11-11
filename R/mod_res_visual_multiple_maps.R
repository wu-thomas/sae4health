#' res_visual_multiple_maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_visual_multiple_maps_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(type = 'text/css', "#big_slider .irs-grid-text, #big_slider .irs-min,
      #big_slider .irs-max,#big_slider .irs-single {font-size: 14px;}"),

      # Custom CSS for styling
      tags$style(HTML("
      .button-container {
        display: flex;           /* Use flexbox to center the button */
        justify-content: center; /* Center button horizontally */
        width: max(50%, 600px);  /* Max width same as map */
        margin: 20px auto;       /* Centering the container itself horizontally */
      }
      .shiny-input-container:not(.shiny-input-container-inline) {
         width: 700px;
         max-width: 100%;
        }

        .model-checkbox-table {
          width: 100%; /* Full width to contain the DataTable */
          max-width: 800px;
          margin: 0 auto; /* Center the table horizontally */
          float: left;
        }

        .model-checkbox-table .dataTable {
          font-size: 16px; /* Larger text for readability */
          width: 100% !important; /* Force the table to expand to the container width */
          table-layout: fixed; /* Equal column widths */
          border-collapse: collapse; /* For border styling */
        }

        /* Header and cells styling */
        .model-checkbox-table .dataTable th,
        .model-checkbox-table .dataTable td {
          border: 1px solid #ddd; /* Light grey border */
          text-align: center; /* Center alignment for text */
  max-width: 300px !important; /* Ensure cells are less than 300px in width */

        }

        /* Zebra striping for rows */
        .model-checkbox-table .dataTable tr:nth-child(even){background-color: #f2f2f2;}

        /* Column and row headers styling */
        .model-checkbox-table .dataTable thead th {
          background-color: #ADD8E6; /* Green background for column headers */
          color: white; /* White text for contrast */
        }

         .model-checkbox-table .dataTable tbody tr td:first-child,
        .model-checkbox-table .dataTable thead th:first-child {
          width: 20%; /* Increase the width of the row names */
        }

        .model-checkbox-table .dataTable td input[type='checkbox'],
        .model-checkbox-table .dataTable td input[type='radio'] {
          display: block;
          margin-top: 10px;
          padding-left:3px;
          display: flex !important; justify-content: center !important; align-items: center !important;
          /* Additional custom styles for checkboxes and radio buttons can go here */
        }
    "))
    ),

    div(class = "module-title",
        h4("Comparing Multiple Maps")
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
      # Main panel on the left
      column(12,
             tabsetPanel(
                         tabPanel("Single Model: Comparing Statistics",
                                  div(style = "margin-top:15px;margin-bottom:-10px",
                                      fluidRow(
                                        column(4,
                                               selectInput(ns("selected_method"), "Select Method",
                                                           choices = c("Direct Estimates"="Direct",
                                                                       "Area-level Model"= "FH", "Unit-level Model"="Unit"))
                                        ),
                                        column(4,
                                               selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
                                        )
                                      )
                                      ),
                                      tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")

                                      fluidRow(
                                        div(style = "display: flex; justify-content: start;
                                            width: min(1000px,100%); align-items: center;margin-bottom:-10px;",
                                            # Checkbox Group
                                            div(style = "flex-grow: 1; padding-right: 5px;margin-left:15px;",  # Reduced padding for closer alignment
                                                checkboxGroupInput(ns("selected_stats"), with_red_star("Select (Multiple) Statistics to Plot: "),
                                                                   choices = c("Mean"="mean",
                                                                               "Coefficient of Variation"= "cv",
                                                                               "Width of 95% Credible Interval"="CI.width",
                                                                               "Exceedance Probability"="exceed_prob"),
                                                                   inline = TRUE)
                                            ),

                                            # Conditional Panel
                                            div(id = 'big_slider',
                                                style = "flex-grow: 0; max-width: 300px;",  # Using max-width for better control
                                                uiOutput(ns("choose_thresh_1"))
                                                #conditionalPanel(
                                                #  condition = "input.selected_stats.includes('exceed_prob')",
                                               #   sliderInput("probLevel", "Probability Level:",
                                                #              min = 0, max = 1, value = 0.95, step = 0.01)
                                                #)
                                            )
                                        )),
                                  tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
                                  fluidRow(
                                    column(12,
                                           div(style = " margin: auto;float: left;",
                                               uiOutput(ns("single_model_text_display"))
                                           )
                                    )
                                  ),

                                  # Action Button at the bottom
                                  fluidRow(
                                    div(style = "display: flex;",

                                        ### toggle input for interactive map
                                        div(style = "margin-top: 10px; margin-left: 15px;max-width: 250px;",
                                        shinyWidgets::materialSwitch(inputId = ns("mapType"), label = HTML("<strong>Interactive Map Enabled</strong>"),
                                                                     status = "success",value =T)
                                        ),
                                        div(style = "margin-left: 15px;margin-right: 15px;",
                                            tags$div(style = "margin-top: 5px; font-size: 18px;", # Larger font size for better readability
                                                     "Click ",
                                                     tags$b(actionButton(ns("plot_single_model"), "Generate Plot",
                                                                         style = "padding: 0px 3px 3px ;font-size: 18px;", class = "btn-primary")),
                                                     " to produce plot and/or apply changes."
                                            )
                                            )
                                    )
                                  ),

                                  fluidRow(
                                    column(12,
                                           div(
                                             id = "map-container",
                                             style = "width: min(98%, 1000px); margin-top: 20px;margin-bottom: 10px;",
                                             uiOutput(ns("map_single_model"))
                                           )
                                           )),
                                  fluidRow(
                                    column(12,
                                           div( style = "width: min(98%, 1000px); margin-top: 10px; display: flex; justify-content: center;",
                                                uiOutput(ns("download_single_model_ui"))

                                                #downloadButton(ns("dl"), "Download as HTML", icon = icon("download"),
                                                #            class = "btn-primary")
                                           )
                                    )
                                  )

                                  ),
                         tabPanel("Same Statistics Compared Across Models",
                                  fluidRow(
                                    div( style = "margin-top: 15px;margin-left:0px; ",
                                         column(4,
                                                selectInput(ns("selected_measure_multiple_model"), "Select Statistics",
                                                            choices = c("Mean"="mean",
                                                                        "Coefficient of Variation"= "cv",
                                                                        "Width of 95% Credible Interval"="CI.width",
                                                                        "Exceedance Probability"="exceed_prob"))
                                         ),
                                         div(id = 'big_slider',
                                             column(4,
                                                    uiOutput(ns("choose_thresh_multiple_model"))
                                             ))
                                    )),
                                  div( style = "margin-top: -5px; ",
                                  tags$hr(style="border-top-color: #E0E0E0;") # (style="border-top: 2px solid #707070;")
                                  ),
                                  fluidRow(
                                    column(12,
                                           div(style = " margin: auto;float: left;",
                                               uiOutput(ns("text_display_multiple_model"))
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    div(style = "width: 100%; max-width: 800px;margin-top:10px;",
                                        column(12,
                                               div(DT::DTOutput(ns('select_model_table')), class = "model-checkbox-table"),
                                        )
                                    )),

                                  # Action Button at the bottom
                                  fluidRow(
                                    div(style = "display: flex;margin-top:10px;",

                                        ### toggle input for interactive map
                                        div(style = "margin-top: 10px; margin-left: 15px;max-width: 250px;",
                                            shinyWidgets::materialSwitch(inputId = ns("mapType2"), label = HTML("<strong>Interactive Map Enabled</strong>"),
                                                                         status = "success",value =T)
                                        ),
                                        div(style = "margin-left: 15px;margin-right: 15px;",
                                            tags$div(style = "margin-top: 5px; font-size: 18px;", # Larger font size for better readability
                                                     "Click ",
                                                     tags$b(actionButton(ns("plot_multiple_model"), "Generate Plot",
                                                                         style = "padding: 0px 3px 3px ;font-size: 18px;", class = "btn-primary")),
                                                     " to produce plot and/or apply changes."
                                            )
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           div(
                                             id = "map-container",
                                             style = "width: min(98%, 1000px); margin-top: 20px;margin-bottom: 10px;",
                                             uiOutput(ns("map_multiple_model"))
                                           )
                                    )),
                                  fluidRow(
                                    column(12,
                                           div( style = "width: min(98%, 1000px); margin-top: 10px; display: flex; justify-content: center;",
                                                uiOutput(ns("download_multiple_model_ui"))
                                           )
                                    )
                                  )
                         )
             )
      )
    )
  )
}

#' res_visual_multiple_maps Server Functions
#'
#' @noRd
mod_res_visual_multiple_maps_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("Package 'patchwork' is required for this function. Please install it with install.packages('patchwork').")
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

    ###############################################################
    ### determine interactive vs static map based on user selection
    ###############################################################

    observeEvent(input$mapType,{

      CountryInfo$display_interactive(input$mapType)

    })

    observeEvent(CountryInfo$display_interactive(),{

      interactive_map <- CountryInfo$display_interactive()
      shinyWidgets::updateMaterialSwitch(session=session, inputId="mapType", value = interactive_map)

    })


    ###############################################################
    ### UI updates: single model, multiple statistics
    ###############################################################

    ### determine which UI to present plot
    output$map_single_model <- renderUI({
      if (input$mapType) {  # if TRUE, show interactive map
        uiOutput(ns("map_single_model_interactive"))
      } else {  # if FALSE, show static map
        plotOutput(ns("map_single_model_static"),height = "auto")
      }
    })


    ### update choices of admin levels
    GADM.levels <- reactive({ CountryInfo$GADM_analysis_levels()    })

    observeEvent(GADM.levels(), {
      adm.choice <- GADM.levels()
      adm.choice <- adm.choice[adm.choice!='National']
      updateSelectInput(inputId = "selected_adm",
                        choices = adm.choice)
    })

    ### update choices of statistics
    observeEvent(input$selected_method,{

      if (is.null(input$selected_method) || length(input$selected_method)==0) {
        return(NULL)
      }

      if(FALSE){
      if(input$selected_method=='Direct'){
        if('exceed_prob' %in% input$selected_stats){
          tmp.selected <- input$selected_stats[input$selected_stats !='exceed_prob']
        }else{tmp.selected <-input$selected_stats}
        updateCheckboxGroupInput(session, "selected_stats",
                                 choices = c("Mean"="mean",
                                             "Coefficient of Variation"= "cv",
                                             "Width of 95% Credible Interval"="CI.width"),
                                 inline = TRUE,
                                 selected=tmp.selected)
      }else{

      }
      }

      tmp.selected <-input$selected_stats
      updateCheckboxGroupInput(session, "selected_stats",
                               choices = c("Mean"="mean",
                                           "Coefficient of Variation"= "cv",
                                           "Width of 95% Credible Interval"="CI.width",
                                           "Exceedance Probability"="exceed_prob"),
                               inline = TRUE,
                               selected=tmp.selected)



    })


    ### initialize exceedance probability slider bar
    output$choose_thresh_1 <- renderUI({

      req(input$selected_stats)

      if (is.null(input$selected_stats) || length(input$selected_stats)==0) {
        return(NULL)
      }

      if ('exceed_prob' %in% input$selected_stats) {

        ### set initial threshold to national average
        tmp.natl.res <- AnalysisInfo$Natl_res()
        if(!is.null(tmp.natl.res)){
          initial.val <- round(tmp.natl.res$direct.est,digits=2)
        }else{
          initial.val=0.5
        }

        return(      sliderInput(ns("selected_threshold_1"),
                                 "Select a threshold for exceedance probability",
                                 min = 0,
                                 max = 1,
                                 value = initial.val,  # Default initial value
                                 step = 0.01)
        )
      } else {  # if FALSE, show nothing
        return(NULL)
      }
    })


    ### text display for models not fitted
    output$single_model_text_display <- renderUI({

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

        HTML(paste0(
          "<p style='font-size: large;'>",
          "Results for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span>",
          " level are ",
          "<strong style='color: red;'>NOT</strong>",
           " available. Please make sure the model has been successfully fitted.",
          "</p> <hr style='border-top-color: #E0E0E0;'>"
        ))

      }else{

        text_display <- HTML(paste0(
          "<p style='font-size: large;'>",
          "Presenting map for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> level.",
          "</p>"
        ))

        return(NULL)

      }


    })

    ###############################################################
    ### plot single model, multiple statistics
    ###############################################################


    output$map_single_model_interactive <- renderUI({
      return(single.model.interactive.output())

    })

    output$map_single_model_static <- renderPlot({
      return(single.model.static.output())

    },height = function(){
      #message(single.model.static.height())
      single.model.static.height()})

    single.model.interactive.output <- reactiveVal(NULL)
    single.model.static.output <- reactiveVal(NULL)
    single.model.static.height <- reactiveVal(500)

    # reset plot new country/indicator/survey is selected
    meta_snapshot <- reactive({
      list(
        country_selected = CountryInfo$country(),
        year_selected = CountryInfo$svyYear_selected(),
        indicator_selected = CountryInfo$svy_indicator_var()
      )
    })
    observeEvent(meta_snapshot(),{

      single.model.interactive.output(NULL)
      single.model.static.output(NULL)
      single.model.static.height(500)
    })


    observeEvent(input$plot_single_model,{

      tmp.single.interactive.map <- leaflet::leaflet()

      if(CountryInfo$use_basemap()=='OSM'&&CountryInfo$display_interactive()==T){
        tmp.single.interactive.map<- tmp.single.interactive.map %>% leaflet::addTiles()
      }

      ### return empty map if no subnational level selected
      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        single.model.interactive.output(tmp.single.interactive.map)
        single.model.static.output(NULL)
        return(NULL)
      }

      ### return empty map if no statistic measure selected
      if (is.null(input$selected_stats)||length(input$selected_stats) == 0) {
        single.model.interactive.output(tmp.single.interactive.map)
        single.model.static.output(NULL)
        return(NULL)
      }

      ### extract selections
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      selected_measure_vec <- input$selected_stats

      if(CountryInfo$use_preloaded_Madagascar()){
        # AnalysisInfo$model_res_list(mdg.ex.model.res)
      }

      ### initialize parameters
      model_res_all <- AnalysisInfo$model_res_list()
      strat.gadm.level <- CountryInfo$GADM_strata_level()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]

      ### do not plot if no results produced for the selection
      if(is.null(model_res_selected)|selected_adm=='National'){
        single.model.interactive.output(tmp.single.interactive.map)
        return(NULL)
      }


      ######################
      ### interactive map
      ######################

      if(CountryInfo$display_interactive()==T){
        ### determine hatching density by country size
        hatching.density.country <- tryCatch({
          country.area <- as.numeric(sf::st_area(CountryInfo$GADM_list_smoothed()[["National"]])/1e6)
          hatching.density.country <- round(sqrt(9e07/country.area)/1.3)
          hatching.density.country

        },error = function(e) {

          return(12)
          #hatching.density.country <- 12

        })

        tmp.map.list <- list()

        for(selected_measure in selected_measure_vec){

          #message(paste0('single model: preparing map for ',selected_measure))

          if(selected_measure=='exceed_prob'){selected_threshold <- input$selected_threshold_1}else{selected_threshold=NULL}

          ### present measures as full name
          measure_match <- c(
            "cv" = "Coefficient <br>of variation",
            "mean" = "Mean",
            "CI.width" = "Width of <br>95% CI",
            "exceed_prob" = "Exceedance <br>probability"
          )


          one.interactive.plot <-  tryCatch({
            suppressWarnings(prevMap.leaflet(res.obj = model_res_selected,
                                             gadm.shp = CountryInfo$GADM_list_smoothed()[[selected_adm]],
                                             model.gadm.level = admin_to_num(selected_adm),
                                             strata.gadm.level = CountryInfo$GADM_strata_level(),
                                             value.to.plot =selected_measure,
                                             legend.label = measure_match[selected_measure],
                                             hatching.density = hatching.density.country,
                                             map.title=NULL,
                                             threshold.p = selected_threshold,
                                             use.basemap = CountryInfo$use_basemap(),
                                             legend.color.reverse=CountryInfo$legend_color_reverse()))
          },error = function(e) {
            message(e$message)
            return(NULL)
          })


          if(!is.null(one.interactive.plot)){
            tmp.map.list[[selected_measure]]=one.interactive.plot
          }

        }

        #saveRDS(tmp.map.list,'single_model_res.rds')
        sync.plot <- leafsync::latticeView(tmp.map.list,ncol = 2,
                                           sync = "none")
        single.model.interactive.output(sync.plot)
      }

      #single.model.interactive.output(tmp.map.list[[1]])

      ######################
      ### static map
      ######################

      if(CountryInfo$display_interactive()==F){

        tmp.map.list <- list()

        for(selected_measure in selected_measure_vec){

          message(paste0('single model: preparing map for ',selected_measure))

          if(selected_measure=='exceed_prob'){selected_threshold <- input$selected_threshold_1}else{selected_threshold=NULL}

          ### present measures as full name
          measure_match <- c(
            "cv" = "Coefficient \n of variation",
            "mean" = "Mean",
            "CI.width" = "Width of \n 95% CI",
            "exceed_prob" = "Exceedance \n probability"
          )

          one.static.plot <-  tryCatch({
            tmp.plot <- suppressWarnings(prevMap.static(res.obj = model_res_selected,
                                             gadm.shp = CountryInfo$GADM_list_smoothed()[[selected_adm]],
                                             model.gadm.level = admin_to_num(selected_adm),
                                             strata.gadm.level = CountryInfo$GADM_strata_level(),
                                             value.to.plot =selected_measure,
                                             legend.label = measure_match[selected_measure],
                                             map.title=NULL,
                                             threshold.p = selected_threshold))

            tmp.plot <- tmp.plot+
              ggplot2::theme (legend.text=ggplot2::element_text(size=12),
                              legend.title = ggplot2::element_text(size=14),
                              strip.text.x = ggplot2::element_text(size = 12),
                              legend.key.height = ggplot2::unit(1,'cm'))


          },error = function(e) {
            message(e$message)
            return(NULL)
          })


          if(!is.null(one.static.plot)){
            tmp.map.list[[selected_measure]]=one.static.plot
          }

        }

        plot.grid <- tryCatch({patchwork::wrap_plots(tmp.map.list, ncol = 2)
        },error = function(e) {
          message(e$message)
          return(NULL)
        })

        if(is.null(plot.grid)||length(tmp.map.list)<1){
          single.model.static.output(NULL)
          single.model.static.height(500)
          return(NULL)
        }



        if(length(tmp.map.list)==1){
          single.model.static.output(tmp.map.list[[1]])
        }else{single.model.static.output(plot.grid)}

        # Calculate the bounding box
        bbox <- sf::st_bbox(CountryInfo$GADM_list_smoothed()[['National']])

        # Extract the dimensions of the bbox
        width = bbox["xmax"] - bbox["xmin"]
        height = bbox["ymax"] - bbox["ymin"]

        # Calculate the height-to-width ratio
        height_to_width_ratio = height / width

        tmp.height <- max(1,height_to_width_ratio)*300*(1+(ceiling(length(tmp.map.list)/2)-1)*0.6)
        tmp.height <- round(tmp.height/10)*10
        #tmp.height <- 500+ (round(length(tmp.map.list)/2+0.1)-1)*300
        message(tmp.height)

        #single.model.static.height(500+ (round(length(tmp.map.list)/2+0.1)-1)*300)
        single.model.static.height(as.numeric(tmp.height))

      }

    })



    ###############################################################
    ### downloads: single model, multiple statistics
    ###############################################################

    ### update download button
    output$download_single_model_ui <- renderUI({

      #message(paste0('Map type is:',CountryInfo$display_interactive()))
      if (CountryInfo$display_interactive()==T) {  # HTML download
        if(is.null(single.model.interactive.output())){return(NULL)}else{
          uiOutput(ns("download_interactive_p1_text_display"))
        }

      } else {
        if(is.null(single.model.static.output())){return(NULL)}else{

          downloadButton(ns("download_static_p1"), "Download as PDF", icon = icon("download"),
                         class = "btn-primary")
        }
      }
    })


    ### update text for download button
    output$download_interactive_p1_text_display <- renderUI({
      text_display <- HTML(paste0(
        "<p style='font-size: large;'>",
        "Interactive multiple maps cannot be downloaded. Please check out non-interactive maps.",
        "</p>"
      ))

      return(text_display)

    })


    output$download_static_p1 <- downloadHandler(
      filename = function() {

        ### informative file name
        file.prefix <- paste0(CountryInfo$country(),'_',
                              input$selected_adm,'_',
                              input$selected_method)
        file.prefix <- gsub("[-.]", "_", file.prefix)

        #message(paste0(file.prefix,'_multi_Map.pdf'))
        return(paste0(file.prefix,'_multi_statistics_Map.pdf'))

      },

      content = function(file) {
        # Create the PDF
        grDevices::pdf(file, width = 10, height = 10)  # Set width and height of the PDF
        print(single.model.static.output())  # Print the plot to the PDF
        grDevices::dev.off()  # Close the PDF
      }
    )




    ###############################################################
    ### UI updates: multiple model, same statistics
    ###############################################################

    ### select the probability for exceedance probability map
    output$choose_thresh_multiple_model <- renderUI({
      req(input$selected_measure_multiple_model)

      if (input$selected_measure_multiple_model=='exceed_prob') {

        ### set initial threshold to national average
        tmp.natl.res <- AnalysisInfo$Natl_res()
        if(!is.null(tmp.natl.res)){
          initial.val <- round(tmp.natl.res$direct.est,digits=2)
        }else{
          initial.val=0.5
        }

        return(      sliderInput(ns("selected_threshold_2"),
                                 "Select Threshold",
                                 min = 0,
                                 max = 1,
                                 value = initial.val,  # Default initial value
                                 step = 0.01)
        )
      } else {  # if FALSE, show nothing
        return(NULL)
      }
    })


    ### text display for model selection
    output$text_display_multiple_model <- renderUI({

      HTML(paste0(
        "<p style='font-size: large;'>",
        "Select successfully fitted models from the table below for comparison.",
        "</p>"
      ))


    })

    ### update widgets to store selection on interactive maps
    observeEvent(input$mapType2,{

      CountryInfo$display_interactive(input$mapType2)

    })

    observeEvent(CountryInfo$display_interactive(),{

      interactive_map <- CountryInfo$display_interactive()
      shinyWidgets::updateMaterialSwitch(session=session, inputId="mapType2", value = interactive_map)

    })

    ### determine which UI to present plot
    output$map_multiple_model <- renderUI({
      if (CountryInfo$display_interactive()) {  # if TRUE, show interactive map
        uiOutput(ns("map_multiple_model_interactive"))
      } else {  # if FALSE, show static map
        plotOutput(ns("map_multiple_model_static"),height = "auto")
      }
    })

    # Render the checkout table
    method_names <- c('Direct Estimates','Area-level Model','Unit-level Model')

    row_names <- c("Direct", "FH", "Unit")
    nrows <- length(row_names)

    col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
    ncols <- reactive({ length(col_names()) })


    output$select_model_table <- DT::renderDataTable({

      model_res_all <- AnalysisInfo$model_res_list() ## react if model results change

      # Convert the reactive matrix to a regular matrix to create the dataframe
      df <- as.data.frame(matrix(vector('list', nrows * ncols()), nrow = nrows, dimnames = list(row_names, col_names())))

      num_fitted_model <- 0
      # Populate the dataframe with checkbox inputs
      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {

          tmp.method <- row_names[i]
          tmp.adm <- col_names()[j]
          #message(paste0('present model from method ',tmp.method, ' at ', tmp.adm,' level.'))
          model_res_selected <- model_res_all[[tmp.method]][[tmp.adm]]

          if(is.null(model_res_selected)){
            ### gray out selection if no results available for the model
            df[i, j] <- as.character(HTML('<div style="display: flex; justify-content: center;
                                          align-items: center; height: 100%;"><input type="checkbox" disabled="disabled"
                                          style="margin-top: 10px;margin-bottom:10px;margin-left: -7px"></div>'))

          }else{
            df[i, j] <- as.character(shiny::checkboxInput(inputId = ns(paste0("cb_", i, "_", j)),
                                                          label = NULL))
            num_fitted_model <- num_fitted_model+1
          }


        }
      }

      if(num_fitted_model==0){
        multiple.model.interactive.output(NULL)
      }

      if( 'National' %in%  col_names()){
        df[1, which( col_names()=='National')] <- as.character(HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100%;"><input type="checkbox" disabled="disabled" style="margin-top: 10px;margin-bottom:10px;margin-left: -7px"></div>'))
        df[2, which( col_names()=='National')] <- as.character(HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100%;"><input type="checkbox" disabled="disabled" style="margin-top: 10px;margin-bottom:10px;margin-left: -7px"></div>'))
        df[3, which( col_names()=='National')] <- as.character(HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100%;"><input type="checkbox" disabled="disabled" style="margin-top: 10px;margin-bottom:10px;margin-left: -7px"></div>'))
      }


      rownames(df) <- method_names
      # Return the DataTable
      DT::datatable(df, escape = FALSE, selection = 'none',
                    options = list(dom = 't', paging = FALSE, ordering = FALSE,
                                   #autoWidth = TRUE,
                                   #columnDefs = list(list(width = '150px', targets = "_all")),
                                   preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                   drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); }')))
    }, server = FALSE)



    ### track user's selection on models
    multiple_model_selection <- reactiveVal(NULL)

    observe({

      matrix_status <- matrix(FALSE, nrow = nrows, ncol = ncols(), dimnames = list(row_names, col_names()))
      for (i in seq_len(nrows)) {
        for (j in seq_len(ncols())) {
          inputId <- paste0("cb_", i, "_", j)
          matrix_status[i, j] <- input[[inputId]] %||% FALSE
        }
      }

      multiple_model_selection(matrix_status)

      #message(sum(matrix_status,na.rm=T))
    })


    ###############################################################
    ### plot multiple models admin, same statistics
    ###############################################################

    output$map_multiple_model_interactive <- renderUI({
      return(multiple.model.interactive.output())

    })


    output$map_multiple_model_static <- renderPlot({
      return(multiple.model.static.output())

    },height = function(){
      #message(multiple.model.static.height())
      multiple.model.static.height()})


    multiple.model.interactive.output <- reactiveVal(NULL)
    multiple.model.static.output <- reactiveVal(NULL)
    multiple.model.static.height <- reactiveVal(500)

    # reset plot new country/indicator/survey is selected
    meta_snapshot <- reactive({
      list(
        country_selected = CountryInfo$country(),
        year_selected = CountryInfo$svyYear_selected(),
        indicator_selected = CountryInfo$svy_indicator_var()
      )
    })
    observeEvent(meta_snapshot(),{
      multiple.model.interactive.output(NULL)
      multiple.model.static.output(NULL)
      multiple.model.static.height(500)

    })

    observeEvent(input$plot_multiple_model,{

      tmp.multiple.interactive.map <- leaflet::leaflet()

      if(CountryInfo$use_basemap()=='OSM'&&CountryInfo$display_interactive()==T){
        tmp.multiple.interactive.map<- tmp.multiple.interactive.map %>% leaflet::addTiles()
      }
      if(CountryInfo$use_preloaded_Madagascar()){
        # AnalysisInfo$model_res_list(mdg.ex.model.res)
      }


      selection_mat <- multiple_model_selection()
      ### return empty map if no model selected
      if(sum(selection_mat,na.rm=T)==0){
        multiple.model.interactive.output(tmp.multiple.interactive.map)
        multiple.model.static.output(NULL)
        return(NULL)
      }

      ### extract selections
      selected_measure <- input$selected_measure_multiple_model
      #message(selected_measure)

      if(selected_measure=='exceed_prob'){
        selected_threshold <- input$selected_threshold_2
        #message(selected_threshold)
      }else{
        selected_threshold=NULL}


      ### initialize parameters
      value.to.plot <- selected_measure
      col_names_tmp <- col_names()
      model_res_all <- AnalysisInfo$model_res_list()
      strat.gadm.level <- CountryInfo$GADM_strata_level()


      result_list <- list()

      for (j in seq_len(ncols())) {


        tmp.adm <- col_names_tmp[j]
        tmp.adm.num <- admin_to_num(tmp.adm)

        for (i in seq_len(nrows)) {

          tmp.method <- row_names[i]

          if(selection_mat[i,j]==1){

            #message(paste0('now collecting ',tmp.method, ' model at ',tmp.adm))
            tmp.model <- model_res_all[[tmp.method]][[tmp.adm]]

            if(!is.null(tmp.model)){
              result_list[[length(result_list)+1]] <- model_res_all[[tmp.method]][[tmp.adm]]
            }else{
              message(paste0(tmp.method, ' model at ',tmp.adm,' is empty. Something is wrong.'))
            }

          }
        }
      }

      if(length(result_list)==0){
        message('Model is fitted but no result to be plotted. Something is wrong.')
        multiple.model.interactive.output(tmp.multiple.interactive.map)
        multiple.model.static.output(NULL)
        return(NULL)
      }

      ### determine the consistent range across sub plots
      if(selected_measure!='exceed_prob'){
        range_all_model <- range_across_model(result.model.list=result_list,value.to.plot = value.to.plot)
      }else{range_all_model <- c(0,1)}

      #message(range_all_model)


      ######################
      ### interactive map
      ######################

      if(CountryInfo$display_interactive()==T){

        # present measures as full name
        measure_match <- c(
          "cv" = "Coefficient <br>of variation",
          "mean" = "Mean",
          "CI.width" = "Width of <br>95% CI",
          "exceed_prob" = "Exceedance <br>probability"
        )

        hatching.density.country <- tryCatch({
          country.area <- as.numeric(sf::st_area(CountryInfo$GADM_list_smoothed()[["National"]])/1e6)
          hatching.density.country <- round(sqrt(9e07/country.area)/1.3)
          hatching.density.country

        },error = function(e) {

          return(12)
          #hatching.density.country <- 12

        })


        interactive.plot.list <- list()

        for (j in seq_len(ncols())) {

          tmp.adm <- col_names_tmp[j]

          for (i in seq_len(nrows)) {

            tmp.method <- row_names[i]

            if(selection_mat[i,j]==1){

              #message(paste0('now collecting ',tmp.method, ' model at ',tmp.adm))
              tmp.model.res <- model_res_all[[tmp.method]][[tmp.adm]]

              tmp.interactive.plot <-  tryCatch({
                tmp.plot <- suppressWarnings(prevMap.leaflet(res.obj = tmp.model.res,
                                                 gadm.shp = CountryInfo$GADM_list_smoothed()[[tmp.adm]],
                                                 model.gadm.level = admin_to_num(tmp.adm),
                                                 strata.gadm.level = CountryInfo$GADM_strata_level(),
                                                 value.to.plot = value.to.plot,
                                                 legend.label = measure_match[value.to.plot],
                                                 hatching.density = hatching.density.country,
                                                 map.title=NULL,
                                                 threshold.p = selected_threshold,
                                                 value.range = range_all_model,
                                                 use.basemap = CountryInfo$use_basemap(),
                                                 legend.color.reverse=CountryInfo$legend_color_reverse()))
                tmp.plot <- tmp.plot  %>% leaflet::addControl(html=paste0("<h4 style='text-align: center; margin: 10px;'>",
                                                           tmp.adm,': ',tmp.method,
                                                           "</h4>"), position = "topright")

              },error = function(e) {
                message(e$message)
                return(NULL)
              })

              if(!is.null(tmp.interactive.plot)){
                interactive.plot.list[[length(interactive.plot.list)+1]] <- tmp.interactive.plot
              }else{
                message(paste0(tmp.method, ' model at ',tmp.adm, ' not successfully plotted for ',value.to.plot))
              }


            }
          }
        }

        # Calculate the bounding box
        if(FALSE){
        bbox <- sf::st_bbox(CountryInfo$GADM_list_smoothed()[['National']])

        # Extract the dimensions of the bbox
        width = bbox["xmax"] - bbox["xmin"]
        height = bbox["ymax"] - bbox["ymin"]

        # Calculate the height-to-width ratio
        height_to_width_ratio = height / width

        layout.ncol <- calculate_columns(length(interactive.plot.list),height_to_width_ratio)
        }

        #saveRDS(interactive.plot.list,'multiple_model_plot.rds')
        sync.plot <- leafsync::latticeView(interactive.plot.list,ncol = 2,
                                          sync = "none")
        multiple.model.interactive.output(sync.plot)

      }



      ######################
      ### static map
      ######################

      if(CountryInfo$display_interactive()==F){

        measure_match <- c(
          "cv" = "Coefficient \n of variation",
          "mean" = "Mean",
          "CI.width" = "Width of \n 95% CI",
          "exceed_prob" = "Exceedance \n probability"
        )


        static.plot.list <- list()

        for (j in seq_len(ncols())) {

          tmp.adm <- col_names_tmp[j]

          for (i in seq_len(nrows)) {

            tmp.method <- row_names[i]

            if(selection_mat[i,j]==1){

              #message(paste0('now collecting ',tmp.method, ' model at ',tmp.adm))
              tmp.model.res <- model_res_all[[tmp.method]][[tmp.adm]]

              tmp.static.plot <-  tryCatch({

                tmp.plot <- suppressWarnings(prevMap.static(res.obj = tmp.model.res,
                                                             gadm.shp = CountryInfo$GADM_list_smoothed()[[tmp.adm]],
                                                             model.gadm.level = admin_to_num(tmp.adm),
                                                             strata.gadm.level = CountryInfo$GADM_strata_level(),
                                                             value.to.plot = value.to.plot,
                                                             legend.label = measure_match[value.to.plot],
                                                             map.title=NULL,
                                                             threshold.p = selected_threshold,
                                                             value.range = range_all_model
                                                            ))

                tmp.plot$data$model_des <- paste0(tmp.adm,': ',tmp.method)
                tmp.plot <- tmp.plot+ ggplot2::facet_wrap(ggplot2::vars(model_des))+
                  ggplot2::theme (legend.text=ggplot2::element_text(size=13),
                                  legend.title = ggplot2::element_text(size=15),
                                  strip.text.x = ggplot2::element_text(size = 13),
                                  legend.key.height = ggplot2::unit(1.2,'cm'),
                                  strip.text = ggplot2::element_text(size=16))



              },error = function(e) {
                message(e$message)
                return(NULL)
              })

              if(!is.null(tmp.static.plot)){
                static.plot.list[[length(static.plot.list)+1]] <- tmp.static.plot
              }else{
                message(paste0(tmp.method, ' model at ',tmp.adm, ' not successfully plotted for ',value.to.plot))
              }


            }
          }
        }

        # Calculate the bounding box
        bbox <- sf::st_bbox(CountryInfo$GADM_list_smoothed()[['National']])

        # Extract the dimensions of the bbox
        width = bbox["xmax"] - bbox["xmin"]
        height = bbox["ymax"] - bbox["ymin"]

        # Calculate the height-to-width ratio
        height_to_width_ratio = height / width

        # determine number of columns
        layout.ncol <- calculate_columns(length(static.plot.list),height_to_width_ratio)


        static.plot.grid <- tryCatch({
          patchwork::wrap_plots(static.plot.list, ncol = layout.ncol)+
            patchwork::plot_layout(guides = "collect") & ggplot2::theme(legend.position = "right")
        },error = function(e) {
          message(e$message)
          return(NULL)
        })


        if(is.null(static.plot.grid)||length(static.plot.list)<1){
          multiple.model.static.output(NULL)
          multiple.model.static.height(800)
          return(NULL)
        }


        if(length(static.plot.list)==1){
          multiple.model.static.output(static.plot.list[[1]])
        }else{multiple.model.static.output(static.plot.grid)}

        tmp.height <- max(1,height_to_width_ratio)*300*(1+(ceiling(length(static.plot.list)/layout.ncol)-1)*0.6)
        tmp.height <- round(tmp.height/10)*10

        multiple.model.static.height(as.numeric(tmp.height))




      }


      ###############################################################
      ### downloads: multiple model, single statistics
      ###############################################################

      ### update download button
      output$download_multiple_model_ui <- renderUI({

        #message(paste0('Map type is:',CountryInfo$display_interactive()))
        if (CountryInfo$display_interactive()==T) {  # HTML download
          if(is.null(multiple.model.interactive.output())){return(NULL)}else{
            uiOutput(ns("download_interactive_p2_text_display"))
          }

        } else {
          if(is.null(multiple.model.static.output())){return(NULL)}else{

            downloadButton(ns("download_static_p2"), "Download as PDF", icon = icon("download"),
                           class = "btn-primary")
          }
        }
      })


      ### update text for download button
      output$download_interactive_p2_text_display <- renderUI({
        text_display <- HTML(paste0(
          "<p style='font-size: large;'>",
          "Interactive multiple maps cannot be downloaded. Please check out non-interactive maps.",
          "</p>"
        ))

        return(text_display)

      })


      output$download_static_p2 <- downloadHandler(
        filename = function() {

          ### informative file name
          file.prefix <- paste0(CountryInfo$country(),'_',
                                input$selected_measure_multiple_model)
          file.prefix <- gsub("[-.]", "_", file.prefix)

          #message(paste0(file.prefix,'_multi_Map.pdf'))
          return(paste0(file.prefix,'_multi_model_Map.pdf'))

        },

        content = function(file) {
          # Create the PDF
          grDevices::pdf(file, width = 10, height = round(multiple.model.static.height()*1.8/100))  # Set width and height of the PDF
          print(multiple.model.static.output())  # Print the plot to the PDF
          grDevices::dev.off()  # Close the PDF
        }
      )










    })





  })
}

## To be copied in the UI
# mod_res_visual_multiple_maps_ui("res_visual_multiple_maps_1")

## To be copied in the server
# mod_res_visual_multiple_maps_server("res_visual_multiple_maps_1")
