#' result_visual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_visual_prev_map_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    shinyWidgets::chooseSliderSkin("Flat", color = "#b0c4de"),
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
    "))
    ),

    div(class = "module-title",
        h4("Subnational Results Mapping")
    ),

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
                         choices = c("Direct Estimates"="Direct",
                                     "Area-level Model"= "FH", "Unit-level Model"="Unit"))
      ),
      column(4,
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      )
    ),

    fluidRow(
      column(4,
             selectInput(ns("selected_measure"), "Select Statistics",
                         choices = c("Mean"="mean",
                                     "Coefficient of Variation"= "cv",
                                     "Width of 95% Credible Interval"="CI.width",
                                     "Exceedance Probability"="exceed_prob"))
      ),
      div(id = 'big_slider',
      column(4,
             uiOutput(ns("choose_prob"))
      ))
    ),

    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("text_display"))
             )
      )
    ),
    fluidRow(
      column(12,
             #tags$h4("Map for estimates from selected model"),
             hr(style="border-top-color: #E0E0E0;"), # More subtle horizontal line
             shinyWidgets::materialSwitch(inputId = ns("PrevmapType"), label = "Interactive Map Enabled",
                                          status = "success",value =T),

             div(
               id = "map-container",
               style = "width: max(50%, 600px); margin-top: 20px;",
               uiOutput(ns("prev_map"))
               #leaflet::leafletOutput(ns("prev_map"))
             ),
             div( style = "width: max(50%, 600px); margin-top: 20px; display: flex; justify-content: center;",
                  uiOutput(ns("download_button_ui"))

                  #downloadButton(ns("dl"), "Download as HTML", icon = icon("download"),
                  #            class = "btn-primary")
             )
      )
    ),


  )

}


#' result_visual Server Functions
#'
#' @noRd
mod_res_visual_prev_map_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo){
  moduleServer(id, function(input, output, session) {
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

    ###############################################################
    ### initialize parameters
    ###############################################################


    row_names <- c("Direct", "FH", "Unit")
    nrows <- length(row_names)

    col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
    ncols <- reactive({ length(col_names()) })

    observeEvent(col_names(), {
      adm.choice <- col_names()
      adm.choice <- adm.choice[adm.choice!='National']
      updateSelectInput(inputId = "selected_adm",
                        choices = adm.choice)
    })


    ### select the probability for exceedance probability map
    output$choose_prob <- renderUI({
      req(input$selected_measure)

      if (input$selected_measure=='exceed_prob') {

        ### set initial threshold to national average
        tmp.natl.res <- AnalysisInfo$Natl_res()
        if(!is.null(tmp.natl.res)){
          initial.val <- round(tmp.natl.res$direct.est,digits=2)
        }else{
          initial.val=0.5
        }

        return(      sliderInput(ns("selected_threshold"),
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


    ###############################################################
    ### determine interactive vs static map based on user selection
    ###############################################################

    observeEvent(input$PrevmapType,{

      CountryInfo$display_interactive(input$PrevmapType)

    })

    observeEvent(CountryInfo$display_interactive(),{

      interactive_map <- CountryInfo$display_interactive()
      shinyWidgets::updateMaterialSwitch(session=session, inputId="PrevmapType", value = interactive_map)

    })

    ### determine which UI to present plot

    output$prev_map <- renderUI({
      if (input$PrevmapType) {  # if TRUE, show interactive map
        leaflet::leafletOutput(ns("prev_map_interactive"))
      } else {  # if FALSE, show static map
        plotOutput(ns("prev_map_static"))
      }
    })

    output$download_button_ui <- renderUI({
      if (input$PrevmapType) {  # HTML download
        if(is.null(prev.map.interactive.output())){return(NULL)}else{
          uiOutput(ns("download_interactive_p1_text_display"))
        }

        } else {
          downloadButton(ns("download_static"), "Download as PDF", icon = icon("download"),
                         class = "btn-primary")
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


    ###############################################################
    ### Text display
    ###############################################################

    output$text_display <- renderUI({

      ### return empty map if no subnational level selected
      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### extract selections
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      selected_measure <- input$selected_measure


      ### no exceedance probability map for direct estimates
      if(FALSE){
        if(selected_measure=='exceed_prob' &selected_method=='Direct'){

          text_display <- HTML(paste0(
            "<p style='font-size: large;'>",
            "Exceedance probabilty map cannot be produced for Direct Estimates Model.",
            "</p>"
          ))

          return(text_display)
        }
      }


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
          "</p>"
        ))

      }else{

        HTML(paste0(
          "<p style='font-size: large;'>",
          "Presenting map for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> level.",
          "</p>"
        ))

      }


    })

    ###############################################################
    ### prepare maps
    ###############################################################

    ### interactive map

    prev.map.interactive.output <- reactiveVal(NULL)

    output$prev_map_interactive <- leaflet::renderLeaflet({

      ### initialize base map
      prev.interactive.plot <- leaflet::leaflet()

      if(CountryInfo$use_basemap()=='OSM'){
        prev.interactive.plot<- prev.interactive.plot %>% leaflet::addTiles()
      }

      ### return empty map if no subnational level selected
      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(prev.interactive.plot)
      }

      ### extract selections
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      selected_measure <- input$selected_measure


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}


      ### initialize parameters
      model_res_all <- AnalysisInfo$model_res_list()
      strat.gadm.level <- CountryInfo$GADM_strata_level()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]


      ### do not plot if no results produced for the selection
      if(is.null(model_res_selected)|selected_adm=='National'){
        return(prev.interactive.plot)
      }

      ### determine hatching density by country size
      hatching.density.country <- tryCatch({
          country.area <- as.numeric(sf::st_area(CountryInfo$GADM_list_smoothed()[["National"]])/1e6)
          hatching.density.country <- round(sqrt(9e07/country.area))
          hatching.density.country

        },error = function(e) {

          return(12)
          #hatching.density.country <- 12

        })

      if(selected_measure=='exceed_prob'){
        req(input$selected_threshold)
        selected_threshold <- input$selected_threshold
      }else{
        selected_threshold=NULL
      }

      prev.interactive.plot <-  tryCatch({
        suppressWarnings(prevMap.leaflet(res.obj = model_res_selected,
                                    gadm.shp = CountryInfo$GADM_list_smoothed()[[selected_adm]],
                                    model.gadm.level = admin_to_num(selected_adm),
                                    strata.gadm.level = CountryInfo$GADM_strata_level(),
                                    value.to.plot =selected_measure,
                                    legend.label = 'Estimates',
                                    hatching.density = hatching.density.country,
                                    map.title=NULL,
                                    threshold.p = selected_threshold,
                                    use.basemap = CountryInfo$use_basemap(),
                                    legend.color.reverse=CountryInfo$legend_color_reverse()))
      },error = function(e) {
        message(e$message)
        return(NULL)
      })

      prev.map.interactive.output(prev.interactive.plot)
      #message(paste0(input$prev_map$lng,'_',input$map_center$lat,'_', input$map_zoom))
      return(prev.interactive.plot)

    })

    #output$check_ID <-renderPrint({reactiveValuesToList(input)})

    #output$prev_map_static <- ()



    ### static map

    prev.map.static.output <- reactiveVal(NULL)

    output$prev_map_static <- renderPlot({

      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### initialize parameters
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      selected_measure <- input$selected_measure


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}


      ### load results
      model_res_all <- AnalysisInfo$model_res_list()

      strat.gadm.level <- CountryInfo$GADM_strata_level()

      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]

      ### plot
      if(is.null(model_res_selected)|selected_adm=='National'){

        return(NULL)

      }

      if(selected_measure=='exceed_prob'){selected_threshold <- input$selected_threshold}else{selected_threshold=NULL}

      prev.static.plot <-  tryCatch({

        prev.static.plot <- suppressWarnings(prevMap.static(res.obj = model_res_selected,
                                                            gadm.shp = CountryInfo$GADM_list_smoothed()[[selected_adm]],
                                                            model.gadm.level = admin_to_num(selected_adm),
                                                            strata.gadm.level = CountryInfo$GADM_strata_level(),
                                                            value.to.plot =selected_measure,
                                                            threshold.p = selected_threshold,
                                                            legend.label = 'Estimates',
                                                            color.reverse = T,
                                                            map.title=NULL))

      },error = function(e) {
        message(e$message)
        return(NULL)
      })

      prev.map.static.output(prev.static.plot)
      #message(paste0(input$prev_map$lng,'_',input$map_center$lat,'_', input$map_zoom))
      return(prev.static.plot)

    })



    ###############################################################
    ### download maps
    ###############################################################

    if(FALSE){
    output$download_interactive <- downloadHandler(

      filename = function() {

        ### informative file name
        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

        file.prefix <- paste0(DHS_country_code,CountryInfo$svyYear_selected(),'_',
                              CountryInfo$svy_indicator_var(),'_',
                              input$selected_adm,'_',
                              input$selected_method,'_',
                              input$selected_measure)

        # file.prefix <- paste0(CountryInfo$country(),'_',
        #                       input$selected_adm,'_',
        #                       input$selected_method,'_',
        #                       input$selected_measure)
        file.prefix <- gsub("[-.]", "_", file.prefix)

        return(paste0(file.prefix,'_prevMap.html'))

      },
      content = function(file) {
        # Save the Leaflet widget as an HTML file directly
        # htmlwidgets::saveWidget(prev_map_output(), file, selfcontained = TRUE)
        htmlwidgets::saveWidget(prev.map.interactive.output(),file, selfcontained = TRUE)

        #htmlwidgets::saveWidget(tmp.plot, map_html_temp, selfcontained = TRUE)
        #webshot2::webshot(url='mymap.html', file = file)
      }
    )
    }

    output$download_static <- downloadHandler(
      filename = function() {

        # file.prefix <- paste0(CountryInfo$country(),'_',
        #                       input$selected_adm,'_',
        #                       input$selected_method,'_',
        #                       input$selected_measure)

        ### informative file name

        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

        file.prefix <- paste0(DHS_country_code,CountryInfo$svyYear_selected(),'_',
                              CountryInfo$svy_indicator_var(),'_',
                              input$selected_adm,'_',
                              input$selected_method,'_',
                              input$selected_measure)


        file.prefix <- gsub("[-.]", "_", file.prefix)

        return(paste0(file.prefix,'_prevMap.pdf'))

        },

      content = function(file) {
        # Create the PDF
        grDevices::pdf(file, width = 10, height = 10)  # Set width and height of the PDF
        print(prev.map.static.output())  # Print the plot to the PDF
        grDevices::dev.off()  # Close the PDF
      }
    )




  })
}


## To be copied in the UI
# mod_result_visual_ui("result_visual_1")

## To be copied in the server
# mod_result_visual_server("result_visual_1")
