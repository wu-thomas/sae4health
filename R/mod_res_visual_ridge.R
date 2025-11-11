#' res_visual_ridge UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_visual_ridge_ui <- function(id){
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
        h4("Subnational Posterior Density Plot")
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
             selectInput(ns("selected_method"), "Select Method",
                         choices = c("Area-level Model"= "FH", "Unit-level Model"="Unit"))
                         #choices = c("Unit-level Model"="Unit"))

      ),
      column(4,
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      )
    ),
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("model_fitted_text"))
             )
      )
    ),
    tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
    fluidRow(
      # Main panel on the left
      column(12,
             tabsetPanel(id = ns("plot_type"),
                         tabPanel("All Regions",
                                  div(style = "margin-top:15px;",
                                      fluidRow(
                                        column(6,
                                               div(style = " margin: auto;width: max(100%, 400px);float: left;",
                                                   uiOutput(ns("select_adm2"))
                                               )
                                        )
                                      ),
                                      fluidRow(
                                        div(
                                          style = "width: min(98%, 1100px); margin-top: 10px;margin-left: 20px;margin-right: 10px;",
                                          uiOutput(ns("ridge_plot_all"))
                                        ),
                                        div( style = "width: min(98%, 1100px); margin-top: 20px; display: flex; justify-content: center;",
                                             uiOutput(ns("download_button_ridge_all"))
                                        )

                                      )
                                  )
                         ),
                         tabPanel("Regions with Highest/Lowest Prevalence",
                                  div(style = "margin-top:15px;",
                                      fluidRow(
                                        column(4,
                                               selectInput(ns("num_region_each"),
                                                           "Number of highest/lowest regions",
                                                           choices = c(1:30),
                                                           selected= 8)
                                        ),
                                        column(4,
                                               selectInput(ns("selected_format"),
                                                           "Choose a plot style",
                                                           choices = c('Wide','Long'))
                                        ),
                                        column(4,
                                               )
                                      ),
                                      fluidRow(
                                        div(
                                          style = "width: min(98%, 1100px); margin-top: 10px;margin-left: 20px;margin-right: 10px;",
                                          plotOutput(ns("ridge_plot_extreme"),height = "auto")
                                        ),
                                        div( style = "width: min(98%, 1100px); margin-top: 20px; display: flex; justify-content: center;",
                                             uiOutput(ns("download_button_ridge_extreme"))
                                        )
                                      ),
                                  )
                         )
             )
      )
    )
  )
}

#' res_visual_ridge Server Functions
#'
#' @noRd
mod_res_visual_ridge_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo){
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


    ### update selections
    col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
    ncols <- reactive({ length(col_names()) })

    observeEvent(col_names(), {
      adm.choice <- col_names()
      adm.choice <- adm.choice[adm.choice!='National']
      updateSelectInput(inputId = "selected_adm",
                        choices = adm.choice)
    })



    ### update text display on whether the model is fitted

    output$model_fitted_text <- renderUI({

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
          "Presenting ridge plot for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> level.",
          "</p>"
        ))

      }


    })


    ###############################################################
    ### render plot for all regions
    ###############################################################


    ### plot per upper admin region if at admin-2

    pseudo_level_reactive <-  reactiveVal(NULL)

    observe({

      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      selected.adm.num <-  admin_to_num(input$selected_adm)
      strata.level <- CountryInfo$GADM_strata_level()

      ### get pseudo level
      if(selected.adm.num > strata.level){ pseudo_level_reactive(2)}else{ pseudo_level_reactive(1)}
    })

    ### make selection on upper admin if region is finer than admin-2
    output$select_adm2 <- renderUI({

      req(CountryInfo$GADM_list())
      req(input$selected_adm)
      req(pseudo_level_reactive())


      if(pseudo_level_reactive()==1){
        return(NULL)
      }

      if(pseudo_level_reactive()==2){
        upper.gadm.num <- admin_to_num(input$selected_adm)-1
        upper.gadm <- num_to_admin(upper.gadm.num)

        gadm.list = CountryInfo$GADM_list()
        upper.adm.gadm <- gadm.list[[upper.gadm]]

        upper.adm.name <- upper.adm.gadm[[paste0('NAME_',upper.gadm.num)]]

       return(selectInput(ns("selected_upper_adm"),
                  "Choose an upper admin region",
                  choices = upper.adm.name))
      }

    })

    output$ridge_plot_all <- renderUI({
      req(pseudo_level_reactive())
      req(input$selected_adm)

      #message(paste0('pseudo level:',pseudo_level_reactive()))

      if (pseudo_level_reactive()==2) {
        plotOutput(ns("ridge_plot_all_adm2"),height = "auto")
      } else {  # if FALSE, show static map
        plotOutput(ns("ridge_plot_all_adm1"),height = "auto")
      }
    })


    ### initialize plotting parameters
    ridge_plot_all_adm1_output <- reactiveVal(NULL)
    ridge_plot_all_adm2_output <- reactiveVal(NULL)
    ridge_plot_all_adm1_height <- reactiveVal(NULL)
    ridge_plot_all_adm2_height <- reactiveVal(NULL)

    ### generate admin-1 plot
    observe({

      req(pseudo_level_reactive())

      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### initialize parameters
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method


      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}


      ### load results
      model_res_all <- AnalysisInfo$model_res_list()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]



      ### plot
      if(is.null(model_res_selected)|selected_adm=='National'){
        ridge_plot_all_adm1_output(NULL)
        ridge_plot_all_adm2_output(NULL)
        ridge_plot_all_adm1_height(400)
        ridge_plot_all_adm2_height(400)

        return(NULL)

      }

      if(pseudo_level_reactive()==1){

        #saveRDS(model_res_selected,'tmp_unit_admin1_1.rds')


        ridge.all.plot <-
          tryCatch({
            message(paste0('preparing ridge plot (admin-1 type). Model: ',selected_adm, ', ',selected_method))

            posterior_ridge_plot(res.obj = model_res_selected,
                                         model.gadm.level= admin_to_num(selected_adm),
                                         plot.extreme.num=NA,
                                         strata.gadm.level = CountryInfo$GADM_strata_level(),
                                         legend.label = 'Value',
                                         color.reverse= T) # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator

          },error = function(e) {
            message(e$message)
            return(NULL)
          })

        if(is.null(ridge.all.plot)){

          ### do not display map if no map successfully generated

          ridge_plot_all_adm1_output(NULL)
          ridge_plot_all_adm1_height(400)

        }else{

          ### display map and adjust for plot height

          ridge_plot_all_adm1_output(ridge.all.plot)
          n.plot.rows <- dim(ridge.all.plot$data)[1]/1000
          tmp.plot.height <- 400+min(n.plot.rows,5)*20+n.plot.rows*30
          ridge_plot_all_adm1_height(tmp.plot.height)

        }


      }

      if(pseudo_level_reactive()==2){

      #if(FALSE){
        #saveRDS(model_res_selected,'tmp_unit_admin2_1.rds')

        req(input$selected_upper_adm)


        ridge.all.plot <- tryCatch({

          if(is.null(input$selected_upper_adm)){return(NULL)}

          if (length(input$selected_upper_adm) == 0 || input$selected_upper_adm == "") {return(NULL)}

          message(paste0('preparing ridge plot (admin-2 type). Model: ',selected_adm, ', ',selected_method))

          tmp.plot <- posterior_ridge_plot(res.obj = model_res_selected,
                               by.upper.adm.name = input$selected_upper_adm,
                               plot.extreme.num=NA,
                               model.gadm.level= admin_to_num(selected_adm),
                               strata.gadm.level = CountryInfo$GADM_strata_level(),
                               legend.label = 'Value',
                               color.reverse= T) # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
          tmp.plot

          #ridge.all.plot <<- tmp.plot

        },error = function(e) {
          message('error for ridge plot about to be reported:')
          message(e$message)
          return(NULL)
        })

        if(is.null(ridge.all.plot)){

          ### do not display map if no map successfully generated
          ridge_plot_all_adm2_output(NULL)
          ridge_plot_all_adm2_height(400)
          return(NULL)

        }else{
          ### display map and adjust for plot height

          ridge_plot_all_adm2_output(ridge.all.plot)

          n.plot.rows <- dim(ridge.all.plot$data)[1]/1000

          tmp.plot.height <- 400+min(n.plot.rows,5)*20+n.plot.rows*30

          ridge_plot_all_adm2_height(tmp.plot.height)

        }

      }


    })

    ### render admin-1 plot
    output$ridge_plot_all_adm1 <- renderPlot({

      message(paste0('plot admin-1 ridge plot (all).'))

      req(ridge_plot_all_adm1_output())
      req(ridge_plot_all_adm1_height())

      #message(paste0('plot of height ',ridge_plot_all_adm1_height()))


      return(ridge_plot_all_adm1_output())

    },  height = function(){ridge_plot_all_adm1_height()})

    ### render admin-2 plot
    output$ridge_plot_all_adm2 <- renderPlot({

      message(paste0('plot admin-2 ridge plot (all).'))

      req(ridge_plot_all_adm2_output())
      req(ridge_plot_all_adm2_height())

      #message(paste0('plot of height ',ridge_plot_all_adm2_height()))


      return(ridge_plot_all_adm2_output())

    },  height = function(){ridge_plot_all_adm2_height()})


    ### download button UI
    output$download_button_ridge_all <- renderUI({

      if (is.null(pseudo_level_reactive())) {
        return(NULL)
      }

      if(pseudo_level_reactive()==1 && is.null(ridge_plot_all_adm1_output())){
        return(NULL)
      }

      if(pseudo_level_reactive()==2 && is.null(ridge_plot_all_adm2_output())){
        return(NULL)
      }

      downloadButton(ns("download_ridge_all"), "Download as PDF", icon = icon("download"),
                     class = "btn-primary")
    })


    ### download map as PDF

    output$download_ridge_all <- downloadHandler(

      filename = function() {

        ### informative file name
        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

        file.prefix <- paste0(DHS_country_code,CountryInfo$svyYear_selected(),'_',
                              CountryInfo$svy_indicator_var(),'_',
                              input$selected_adm,'_',
                              input$selected_method,'_')

        if(pseudo_level_reactive()==2){
          file.prefix <- paste0(file.prefix,'sub_')
        }



        file.prefix <- gsub("[-.]", "_", file.prefix)
        return(paste0(file.prefix,'ridge.pdf'))

      },
      content = function(file) {

        if(pseudo_level_reactive()==1){
          map.download <- ridge_plot_all_adm1_output()
          map.height <- ridge_plot_all_adm1_height()
        }
        if(pseudo_level_reactive()==2){
          map.download <- ridge_plot_all_adm2_output()
          map.height <- ridge_plot_all_adm2_height()
        }

        map.download <- map.download +
          ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25),
                                   "inches"))
        # Create the PDF
        grDevices::pdf(file, width = 10, height = round(map.height/100))  # Set width and height of the PDF
        print(map.download)  # Print the plot to the PDF
        grDevices::dev.off()  # Close the PDF
      }
    )




    ###############################################################
    ### render plot for top/bottom regions
    ###############################################################

    ridge.extreme.output <- reactiveVal(NULL)
    plot.extreme.height <- reactiveVal(400)


    ### determine height of plot
    if(FALSE){
    observe({

      req(input$selected_adm)

      #n.region <- dim(countryInfo$GADM_list_smoothed()[[selected_adm]])[1]


      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }
      #message(n.region)


    })
    }

    ### generate plot
    observe({

      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      ### initialize parameters
      selected_adm <- input$selected_adm
      selected_method <- input$selected_method

      ### load Madagascar example
      if(CountryInfo$use_preloaded_Madagascar()){
        AnalysisInfo$model_res_list(mdg.ex.model.res)}


      ### load results
      model_res_all <- AnalysisInfo$model_res_list()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]


      ### plot
      if(is.null(model_res_selected)|selected_adm=='National'){

        ridge.extreme.output(NULL)
        plot.extreme.height(400)
        return(NULL)

      }


      ridge.extreme.plot <-
      tryCatch({
        message(paste0('preparing ridge plot (extremes). Model: ',selected_adm, ', ',selected_method))

        posterior_ridge_plot(res.obj = model_res_selected,
                                                   plot.extreme.num = as.numeric(input$num_region_each), #plot.extreme.num=10
                                                   model.gadm.level= admin_to_num(selected_adm),
                                                   strata.gadm.level = CountryInfo$GADM_strata_level(),
                                                   legend.label = 'Value',
                                                   color.reverse= T,
                                                   plot.format = input$selected_format, # for extreme regions, side-by-side or long plot
                                                   top.bottom.label=c(' lowest regions',
                                                                      ' highest regions') # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
        )


      },error = function(e) {
        message('Ridge plot error:')
        message(e$message)
        return(NULL)
      })


      if(is.null(ridge.extreme.plot)){

        ridge.extreme.output(NULL)
        plot.extreme.height(400)

      }else{

        n.plot.rows <- dim(ridge.extreme.plot$data)[1]/1000

        if(input$selected_format=='Wide'&n.plot.rows==2*as.numeric(input$num_region_each)){
          n.plot.rows <- n.plot.rows/2}

        ridge.extreme.output(ridge.extreme.plot)
        tmp.plot.height <- 400+min(n.plot.rows,5)*20+n.plot.rows*30
        plot.extreme.height(tmp.plot.height)
      }


    })



    output$ridge_plot_extreme <- renderPlot({

      req(plot.extreme.height())
      req(ridge.extreme.output())

      #message(paste0('plot of height ',plot.extreme.height()))
      message(paste0('plot ridge plot (extremes).'))


      return(ridge.extreme.output())

    },  height = function(){plot.extreme.height()})


    ### download button UI
    output$download_button_ridge_extreme <- renderUI({

      if (is.null(ridge.extreme.output())) {
        return(NULL)
      }

      downloadButton(ns("download_ridge_extreme"), "Download as PDF", icon = icon("download"),
                     class = "btn-primary")
    })


    ### download map as PDF

    output$download_ridge_extreme <- downloadHandler(

      filename = function() {

        ### informative file name
        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

        file.prefix <- paste0(DHS_country_code,CountryInfo$svyYear_selected(),'_',
                              CountryInfo$svy_indicator_var(),'_',
                              input$selected_adm,'_',
                              input$selected_method,'_')

        file.prefix <- gsub("[-.]", "_", file.prefix)
        return(paste0(file.prefix,'extreme_ridge.pdf'))

      },
      content = function(file) {

        map.download <- ridge.extreme.output()
        map.height <- plot.extreme.height()

        map.download <- map.download +
          ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25),
                                                     "inches"))
        # Create the PDF
        grDevices::pdf(file, width = 11, height = round(map.height*1.2/100))  # Set width and height of the PDF
        print(map.download)  # Print the plot to the PDF
        grDevices::dev.off()  # Close the PDF
      }
    )




  })
}

## To be copied in the UI
# mod_res_visual_ridge_ui("res_visual_ridge_1")

## To be copied in the server
# mod_res_visual_ridge_server("res_visual_ridge_1")
