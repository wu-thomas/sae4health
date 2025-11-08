#' report_preparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_preparation_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    div(class = "module-title",
        h4("Report Generation")), # Add a title

    ## country, survey and indicator info
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 5px",
                 uiOutput(ns("info_display"))
             )
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
             div( style = "width:100%;max-width:1000px; margin-top: 20px; display: flex; justify-content: center;",
                  uiOutput(ns("download_button_ui"))
             )
      )
    )
    # Place additional UI elements below
  )
}

#' report_preparation Server Functions
#'
#' @importFrom grid gpar pushViewport grid.draw grid.newpage grid.text upViewport viewport
#' @importFrom gridExtra arrangeGrob tableGrob ttheme_default
#'
#' @noRd
mod_report_preparation_server <- function(id,CountryInfo,AnalysisInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ref_tab_all <- surveyPrev::indicatorList
    
    if (!requireNamespace("grid", quietly = TRUE)) {
      stop("Package 'grid' is required for this function. Please install it with install.packages('grid').")
    }

    if (!requireNamespace("gridExtra", quietly = TRUE)) {
      stop("Package 'gridExtra' is required for this function. Please install it with install.packages('gridExtra').")
    }

    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("Package 'patchwork' is required for this function. Please install it with install.packages('patchwork').")
    }

    ### Display general info
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




    ### download button

    output$download_button_ui <- renderUI({

      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      req(CountryInfo$svy_analysis_dat())

      downloadButton(ns("download_report"), "Download the Report", icon = icon("download"),
                     class = "btn-primary")

    })


    output$download_report <- downloadHandler(
      filename = function() {

        # file.prefix <- paste0(CountryInfo$country(),'_',
        #                       input$selected_adm,'_',
        #                       input$selected_method,'_',
        #                       input$selected_measure)

        ### informative file name

        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

        file.prefix <- paste0(DHS_country_code,CountryInfo$svyYear_selected(),'_',
                              CountryInfo$svy_indicator_var())


        file.prefix <- gsub("[-.]", "_", file.prefix)

        return(paste0(file.prefix,'_report.pdf'))

      },

      content = function(file) {
        # Create the PDF
        grDevices::pdf(file, width = 10, height = 10)  # Set width and height of the PDF

        # Function to add header and footer
        # add_header_footer <- function(header_text, footer_text) {
        #   grid.text(header_text, x = 0.1, y = 0.98, gp = gpar(fontsize = 10, fontface = "bold"), just = "center")
        #   grid.text(footer_text, x = 0.5, y = 0.02, gp = gpar(fontsize = 8), just = "center")
        # }
        #
        # # Page 1: Table of Contents
        # grid.newpage()
        # grid.text("Table of Contents", y = 0.9, gp = gpar(fontsize = 14, fontface = "bold"))
        # toc_text <- "1. Plot on the First Page...........1\n2. Text Paragraph....................2\n3. Image.............................3"
        # grid.text(toc_text, x = 0.1, y = 0.7, gp = gpar(fontsize = 12), just = "left")
        # #add_header_footer("Document Title", "Page 1")
        #
        # # Page 2: Plot
        # grid.newpage()
        # plot(stats::rnorm(100), main = "Plot on the First Page")
        # add_header_footer("Document Title", "Page 2")
        #
        # # Page 3: Text Paragraph
        # grid.newpage()
        # grid.text("This is a paragraph of text. It appears on the second page of the document.\n
        #   You can add multiple lines here for longer paragraphs and provide more information.",
        #           x = 0.5, y = 0.5, gp = gpar(fontsize = 12), just = "center")
        # add_header_footer("Document Title", "Page 3")

        {

          session$sendCustomMessage('controlSpinner', list(action = "show",
                                                           message = paste0("Report is being prepared. Please wait...")))

          ###############################################################
          ### preparation
          ###############################################################

          ### first make sure that the cluster info are prepared for all admin levels
          geo_info_list <- AnalysisInfo$cluster_admin_info_list()

          for (j in seq_len(length(CountryInfo$GADM_analysis_levels()))){

            tmp.adm <- CountryInfo$GADM_analysis_levels()[j]
            tmp.adm.num <- admin_to_num(tmp.adm)
            ### prepare admin level GPS info if not stored
            tmp.geo.info <- geo_info_list[[tmp.adm]]

            if(is.null(tmp.geo.info)){

              tryCatch({

                message(tmp.adm)

                tmp.cluster.adm.info <- cluster_admin_info(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                                           gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                                           model.gadm.level = admin_to_num(tmp.adm),
                                                           strat.gadm.level = CountryInfo$GADM_strata_level())


                AnalysisInfo$set_info_list(tmp.adm,tmp.cluster.adm.info)

                geo_info_list <- AnalysisInfo$cluster_admin_info_list()
                tmp.geo.info <- geo_info_list[[tmp.adm]]

              },error = function(e) {
                message(e$message)
              })
            }

          }

          geo_info_list <- AnalysisInfo$cluster_admin_info_list()


          ###########################
          ### initialize parameters
          ###########################

          page_counter = 1

          ###############################################################
          ### country/survey/indicator meta info
          ###############################################################

          grid.newpage()
          grid.text("Summary Info", x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

          tryCatch({

            #pushViewport(viewport(layout = grid.layout(10, 1, heights = grid::unit(c(0.5, 0.4, 0.4, 0.4, 0.6, 0.4, 1.5, 0.4, 1.5, 2), "inches"))))

            # Title section for summary info
            #grid.text("Summary Info", x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

            # Country info section
            grid.text("Country: ", x = 0.05, y = 0.85, just = "left", gp = gpar(fontsize = 12))
            grid.text(CountryInfo$country(), x = 0.2, y = 0.85, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

            grid.text("Survey: ", x = 0.05, y = 0.8, just = "left", gp = gpar(fontsize = 12))
            grid.text(CountryInfo$svyYear_selected(), x = 0.2, y = 0.8, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

            grid.text("Indicator: ", x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))
            grid.text(CountryInfo$svy_indicator_des(), x = 0.2, y = 0.75, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

            grid.text("Levels: ", x = 0.05, y = 0.7, just = "left", gp = gpar(fontsize = 12))
            grid.text(concatenate_vector_with_and(CountryInfo$GADM_analysis_levels()), x = 0.2, y = 0.7, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

            # Title for number of regions section
            grid.text("Number of regions at selected admin levels:", x = 0.05, y = 0.6, just = "left", gp = gpar(fontsize = 12))

            # Table for number of admin regions (placed lower)
            pushViewport(viewport(y = 0.6, height = grid::unit(1.5, "inches"), just = "top"))
            n_region_tab <- check_gadm_levels(CountryInfo$GADM_list())
            n_region_tab_grob <- tableGrob(n_region_tab)  # Remove row names
            grid.draw(n_region_tab_grob)
            upViewport()

            # Title for detailed indicator info
            pushViewport(viewport(y = 0.5, height = grid::unit(2, "inches"), just = "top"))
            grid.text("Detailed information on the indicator:", x = 0.05, y = 0.5, just = "left", gp = gpar(fontsize = 12))
            upViewport()

            # Table for detailed indicator info
            pushViewport(viewport(y = 0.4, height = grid::unit(2, "inches"), just = "top"))

            # Filter and wrap text for the indicator table

            tmp_indicator_ID  <- CountryInfo$svy_indicator_var()

            {
              ### customized window
              if(tmp_indicator_ID=='CM_ECMR_C_NNF'){tmp_indicator_ID = 'CM_ECMR_C_NNR'}

            }

            ind_detailed_tab <- ref_tab_all[ref_tab_all$ID == tmp_indicator_ID, c('ID', 'Topic', 'Full_definition')]
            colnames(ind_detailed_tab) <- c('DHS Standard ID', 'DHS Report Chapter', 'Definition')

            # Text wrapping for the Definition column
            wrap_text <- function(text, width = 75) {
              paste(strwrap(text, width = width), collapse = "\n")
            }
            ind_detailed_tab$Definition <- sapply(ind_detailed_tab$Definition, wrap_text, width = 50)
            ind_detailed_tab$`DHS Report Chapter` <- sapply(ind_detailed_tab$`DHS Report Chapter`, wrap_text, width = 50)

            # Create and draw the indicator details table
            ind_tab_grob <- tableGrob(ind_detailed_tab, rows = NULL)  # Remove row names
            grid.draw(ind_tab_grob)

            upViewport()

            grid.text(paste0("*"), x = 0.05, y = 0.15, gp = gpar(fontsize = 12, col = "red"),
                      just = "left")

            grid.text(paste0("For indicators with multiple versions, the app defaults to the ",
                             "5-year period before the survey (unless specified otherwise),"),
                      x = 0.06, y = 0.15, just = "left", gp = gpar(fontsize = 12))

            grid.text(paste0("and unstratified age groups (total)."),
                      x = 0.05, y = 0.12, just = "left", gp = gpar(fontsize = 12))




          },error = function(e) {

            grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

            message(e$message)

          })

          grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
          page_counter = page_counter +1


          ###############################################################
          ### Estimate Consistency Check (National Level)
          ###############################################################

          grid.newpage()
          grid.text("Estimate Consistency Check (National Level)",
                    x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

          tryCatch({

            ###########################
            ### Prepare
            ###########################

            ### prepare national estimates
            tmp.natl.res <- AnalysisInfo$Natl_res()
            natl_est = tmp.natl.res$direct.est*100

            if(grepl("dying", CountryInfo$svy_indicator_des())|grepl("ortality", CountryInfo$svy_indicator_des())){
              natl_est = natl_est*10
              description_suffix <- ' per 1000 individuals'
            }else{
              description_suffix <- ' %'
            }

            natl_est <- format(round(natl_est, digits=2), nsmall = 2)


            ### prepare DHS estimate

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
            }else{

              ind_api_est <- merge(ind_api_est,
                                   DHS_ind_dictionary[,c("DHS Standard Indicator ID","Full Definition")],
                                   by.x="DHS Standard ID",
                                   by.y="DHS Standard Indicator ID",
                                   all.x=T)


              ind_api_est <- ind_api_est[,c('Country','Survey Year',
                                            'DHS Standard ID','Full Definition','Estimate','By Variable Label')]

              wrap_text <- function(text, width = 75) {
                paste(strwrap(text, width = width), collapse = "\n")
              }
              ind_api_est$`Full Definition` <- sapply(ind_api_est$`Full Definition`, wrap_text, width = 35)
              ind_api_est$`By Variable Label` <- sapply(ind_api_est$`By Variable Label`, wrap_text, width = 30)

              if(dim(ind_api_est)[1]>1){
                ind_api_est$`Full Definition` <- c(ind_api_est$`Full Definition`[1],rep('Same as above',times=dim(ind_api_est)[1]-1))
              }

              if('Total' %in% ind_api_est$`By Variable Label`){
                ind_api_est <- ind_api_est[ind_api_est$`By Variable Label` =='Total',]
              }




            }



            ###########################
            ### Draw
            ###########################
            grid.text("To ensure the accuracy of our indicator coding schemes, we highly recommend users to compare ",
                      x = 0.05, y = 0.88, just = "left", gp = gpar(fontsize = 12))

            grid.text(expression(paste(
              bold("app-calculated national estimates")," with the ",
              bold("DHS final reports"),'.'
            )),
            x = 0.05, y = 0.83, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))


            grid.text("National estimate (from the app): ", x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))
            grid.text(paste0(natl_est,description_suffix),
                      x = 0.3, y = 0.75, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))


            if(dim(ind_api_est)[1]==0){
              grid.text("Estimate from the DHS report is not available through DHS API. Please manually check for consistency. ",
                        x = 0.05, y = 0.68, just = "left", gp = gpar(fontsize = 12))
            }else{
              grid.text("National estimate (from DHS Final Report): ",
                        x = 0.05, y = 0.68, just = "left", gp = gpar(fontsize = 12))
              pushViewport(viewport(y = 0.63, height = grid::unit(3, "inches"), just = "top"))

              # Get the number of rows and columns in your table
              n_rows <- nrow(ind_api_est)
              n_cols <- ncol(ind_api_est)

              # Modify the theme to color only the estimate column
              custom_theme <- gridExtra::ttheme_default(
                core = list(
                  bg_params = list(
                    fill = matrix(c(rep("white", n_rows * (n_cols - 2)), rep("darkolivegreen1", n_rows),
                                    rep("white",n_rows)),
                                  nrow = n_rows, ncol = n_cols, byrow = FALSE)
                  )
                )
              )

              # Apply the theme to the table
              DHS_est_tab_grob <- tableGrob(ind_api_est, rows = NULL, theme = custom_theme)


              # color national estimates
              #DHS_est_tab_grob$grobs[23][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)

              grid.draw(DHS_est_tab_grob)

              upViewport()

              grid.text(paste0("*"), x = 0.05, y = 0.15, gp = gpar(fontsize = 12, col = "red"),
                        just = "left")

              grid.text(paste0("For indicators with multiple versions, the app defaults to the ",
                               "5-year period before the survey (unless specified otherwise),"),
                        x = 0.06, y = 0.15, just = "left", gp = gpar(fontsize = 12))

              grid.text(paste0("and unstratified age groups (total)."),
                        x = 0.05, y = 0.12, just = "left", gp = gpar(fontsize = 12))

            }



          },error = function(e) {

            grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

            message(e$message)

          })

          grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
          page_counter = page_counter +1

          ###############################################################
          ### Overall Sampling Info
          ###############################################################


          grid.newpage()
          grid.text("Overall Sample Info", x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

          tryCatch({

            ###########################
            ### Prepare
            ###########################

            ### preparation
            analysis_dat <- CountryInfo$svy_analysis_dat()
            complete_dat <- analysis_dat[!is.na(analysis_dat$value),]

            ### calculate missing in regions
            samp_info_adm_map_list <- list()
            p_missing_cluster <- vector()


            for(tmp_adm in CountryInfo$GADM_analysis_levels()){
              if(tmp_adm!='National'){
                ex_adm_maps <- sample_info_map_static(model.gadm.level = admin_to_num(tmp_adm),
                                                      strat.gadm.level = 1,
                                                      analysis.dat = CountryInfo$svy_analysis_dat(),
                                                      gadm.list.visual = CountryInfo$GADM_list_smoothed(),
                                                      cluster.info = geo_info_list[[tmp_adm]]$cluster.info)
                samp_info_adm_map_list[[tmp_adm]] <- ex_adm_maps

                p_missing_cluster <- c(p_missing_cluster,
                                       sum(is.na(ex_adm_maps$adm.sample.info$n.clusters))/dim(ex_adm_maps$adm.sample.info)[1])
              }
            }

            cluster_missing_info <- data.frame(
              Admin_Level = (CountryInfo$GADM_analysis_levels()),
              Percent_Missing = c(0,p_missing_cluster)
            )

            cluster_missing_info$Percent_Missing <- paste0(round(cluster_missing_info$Percent_Missing*100,digits=2),'%')
            row.names(cluster_missing_info) <- NULL
            colnames(cluster_missing_info) <- c("Admin Level", "Regions without Any Clusters")

            missing_tab <- as.data.frame(t(cluster_missing_info))
            colnames(missing_tab) <- missing_tab[1, ]
            missing_tab <-  missing_tab[-1, , drop = FALSE]

            ###########################
            ### Draw
            ###########################

            # Country info section
            grid.text("Total number of clusters: ", x = 0.05, y = 0.85, just = "left", gp = gpar(fontsize = 12))
            grid.text(length(unique(complete_dat$cluster)), x = 0.3, y = 0.85, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

            grid.text("Total sample size: ", x = 0.05, y = 0.8, just = "left", gp = gpar(fontsize = 12))
            grid.text(dim(complete_dat)[1], x = 0.3, y = 0.8, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

            grid.text("Total number of events: ", x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))
            grid.text(sum(complete_dat$value), x = 0.3, y = 0.75, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

            # Title for % of regions without any clusters
            grid.text("Number of regions without any data:", x = 0.05, y = 0.65, just = "left", gp = gpar(fontsize = 12))

            # Table for number of admin regions (placed lower)
            pushViewport(viewport(y = 0.65, height = grid::unit(1.5, "inches"), just = "top"))
            p_missing_tab_grob <- tableGrob(missing_tab)

            grid.draw(p_missing_tab_grob)
            upViewport()


          },error = function(e) {

            grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

            message(e$message)

          })


          grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
          page_counter = page_counter +1

          ###############################################################
          ### Admin Specific Sampling Info
          ###############################################################

          for(tmp_adm in CountryInfo$GADM_analysis_levels()){

            if(tmp_adm=='National'){next}

            grid.newpage()

            grid.text(paste0("Sample Info for ",tmp_adm), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

            tryCatch({

              ex_adm_maps <- sample_info_map_static(model.gadm.level = admin_to_num(tmp_adm),
                                                    strat.gadm.level = 1,
                                                    analysis.dat = CountryInfo$svy_analysis_dat(),
                                                    gadm.list.visual = CountryInfo$GADM_list_smoothed(),
                                                    cluster.info = geo_info_list[[tmp_adm]]$cluster.info)

              # Define the map objects and their descriptions
              map_list <- list(
                tmp_map_event = list(map = ex_adm_maps$n_event_map, description = "Number of Events"),
                tmp_map_cluster = list(map = ex_adm_maps$n_cluster_map, description = "Number of Clusters"),
                tmp_map_sample = list(map = ex_adm_maps$n_sample_map, description = "Number of Samples")
              )

              # Function to apply common modifications to each map
              apply_map_modifications <- function(map_obj, description) {
                map_obj$data$model_des <- description
                map_obj <- map_obj +
                  ggplot2::facet_wrap(ggplot2::vars(model_des)) +
                  ggplot2::theme(
                    legend.text = ggplot2::element_text(size = 13),
                    legend.title = ggplot2::element_text(size = 14),
                    strip.text.x = ggplot2::element_text(size = 13),
                    legend.key.height = ggplot2::unit(0.5, 'cm'),
                    strip.text = ggplot2::element_text(size = 16),
                    legend.position = "bottom",
                    legend.justification = c(0.5, 0),      # Center the legend at the bottom
                    legend.box = "horizontal"              # Arrange legend items in a row
                  )
                return(map_obj)
              }

              # Apply the function to each map
              for (name in names(map_list)) {
                map_list[[name]]$map <- apply_map_modifications(map_list[[name]]$map, map_list[[name]]$description)
              }

              # Retrieve modified maps if needed
              tmp_map_event <- map_list$tmp_map_event$map
              tmp_map_cluster <- map_list$tmp_map_cluster$map
              tmp_map_sample <- map_list$tmp_map_sample$map


              #tmp_map_grob <- ggplot2::ggplotGrob(tmp_map)
              combined_grob <- arrangeGrob(
                tmp_map_cluster,  # Centered first row
                arrangeGrob(tmp_map_event, tmp_map_sample, ncol = 2),  # Second row with two plots
                nrow = 2)

              pushViewport(viewport(width = 0.9, height = 0.8, just = c("center", "center")))  # Set size within page
              # grid.arrange(
              #   tmp_map_grob,  # Centered first row
              #   arrangeGrob(tmp_map_grob, tmp_map_grob, ncol = 2),  # Second row with two plots
              #   nrow = 2,
              #   heights = c(0.45, 0.45))
              grid.draw(combined_grob)

              upViewport()
            },error = function(e) {

              grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

              message(e$message)

            })

            grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
            page_counter = page_counter +1




          }





          ###############################################################
          ### Collect fitted model info and set common info
          ###############################################################

          selected_adms <- CountryInfo$GADM_analysis_levels()
          method_names <- c("Direct", "FH", "Unit")
          model_res_all <- AnalysisInfo$model_res_list() ## react if model results change

          fitted_model_matrix <- as.data.frame(matrix(vector('integer', length(method_names) * length(selected_adms)),
                                                      nrow = length(method_names),
                                                      dimnames = list(method_names, selected_adms)))


          #num_fitted_model <- 0
          # Populate the dataframe with checkbox inputs
          for (i in seq_len(length(method_names))) {
            for (j in seq_len(length(selected_adms))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]
              #message(paste0('present model from method ',tmp.method, ' at ', tmp.adm,' level.'))
              model_res_selected <- model_res_all[[tmp.method]][[tmp.adm]]

              if(!is.null(model_res_selected)){
                fitted_model_matrix[i,j]=1
              }else{
                fitted_model_matrix[i,j]=0
              }

            }
          }

          ### set up labels used across plots

          method_match <- c(
            "Direct" = "Direct estimates",
            "Unit" = "Unit-level",
            "FH" = "Area-level"
          )

          # present measures as full name, maps
          measure_match <- c(
            "cv" = "Coefficient \n of variation",
            "mean" = "Mean",
            "CI.width" = "Width of \n 95% CI",
            "exceed_prob" = "Exceedance \n probability"
          )

          # present measures as full name, scatter plot
          measure_match_nobreak <- c(
            "cv" = "Coefficient of variation",
            "mean" = "Mean",
            "CI.width" = "Width of 95% CI",
            "exceed_prob" = "Exceedance probability"
          )



          ###############################################################
          ### Model specific maps
          ###############################################################

          #############################
          ### prepare plot
          #############################

          ### measures used in the plots
          all.measures <- c('mean','CI.width','cv','exceed_prob')
          #all.measures <- c('mean','CI.width','cv')


          single_model_maps_collection <- list()

          for (i in seq_len(length(method_names))) {
            for (j in seq_len(length(selected_adms))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]

              if(!fitted_model_matrix[i,j]==1|tmp.adm=='National'){next}

              message(paste0('preparing key statistics map for ',tmp.method, ' model at ',tmp.adm, ' level.'))

              model_res_tmp <- model_res_all[[tmp.method]][[tmp.adm]]


              tmp.map.list <- list()

              for(tmp_measure in all.measures){

                if(tmp_measure=='exceed_prob'){
                  tmp.natl.res <- AnalysisInfo$Natl_res()
                  selected_threshold <- tmp.natl.res$direct.est
                  message(selected_threshold)
                }else{selected_threshold=NULL}


                one.static.plot <-  tryCatch({
                  tmp.plot <- suppressWarnings(prevMap.static(res.obj = model_res_tmp,
                                                              gadm.shp = CountryInfo$GADM_list_smoothed()[[tmp.adm]],
                                                              model.gadm.level = admin_to_num(tmp.adm),
                                                              strata.gadm.level = CountryInfo$GADM_strata_level(),
                                                              value.to.plot =tmp_measure,
                                                              legend.label = measure_match[tmp_measure],
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
                  tmp.map.list[[tmp_measure]]=one.static.plot
                }
              }

              # key_measure_maps_tmp <- tryCatch({patchwork::wrap_plots(tmp.map.list, ncol = 2)
              # },error = function(e) {
              #   message(e$message)
              #   return(NULL)
              # })


              #single_model_maps_collection[[tmp.method]][[tmp.adm]] <- key_measure_maps_tmp
              single_model_maps_collection[[tmp.method]][[tmp.adm]] <- tmp.map.list


            }
          }

          #############################
          ### draw maps, ind model
          #############################

          for (i in seq_len(length(method_names))) {
            for (j in seq_len(length(selected_adms))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]

              if(!fitted_model_matrix[i,j]==1|tmp.adm=='National'){next}

              grid.newpage()

              grid.text(paste0("Key Statistics Maps: ",method_match[tmp.method], ' model at ',tmp.adm,' level'), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

              tryCatch({

                ### extract prepared maps to plot
                #key_measure_maps_grob_tmp <- patchwork::patchworkGrob(single_model_maps_collection[[tmp.method]][[tmp.adm]] )

                tmp.map.collect <- single_model_maps_collection[[tmp.method]][[tmp.adm]]

                ### specify configurations depends on number of statistics selected
                if(length(all.measures)==1){
                  key_stat_map_grob <- arrangeGrob(tmp.map.collect[[1]], nrow = 1)
                }

                if(length(all.measures)==2){
                  key_stat_map_grob <- arrangeGrob(tmp.map.collect[[1]],tmp.map.collect[[2]], nrow = 1)
                }

                if(length(all.measures)==3){
                  key_stat_map_grob <- arrangeGrob(
                    tmp.map.collect[[1]],  # Centered first row
                    arrangeGrob(tmp.map.collect[[2]], tmp.map.collect[[3]], ncol = 2),  # Second row with two plots
                    nrow = 2)
                }

                if(length(all.measures)==4){
                  key_measure_patch <- patchwork::wrap_plots(tmp.map.collect, ncol = 2)
                  key_stat_map_grob <- patchwork::patchworkGrob(key_measure_patch)

                }


                pushViewport(viewport(width = 0.9, height = 0.8, just = c("center", "center")))  # Set size within page

                #grid.draw(key_measure_maps_grob_tmp)
                grid.draw(key_stat_map_grob)

                upViewport()

              },error = function(e) {

                grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

                message(e$message)

              })


              grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
              page_counter = page_counter +1


            }
          }


          ###############################################################
          ### Scatter plot comparison with direct estimates
          ###############################################################

          scatter_measure <- c('mean','CI.width')

          #############################
          ### prepare plot
          #############################

          scatter_plot_collection <- list()

          for (j in seq_len(length(selected_adms))) {
            for (i in seq_len(length(method_names))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]

              ## do not make scatter plot if model not fitted
              if(!fitted_model_matrix[i,j]==1|tmp.adm=='National'|tmp.method=='Direct'){next}

              ## do not make scatter plot if direct model at this level is not fitted
              if(fitted_model_matrix[1,j]==0){next}

              tmp.method.x <- 'Direct'
              tmp.method.y <- tmp.method

              tmp_scatter_plot_list <- list()

              for(t in c(1:length(scatter_measure))){

                tmp_measure <- scatter_measure[t]
                label_x <- method_match[tmp.method.x]
                label_y <- method_match[tmp.method.y]

                scatter.static.tmp <- tryCatch({
                  scatter.plot( res.obj.x = model_res_all[[tmp.method.x]][[tmp.adm]],
                                res.obj.y = model_res_all[[tmp.method.y]][[tmp.adm]],
                                value.to.plot = tmp_measure,
                                model.gadm.level = admin_to_num(tmp.adm),
                                strata.gadm.level = CountryInfo$GADM_strata_level(),
                                label.x = label_x,
                                label.y = label_y,
                                plot.title=measure_match_nobreak[tmp_measure],
                                interactive=F)
                },error = function(e) {
                  message(e$message)
                  return(NULL)
                })


                if(!is.null(scatter.static.tmp)){
                  tmp_scatter_plot_list[[tmp_measure]]=scatter.static.tmp
                }

              }


              scatter_plot_collection[[tmp.method]][[tmp.adm]] <- tmp_scatter_plot_list


            }
          }

          #############################
          ### draw scatter plots
          #############################

          for (i in seq_len(length(method_names))) {
            for (j in seq_len(length(selected_adms))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]

              tmp_scatter_plot_obj <- scatter_plot_collection[[tmp.method]][[tmp.adm]]

              if(is.null(tmp_scatter_plot_obj)){next}

              grid.newpage()

              grid.text(paste0("Model Comparison: Direct Estimate vs ",method_match[tmp.method], ' model at ',tmp.adm,' level'), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

              tryCatch({

                ### extract prepared maps to plot
                #key_measure_maps_grob_tmp <- patchwork::patchworkGrob(single_model_maps_collection[[tmp.method]][[tmp.adm]] )

                tmp.scatter.collect <- scatter_plot_collection[[tmp.method]][[tmp.adm]]

                ### specify configurations depends on number of statistics selected
                if(length(scatter_measure)==1){
                  scatter_plot_grob <- arrangeGrob(tmp.scatter.collect[[1]], nrow = 1)
                }

                if(length(scatter_measure)==2){
                  scatter_plot_grob <- arrangeGrob(tmp.scatter.collect[[1]],tmp.scatter.collect[[2]], nrow = 1)
                }

                if(length(scatter_measure)==3){
                  scatter_plot_grob <- arrangeGrob(
                    tmp.scatter.collect[[1]],  # Centered first row
                    arrangeGrob(tmp.scatter.collect[[2]], tmp.scatter.collect[[3]], ncol = 2),  # Second row with two plots
                    nrow = 2)
                }

                pushViewport(viewport(width = 0.9, height = 0.8, just = c("center", "center")))  # Set size within page

                #grid.draw(key_measure_maps_grob_tmp)
                grid.draw(scatter_plot_grob)

                upViewport()

              },error = function(e) {

                grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

                message(e$message)

              })


              grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
              page_counter = page_counter +1


            }
          }



        }




        session$sendCustomMessage('controlSpinner', list(action = "hide"))







        # Close the PDF device
        grDevices::dev.off()



      }
    )


  })
}

## To be copied in the UI
# mod_report_preparation_ui("report_preparation_1")

## To be copied in the server
# mod_report_preparation_server("report_preparation_1")
