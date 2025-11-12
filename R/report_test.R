###############################################################
### test Cote dv, report generation
###############################################################
ref_tab_all <- surveyPrev::indicatorList
if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### initialize settings
  CountryInfo$WHO_version(F)
  CountryInfo$use_basemap('OSM')

  ### country meta
  ex.country <- "Cote d'Ivoire"
  ex.svy.year <- '2021'
  strat.gadm.level <- 1

  ### indicator
  ex.indicator.abbrev <-'RH_DELA_C_SKP'
  #file_path <-'C:/Users/wu-th/Downloads/KE_2022_DHS_04132024_852_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_DHS_04082024_724_143411.zip'
  #file_path <-'C:/Users/wu-th/Downloads/RW_2019-20_dat.zip'
  file_path <- 'E:/Downloads/CI_2021_DHS_11082024_050_143411.zip'

  CountryInfo$GADM_analysis_levels(c('National','Admin-1','Admin-2','Admin-3','Admin-4'))

  ###############################################################
  ### store country meta in R6
  ###############################################################

  ### country and svy year info
  CountryInfo$country(ex.country)
  CountryInfo$svyYear_selected(ex.svy.year) #CountryInfo$svyYear_list(ex.svy.year)

  country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName==CountryInfo$country(),'ISO3_CountryCode']


  ### get shapefiles

  if(!CountryInfo$WHO_version()){
    country_shapefile <- get_country_shapefile(country=ex.country,source='GADM-preload')
  }else{
    country_shapefile <- get_country_shapefile(country=ex.country,source='WHO')
  }

  CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
  CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

  CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


  ### indicator info and stratification level
  CountryInfo$svy_indicator_var(ex.indicator.abbrev)
  CountryInfo$GADM_strata_level(strat.gadm.level)

  ### description of the indicator
  CountryInfo$svy_indicator_des(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),]$Description)


  ###############################################################
  ### load data
  ###############################################################

  ### get recode and filenames for this variable

  recode_for_ind_abbrev <- reactiveVal(NULL)
  recode_for_ind_names <- reactiveVal(NULL)

  recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
  recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                         "Births Recode","Household Recode","Men's Recode",
                         "HIV Test Results Recode","Couples' Recode")

  recode_for_ind_abbrev(recode_list_abbrev[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                             recode_list_abbrev]==T)])

  ### which recode (full names) are needed for this indicator
  recode_for_ind_names(recode_list_names[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                           recode_list_abbrev]==T)])

  ### load survey data
  country= CountryInfo$country()
  svy_year = CountryInfo$svyYear_selected()
  recode_names_list=recode_for_ind_names()

  for (i in 1:length(recode_names_list)){
    file_prefix <- find_DHS_dat_name(ex.country,ex.svy.year,recode =recode_names_list[i])

    recode_path_found <- find_recode_path(file_path = file_path,
                                          recode_file =file_prefix,
                                          extensions = 'DTA')

    recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

    recode.data <- as.data.frame(recode.data)

    CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)


  }


  ### load GPS data
  ## set survey GPS data

  GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )

  GPS_path_found <- find_recode_path(file_path = file_path,
                                     recode_file = GPS_prefix,
                                     extensions = 'shp')

  GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))

  CountryInfo$svy_GPS_dat(GPS.dat)



  ### get analysis data set

  svy_dat_list <- CountryInfo$svy_dat_list()


  if(length(recode_for_ind_abbrev())>1){

    svy_dat_recode <- svy_dat_list[recode_for_ind_abbrev()]
    names(svy_dat_recode) <- as.character(get_recode_names(recode_for_ind_abbrev()))
  }else{

    svy_dat_recode <- svy_dat_list[[recode_for_ind_abbrev()]]

  }


  if(CountryInfo$svy_indicator_var() %in% ref_tab_new$ID){
    analysis_dat_fun =  utils::getFromNamespace(CountryInfo$svy_indicator_var(), "surveyPrev")
    #library(labelled)
    #library(naniar)
    #library(sjlabelled)
    #library(dplyr)
    analysis_dat = surveyPrev::getDHSindicator(Rdata=svy_dat_recode, indicator = NULL, FUN =analysis_dat_fun)

  }else{

    analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                                indicator = CountryInfo$svy_indicator_var())
  }


  CountryInfo$svy_analysis_dat(analysis_dat)

  ###############################################################
  ### data sparsity check
  ###############################################################

  ### initialize parameters
  #col_names_tmp <- names(CountryInfo$GADM_list())
  #n_adm_level <- length(col_names_tmp)
  row_names <- c("Direct", "FH", "Unit")
  nrows <- length(row_names)

  #options(survey.adjust.domain.lonely=TRUE)
  #options(survey.lonely.psu="adjust")


  strat.gadm.level <- CountryInfo$GADM_strata_level()

  ### initialize storage
  AnalysisInfo$model_screen_list(NULL)
  screen_check_list <- AnalysisInfo$model_screen_list()
  AnalysisInfo$cluster_admin_info_list(NULL)

  for (j in seq_len(length(CountryInfo$GADM_analysis_levels()))){

    tmp.adm <- CountryInfo$GADM_analysis_levels()[j]
    tmp.adm.num <- admin_to_num(tmp.adm)

    for (i in seq_len(nrows)) {

      #message(paste0(i),':',paste0(j))

      tmp.method <- row_names[i]

      message('Checking at ',tmp.adm,' using ',tmp.method,' model.')

      tmp.check.model <- screen_check_list[[tmp.method]][[tmp.adm]]


      ### skip model if already tried
      if(!is.null(tmp.check.model$screen.flag)){
        next
      }


      ### prepare admin level GPS info if not stored
      geo_info_list <- AnalysisInfo$cluster_admin_info_list()
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

      ### set model fitting status to Successful, assuming no error occurs
      tmp.check.model$screen.flag <- 'Error'
      tmp.check.model$screen.message <- 'Unable to process cluster and admin information.'

      ### process check results

      tryCatch(
        {
          #R.utils::withTimeout({
          tmp.check.model <- suppressWarnings(
            screen_svy_model(cluster.admin.info=tmp.geo.info,
                             analysis.dat= CountryInfo$svy_analysis_dat(),
                             model.gadm.level= tmp.adm.num,
                             strat.gadm.level = strat.gadm.level,
                             method=tmp.method)
          )
          #}, timeout = 300) ### 5 minutes for timeout
        },error = function(e) {
          tmp.check.model$screen.flag  <<- 'Error'
          tmp.check.model$screen.message <<- e$message
          message(e$message)

        }
      )


      if(tmp.check.model$screen.flag == 'Warning' & tmp.method=='FH'){
        tmp.check.model$screen.flag  <- 'Error'
      }


      message(tmp.check.model$screen.flag)

      ### store model results
      AnalysisInfo$set_screen_Check(tmp.method,tmp.adm,tmp.check.model)




    }

  }









  ###############################################################
  ### analysis
  ###############################################################


  res_list <- list()
  res_tracker_list <- list()

  AnalysisInfo$model_res_list(res_list)
  AnalysisInfo$model_res_tracker_list(res_tracker_list)

  ### tryout model

  #col_names <- names(CountryInfo$GADM_list())

  res_tracker_list <- AnalysisInfo$model_res_tracker_list()

  for (tmp.adm in CountryInfo$GADM_analysis_levels()){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')


      tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
      if(!is.null(tmp.tracker.list$status)){

        message('Skip. Already tried modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')

        next
      }


      ### set model fitting status to Successful, assuming no error occurs
      tmp.tracker.list$status <- 'Successful'
      tmp.tracker.list$message <- 'Successful'

      ### Run model
      tmp.res <- tryCatch(
        {
          #R.utils::withTimeout({
          tmp.res <- fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                   gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                   analysis.dat =   CountryInfo$svy_analysis_dat(),
                                   model.gadm.level = tmp.adm.num,
                                   strat.gadm.level = CountryInfo$GADM_strata_level(),
                                   method = tmp.method,
                                   aggregation =T

          )
          #}, timeout = 300) ### 5 minutes for timeout
        },error = function(e) {
          tmp.tracker.list$status <<- 'Unsuccessful'

          if(inherits(e, "TimeoutException")) {
            message("The operation timed out!")
            tmp.tracker.list$message <<- 'Timed out. Took too long to fit the model.'

          } else {
            tmp.tracker.list$message <<- e$message
            message(e$message)
          }
          return(NULL)
        }
      )



      ### store model results
      AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

      AnalysisInfo$set_fitted_res(tmp.method,tmp.adm,tmp.res)



    }

  }


  examine.tracker <- AnalysisInfo$model_res_tracker_list()
  examine.res <- AnalysisInfo$model_res_list()

  #tmp.unit <- examine.res$Unit$`Admin-2`
  #tmp.FH <- examine.res$FH$`Admin-2`
  #gadm.list.tmp <-  CountryInfo$GADM_list_smoothed()
  AnalysisInfo$Natl_res(examine.res$Direct$National$res.natl)
  ###############################################################
  ### Report
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




  #sanalysis.dat <- CountryInfo$svy_analysis_dat()

  # NMR: 659/20681
  # stunting: 1344/5403



  # geo_info_list list of cluster info

  grDevices::pdf("output1.pdf", width = 10, height = 10)

  {

    ###########################
    ### Table of Content
    ###########################

    # # Page 1: Table of Contents
    # grid::grid.newpage()
    # grid::grid.text("Table of Contents", y = 0.9, gp = gpar(fontsize = 14, fontface = "bold"))
    #
    # toc_entries <- list(
    #   list(title = "1. Plot on the First Page", page = "1"),
    #   list(title = "2. Text Paragraph", page = "2"),
    #   list(title = "3. Image", page = "3")
    # )
    #
    # # Define the y-position for each entry
    # y_positions <- seq(0.8, by = -0.05, length.out = length(toc_entries))
    # x_dot_start <- c(0.3,0.25,0.2)
    # dot_num_repeat <- c(30,28,27)
    #
    # # Draw each TOC entry with aligned components
    # for (i in seq_along(toc_entries)) {
    #   title <- toc_entries[[i]]$title
    #   page <- toc_entries[[i]]$page
    #   y_pos <- y_positions[i]
    #
    #   # Draw the title aligned to the left
    #   grid::grid.text(title, x = 0.1, y = y_pos, just = "left", gp = gpar(fontsize = 12))
    #
    #   # Draw the page number aligned to the right
    #   grid::grid.text(page, x = 0.9, y = y_pos, just = "right", gp = gpar(fontsize = 12))
    #
    #   # Draw dots to fill the space between title and page number
    #   grid::grid.text(strrep(" . ",dot_num_repeat[i]),
    #             x = x_dot_start[i], y = y_pos, just = "center", gp = gpar(fontsize = 12))
    # }

    ###########################
    ### initialize parameters
    ###########################

    page_counter = 1

    ###############################################################
    ### country/survey/indicator meta info
    ###############################################################

    grid::grid.newpage()
    grid::grid.text("Summary Info", x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

    tryCatch({

      #pushViewport(viewport(layout = grid.layout(10, 1, heights = unit(c(0.5, 0.4, 0.4, 0.4, 0.6, 0.4, 1.5, 0.4, 1.5, 2), "inches"))))

      # Title section for summary info
      #grid::grid.text("Summary Info", x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

      # Country info section
      grid::grid.text("Country: ", x = 0.05, y = 0.85, just = "left", gp = gpar(fontsize = 12))
      grid::grid.text(CountryInfo$country(), x = 0.2, y = 0.85, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

      grid::grid.text("Survey: ", x = 0.05, y = 0.8, just = "left", gp = gpar(fontsize = 12))
      grid::grid.text(CountryInfo$svyYear_selected(), x = 0.2, y = 0.8, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

      grid::grid.text("Indicator: ", x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))
      grid::grid.text(CountryInfo$svy_indicator_des(), x = 0.2, y = 0.75, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

      grid::grid.text("Levels: ", x = 0.05, y = 0.7, just = "left", gp = gpar(fontsize = 12))
      grid::grid.text(concatenate_vector_with_and(CountryInfo$GADM_analysis_levels()), x = 0.2, y = 0.7, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

      # Title for number of regions section
      grid::grid.text("Number of regions at selected admin levels:", x = 0.05, y = 0.6, just = "left", gp = gpar(fontsize = 12))

      # Table for number of admin regions (placed lower)
      pushViewport(viewport(y = 0.6, height = unit(1.5, "inches"), just = "top"))
      n_region_tab <- check_gadm_levels(CountryInfo$GADM_list())
      n_region_tab_grob <- gridExtra::tableGrob(n_region_tab)  # Remove row names
      grid::grid.draw(n_region_tab_grob)
      grid::upViewport()

      # Title for detailed indicator info
      pushViewport(viewport(y = 0.5, height = unit(2, "inches"), just = "top"))
      grid::grid.text("Detailed information on the indicator:", x = 0.05, y = 0.5, just = "left", gp = gpar(fontsize = 12))
      grid::upViewport()

      # Table for detailed indicator info
      pushViewport(viewport(y = 0.4, height = unit(2, "inches"), just = "top"))

      # Filter and wrap text for the indicator table
      ind_detailed_tab <- ref_tab_all[ref_tab_all$ID == CountryInfo$svy_indicator_var(), c('ID', 'Topic', 'Full_definition')]
      colnames(ind_detailed_tab) <- c('DHS Standard ID', 'DHS Report Chapter', 'Definition')

      # Text wrapping for the Definition column
      wrap_text <- function(text, width = 75) {
        paste(strwrap(text, width = width), collapse = "\n")
      }
      ind_detailed_tab$Definition <- sapply(ind_detailed_tab$Definition, wrap_text, width = 50)
      ind_detailed_tab$`DHS Report Chapter` <- sapply(ind_detailed_tab$`DHS Report Chapter`, wrap_text, width = 50)

      # Create and draw the indicator details table
      ind_tab_grob <- gridExtra::tableGrob(ind_detailed_tab, rows = NULL)  # Remove row names
      grid::grid.draw(ind_tab_grob)

      grid::upViewport()

      grid::grid.text(paste0("*"), x = 0.05, y = 0.15, gp = gpar(fontsize = 12, col = "red"),
                just = "left")

      grid::grid.text(paste0("For indicators with multiple versions, the app defaults to the ",
                       "5-year period before the survey (unless specified otherwise),"),
                x = 0.06, y = 0.15, just = "left", gp = gpar(fontsize = 12))

      grid::grid.text(paste0("and unstratified age groups (total)."),
                x = 0.05, y = 0.12, just = "left", gp = gpar(fontsize = 12))




    },error = function(e) {

      grid::grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

      message(e$message)

    })

    grid::grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
    page_counter = page_counter +1


    ###############################################################
    ### Estimate Consistency Check (National Level)
    ###############################################################

    grid::grid.newpage()
    grid::grid.text("Estimate Consistency Check (National Level)",
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
      DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode
      ind_api_est <- DHS_api_est[DHS_api_est$`Country Code`==DHS_country_code&
                                   DHS_api_est$`DHS Standard ID`==CountryInfo$svy_indicator_var()&
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
      grid::grid.text("To ensure the accuracy of our indicator coding schemes, we highly recommend users to compare ",
      x = 0.05, y = 0.88, just = "left", gp = gpar(fontsize = 12))

      grid::grid.text(expression(paste(
        bold("app-calculated national estimates")," with the ",
        bold("DHS final reports"),'.'
      )),
      x = 0.05, y = 0.83, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))


      grid::grid.text("National estimate (from the app): ", x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))
      grid::grid.text(paste0(natl_est,description_suffix),
                x = 0.3, y = 0.75, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))


      if(dim(ind_api_est)[1]==0){
        grid::grid.text("Estimate from the DHS report is not available through DHS API. Please manually check for consistency. ",
                  x = 0.05, y = 0.68, just = "left", gp = gpar(fontsize = 12))
      }else{
        grid::grid.text("National estimate (from DHS Final Report): ",
                  x = 0.05, y = 0.68, just = "left", gp = gpar(fontsize = 12))
        pushViewport(viewport(y = 0.63, height = unit(3, "inches"), just = "top"))

        # Get the number of rows and columns in your table
        n_rows <- nrow(ind_api_est)
        n_cols <- ncol(ind_api_est)

        # Modify the theme to color only the estimate column
        custom_theme <- ttheme_default(
          core = list(
            bg_params = list(
              fill = matrix(c(rep("white", n_rows * (n_cols - 2)), rep("darkolivegreen1", n_rows),
                              rep("white",n_rows)),
                            nrow = n_rows, ncol = n_cols, byrow = FALSE)
            )
          )
        )

        # Apply the theme to the table
        DHS_est_tab_grob <- gridExtra::tableGrob(ind_api_est, rows = NULL, theme = custom_theme)


        # color national estimates
        #DHS_est_tab_grob$grobs[23][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)

        grid::grid.draw(DHS_est_tab_grob)

        grid::upViewport()

        grid::grid.text(paste0("*"), x = 0.05, y = 0.15, gp = gpar(fontsize = 12, col = "red"),
                  just = "left")

        grid::grid.text(paste0("For indicators with multiple versions, the app defaults to the ",
                         "5-year period before the survey (unless specified otherwise),"),
                  x = 0.06, y = 0.15, just = "left", gp = gpar(fontsize = 12))

        grid::grid.text(paste0("and unstratified age groups (total)."),
                  x = 0.05, y = 0.12, just = "left", gp = gpar(fontsize = 12))

      }



    },error = function(e) {

      grid::grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

      message(e$message)

    })

    grid::grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
    page_counter = page_counter +1

    ###############################################################
    ### Overall Sampling Info
    ###############################################################


    grid::grid.newpage()
    grid::grid.text("Overall Sample Info", x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

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
      grid::grid.text("Total number of clusters: ", x = 0.05, y = 0.85, just = "left", gp = gpar(fontsize = 12))
      grid::grid.text(length(unique(complete_dat$cluster)), x = 0.3, y = 0.85, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

      grid::grid.text("Total sample size: ", x = 0.05, y = 0.8, just = "left", gp = gpar(fontsize = 12))
      grid::grid.text(dim(complete_dat)[1], x = 0.3, y = 0.8, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

      grid::grid.text("Total number of events: ", x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))
      grid::grid.text(sum(complete_dat$value), x = 0.3, y = 0.75, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))

      # Title for % of regions without any clusters
      grid::grid.text("Number of regions without any data:", x = 0.05, y = 0.65, just = "left", gp = gpar(fontsize = 12))

      # Table for number of admin regions (placed lower)
      pushViewport(viewport(y = 0.65, height = unit(1.5, "inches"), just = "top"))
      p_missing_tab_grob <- gridExtra::tableGrob(missing_tab)

      grid::grid.draw(p_missing_tab_grob)
      grid::upViewport()


    },error = function(e) {

      grid::grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

      message(e$message)

    })


    grid::grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
    page_counter = page_counter +1

    ###############################################################
    ### Admin Specific Sampling Info
    ###############################################################

    for(tmp_adm in CountryInfo$GADM_analysis_levels()){

      if(tmp_adm=='National'){next}

      grid::grid.newpage()

      grid::grid.text(paste0("Sample Info for ",tmp_adm), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

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
        grid::grid.draw(combined_grob)

        grid::upViewport()
      },error = function(e) {

        grid::grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

        message(e$message)

      })

      grid::grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
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

          if(tmp_measure=='exceed_prob'){selected_threshold <- AnalysisInfo$Natl_res()}else{selected_threshold=NULL}


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

        grid::grid.newpage()

        grid::grid.text(paste0("Key Statistics Maps: ",method_match[tmp.method], ' model at ',tmp.adm,' level'), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

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

          #grid::grid.draw(key_measure_maps_grob_tmp)
          grid::grid.draw(key_stat_map_grob)

          grid::upViewport()

        },error = function(e) {

          grid::grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

          message(e$message)

        })


        grid::grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
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

        grid::grid.newpage()

        grid::grid.text(paste0("Model Comparison: Direct Estimate vs ",method_match[tmp.method], ' model at ',tmp.adm,' level'), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

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

          #grid::grid.draw(key_measure_maps_grob_tmp)
          grid::grid.draw(scatter_plot_grob)

          grid::upViewport()

        },error = function(e) {

          grid::grid.text("Something went wrong generating this page of the report.", x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

          message(e$message)

        })


        grid::grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
        page_counter = page_counter +1


      }
    }








  }

  # Close PDF device
  grDevices::dev.off()


  ###############################################################
  ### visualization
  ###############################################################

  ## unit-level admin-3 html
  tmp.plot.html <- prevMap.leaflet(res.obj =  examine.res$Unit$`Admin-3`,
                                   gadm.shp = gadm.list.tmp[["Admin-3"]],
                                   model.gadm.level = 3,
                                   strata.gadm.level = 1,
                                   value.to.plot = 'mean',
                                   legend.label = 'Estimates',
                                   hatching.density = 15,
                                   map.title=NULL,
                                   threshold.p = 0,
                                   use.basemap = 'OSM',
                                   legend.color.reverse=F)




  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)

  hatching.density.country <- round(sqrt(9e07/tmp.area))

  tmp.plot <- prevMap.leaflet(res.obj = examine.res$Direct$`Admin-3`,
                              gadm.shp = gadm.list.tmp[["Admin-3"]],
                              model.gadm.level = 3,
                              strata.gadm.level = 1,
                              value.to.plot ='mean', #mean $CI.width
                              legend.label = 'Estimates',
                              map.title='this is a long longlong title',
                              hatching.density=60)


  sqrt(tmp.area/6)

  tmp.area <- as.numeric(sf::st_area(gadm.list.tmp[["National"]])/1e6)
  ### debug

  ### scatter
  tmp.res <- mdg.ex.model.res$FH$`Admin-2`$res.admin1

  tmp.res <- examine.res$FH$`Admin-2`$res.admin2

  tmp.res2 <- examine.res$Direct$`Admin-2`$res.admin2

  tmp.scatter <- scatter.plot( res.obj.x = examine.res$FH$`Admin-2`,
                               res.obj.y = examine.res$Direct$`Admin-2`,
                               value.to.plot = 'mean',
                               model.gadm.level = 2,
                               strata.gadm.level = 1,
                               label.x = 'M1',
                               label.y = 'M2',
                               plot.title=NULL,
                               interactive=T)

  ### ridge plot

  tmp.res <- examine.res$Unit$'Admin-2'
  test.post <- posterior_ridge_plot(res.obj =  tmp.res,
                                    plot.extreme.num=354, #plot.extreme.num=10
                                    model.gadm.level = admin_to_num('Admin-2'),
                                    strata.gadm.level = 1,
                                    legend.label = 'Value',
                                    color.reverse= T,
                                    plot.format = c('Long','Wide')[1], # for extreme regions, side-by-side or long plot
                                    top.bottom.label=c('Top','Bottom') # how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
  )
  tmp.res11 <- examine.res$Direct$'Admin-2'$res.admin2


}




