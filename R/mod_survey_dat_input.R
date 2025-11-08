#' survey_dat_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_survey_dat_input_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      tags$style(HTML("
      .checklist-item {
        padding: 2px;
        border-radius: 2px;
        transition: background-color 0.3s ease;
      }
      .checklist-item:hover {
        background-color: #f0f0f0;
      }
    "))
    ),
    
    div(class = "module-title",
        h4("Data Upload Checklist")),
    fluidRow(
      column(8,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("text_display"))
             )
      ),
      column(4,
             div(style = "display: flex; flex-wrap: wrap;",
                 uiOutput(ns("checklist"))
             )
      )
    ),
    tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
    fluidRow(
      # Main panel on the left
      column(4,
             tabsetPanel(id = ns("data_provision_method"),
                         tabPanel(#"Manual Upload",
                           title = if (is.null(golem::get_golem_options()$server_link)) "Manual Upload" else "Data Step",
                           div(style = "margin: auto;float: left;margin-top:10px;",
                               uiOutput(ns("manual_upload_text"))
                           ),
                           ### upload survey data
                           div(style = "margin: auto;float: left;width: min(100%,400px);",
                               fileInput(ns("Svy_dataFile"),
                                         accept='.zip',
                                         with_red_star("Upload DHS survey data (.zip)")),
                               
                               #actionButton(ns("upload_Svy_Data"), "Submit Survey Data"),
                               uiOutput(ns("data_button"))
                           )
                         ),
                         #tabPanel("Provide Credentials",
                         #         textInput(ns("username"), "Username:"),
                         #         passwordInput(ns("password"), "Password:"),
                         #         actionButton(ns("login_button"), "Login")
                         #)
             )
      ),
      
      column(
        8,
        #hr(style = "border-top-color: #E0E0E0;"),
        
        # Add nested tabset for cluster map and raw data preview
        tabsetPanel(
          tabPanel(
            "Cluster Map",
            div(style = "margin: auto; float: left; width:min(100%,800px);margin-top: 5px;margin-bottom: 5px",
                radioButtons(
                  ns("AdminLevel"),
                  "Check out maps for number of clusters at Admin levels",
                  choices = c("Admin-1"),
                  inline=T
                ),
                tags$hr(style="border-top-color: #E0E0E0;")
                
                # selectInput(ns("AdminLevel"), "Check out maps for Admin levels",
                #          choices=character(0))
            ),
            div(style = "margin: auto; float: left;width:max(100%,800px);",
                shinyWidgets::materialSwitch(inputId = ns("ClusterMapType"), label = HTML("<strong>Interactive Map Enabled</strong>"),
                                             status = "success",value =T)
            ),
            div(
              id = "map-container",
              style = "width: min(100%, 800px); margin-top: 5px;height: 100%; overflow-y: auto;",
              uiOutput(ns("cluster_map"))
              #leaflet::leafletOutput(ns("prev_map"))
            )
          ),
          tabPanel(
            "Data Preview",
            div(style = "margin: auto; float: left; width:100%;margin-top: 15px;",
                DT::dataTableOutput(ns("Dat_Preview"))
            ),
            div( style = "width:100%;max-width:1000px; margin-top: 20px; display: flex; justify-content: center;",
                 uiOutput(ns("download_button_ui"))
            )
          )
        )
      )
    )
  )
}




# @importFrom labelled set_value_labels set_variable_labels
# @importFrom dplyr mutate if_else case_when

#' survey_dat_input Server Functions
#'
#' @noRd
mod_survey_dat_input_server <- function(id,CountryInfo,AnalysisInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ref_tab_all <- surveyPrev::indicatorList
    
    options(shiny.maxRequestSize=550*1024^2)
    
    
    # if (!requireNamespace("labelled", quietly = TRUE)) {
    #   stop("Package 'labelled' is required for this function. Please install it with install.packages('labelled').")
    # }
    # if (!requireNamespace("naniar", quietly = TRUE)) {
    #   stop("Package 'naniar' is required for this function. Please install it with install.packages('naniar').")
    # }
    # if (!requireNamespace("sjlabelled", quietly = TRUE)) {
    #   stop("Package 'sjlabelled' is required for this function. Please install it with install.packages('sjlabelled').")
    # }
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' is required for this function. Please install it with install.packages('haven').")
    }
    # if (!requireNamespace("data.table", quietly = TRUE)) {
    #   stop("Package 'data.table' is required for this function. Please install it with install.packages('data.table').")
    # }
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package 'dplyr' is required for this function. Please install it with install.packages('dplyr').")
    }
    
    
    ### initialize variables for recode
    recode_for_ind_abbrev <- reactiveVal(NULL)
    recode_for_ind_names <- reactiveVal(NULL)
    
    recode_list_abbrev <- c('IR','PR','KR','BR','HR','MR','AR','CR')
    recode_list_names <- c("Individual Recode","Household Member Recode","Children's Recode",
                           "Births Recode","Household Recode","Men's Recode",
                           "HIV Test Results Recode","Couples' Recode")
    
    ### data upload completion indicator
    dat.complete.ind <- reactiveVal(FALSE)
    
    ###############################################################
    ### text instructions on recode
    ###############################################################
    
    output$text_display <- renderUI({
      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      
      
      ### country information
      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()
      admin_level <- CountryInfo$GADM_display_selected_level()
      #indicator_description <- surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description
      
      ### which recode (abbreviation) are needed for this indicator
      recode_for_ind_abbrev(recode_list_abbrev[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                                 recode_list_abbrev]==T)])
      
      ### which recode (full names) are needed for this indicator
      recode_for_ind_names(recode_list_names[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                               recode_list_abbrev]==T)])
      
      
      
      recode.avail <- check_dat_avail(country =country , svy_year =svy_year , indicator =CountryInfo$svy_indicator_var())
      
      if(length(recode.avail$missing_recode)>0){
        
        return.text <- HTML(paste0(
          "<p style='font-size: large;'>",
          "<strong> DHS ", svy_year, "</strong> survey ",
          "in <strong>", country,"</strong> <br>",
          "  <font color='red'>does not support estimation </font> for",
          "<span style='background-color: #D0E4F7;'>",
          "<br> <strong>",CountryInfo$svy_indicator_des(), "</strong>",
          "</span>. <br>",
          "Please switch to another indicator or survey.",
          "</p>"
          #"<br>",
          #"<hr style='border-top-color: #E0E0E0;'>"
        ))
        
        
        return(return.text)
        
      }
      
      
      ### display the recode as, full names (abbreviation), format
      recode_for_display <- paste(recode_for_ind_abbrev(), " (", recode_for_ind_names(), ")", sep="")
      
      
      if(CountryInfo$server_version()){
        
        ### load from server text box
        return(
          HTML(paste0(
            "<p style='font-size: large;'>",
            "Based on your goal of estimating ",
            "<span style='background-color: #D0E4F7;'>",
            "<br><strong>", CountryInfo$svy_indicator_des(), "</strong>, ",
            "</span><br> in <strong>", country,
            "</strong> using the <strong>DHS ", svy_year, "</strong> survey, ",
            "<br> the following recode(s) are required for the analysis: ",
            "<span style='background-color: #D0E4F7;'>",
            "<strong>", concatenate_vector_with_and(recode_for_display), "</strong></span>.",
            "</p>"
          ))
        )
      }else{
        HTML(paste0(
          "<p style='font-size: large;'>",
          "Based on your goal of estimating ",
          "<span style='background-color: #D0E4F7;'>",
          "<br> <strong>",CountryInfo$svy_indicator_des(), "</strong>, ",
          "</span> <br> in <strong>", country,
          "</strong> with <strong> DHS ", svy_year, "</strong> survey ",
          "<br> Please upload your data in ",
          "<span style='background-color: #D0E4F7;'>",
          "<strong>",concatenate_vector_with_and(recode_for_display), "</strong> </span>.",
          "</p>"
          #"<br>",
          #"<hr style='border-top-color: #E0E0E0;'>"
        ))
      }
      
    })
    
    ###############################################################
    ### text instructions on manually upload data
    ###############################################################
    
    ### define the pop up modal containing detailed instructions
    
    inst.modal.text <-  reactiveVal(NULL)
    
    # Create a modal using bsModal, defined in the server-side logic
    observeEvent(input$triggerModal, {
      showModal(
        modalDialog(
          title = "Detailed Instructions",
          inst.modal.text(),  # Dynamic content from the server
          footer = tagList(
            actionButton(ns("closeModal"), "Close")  # Button to close the modal
          )
        )
      )
    })
    
    # Observer to close the modal when "Close" button is clicked
    observeEvent(input$closeModal, {
      removeModal()
    })
    
    
    
    
    
    output$manual_upload_text <- renderUI({
      
      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      
      
      ### load from server prompt text
      if (CountryInfo$server_version()) {
        return(
          HTML(paste0(
            "<p style='font-size: large; margin-bottom: 20px;'>",
            "Please click below to retrieve the survey data from the server and begin your analysis.",
            "<hr style='border-top-color: #E0E0E0; margin-top: 20px;'>")))
      }
      
      ### which file(s) listed on the DHS website to download
      dhs_dat_names <- unlist(lapply(recode_for_ind_names(), function(recode) {
        find_DHS_dat_name(country= CountryInfo$country(),
                          svy_year = CountryInfo$svyYear_selected(),
                          recode=recode)
      }))
      
      ### which GPS file to download
      dhs_GPS_names <- find_DHS_dat_name(country= CountryInfo$country(),
                                         svy_year = CountryInfo$svyYear_selected(),
                                         recode='Geographic Data')
      
      dhs_dat_list_items <- paste0(
        "<li><span style='background-color: #D0E4F7; padding: 3px; border-radius: 3px;'><strong>",
        toupper(dhs_dat_names),' (',recode_for_ind_names(),')',
        "</strong></span></li>"
      )
      # Combine all items into a single string
      dhs_dat_list_items_display <- paste(dhs_dat_list_items, collapse = "")
      
      # Define the HTML content with a refined style
      upload_instruct_text <- HTML(paste0(
        "<p style='font-size: medium; margin-bottom: 20px; line-height: 2;'>",  # Add line-height for larger spacing
        "Please follow the steps below to select and download data from the DHS website.<sup>a</sup>",  # First footnote reference
        "</p>",
        "<ol style='font-size: medium; margin-top: 0; margin-bottom: 20px; line-height: 2;'>",
        "<li>",
        "Navigate to the <a href='https://dhsprogram.com/Data' target = '_blank'><strong>DHS website</strong></a> and locate the download section. ",
        "Reference <a href='https://dhsprogram.com/data/Using-DataSets-for-Analysis.cfm' target = '_blank'><strong>this page </strong></a> for detailed instructions.",
        "</li>",
        "<li>",
        "Select data with the following file names: <br>",
        "<ul style='list-style-type: disc; margin-left: 20px; line-height: 2;'>",
        "<li>",
        "Survey Datasets: ",
        "<ul style='list-style-type: disc; margin-left: 20px; line-height: 2;'>",
        dhs_dat_list_items_display,  # Dynamically generated list of survey datasets
        "</ul>",
        "</li>",
        "<li>",
        "Geographic Datasets: ",
        "<ul style='list-style-type: disc; margin-left: 20px; line-height: 2;'>",
        "<li>",
        "<span style='background-color: #D0E4F7; padding: 3px; border-radius: 3px;'>",
        "<strong>", toupper(dhs_GPS_names), ' (Geographic Data)',"</strong>",
        "</span>",
        "</li>",
        "</ul>",
        "</li>",
        "</ul>",
        "</li>",
        "<li>",
        "Include all needed files in a single download request.",
        "</li>",
        "<li>",
        "Upload the downloaded <strong>.zip file</strong> using the upload bar provided below.<sup>b</sup>",  # Second footnote reference
        "</li>",
        "</ol>",
        "<hr style='border-top-color: #E0E0E0; margin-top: 20px;'>",
        "<ol style='font-size: medium; margin-left: 20px; line-height: 2;' type='a'>",  # Alphabet indexing for footnotes
        "<li>",
        "An account is needed for accessing DHS data, please register at ",
        "<a href='https://dhsprogram.com/data/new-user-registration.cfm' target = '_blank'><strong>here. </strong></a>",
        "The approval process might take up to a few days.",
        "</li>",
        "<li>",
        "If the browser (such as Safari) automatically utils::unzips files on download, please manually re-zip them to a single file and upload.",
        "</li>",
        "</ol>"
      ))
      
      
      
      
      inst.modal.text(upload_instruct_text)
      
      HTML(paste0(
        "<p style='font-size: large; margin-bottom: 20px;'>",
        "Please upload the corresponding DHS datasets, click ",
        actionButton(
          ns("triggerModal"),  # Button ID to trigger the modal
          "here",
          style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 2px; font-size: large;"  # Larger font
        ),
        " for detailed instructions.",
        "<hr style='border-top-color: #E0E0E0; margin-top: 20px;'>"))
      
    })
    
    
    
    ###############################################################
    ### manually upload data
    ###############################################################
    
    observeEvent(CountryInfo$server_version(), {
      if (CountryInfo$server_version()){
        shinyjs::hide("Svy_dataFile")
        
        output$data_button <- renderUI({
          #actionButton(ns("preload_Svy_Data"), "Load from Server")
          actionButton(ns("preload_Svy_Data"), "Load Data from Server",
                       style = "color: white; background-color: #0073e6; border-color: #005bb5;")
        })
      } else {
        shinyjs::show("Svy_dataFile")
        output$data_button <- renderUI({
          actionButton(ns("upload_Svy_Data"), "Upload Survey Data")
        })
      }
    })
    
    
    
    observeEvent(CountryInfo$use_preloaded_Zambia(), {
      
      # if(CountryInfo$use_preloaded_Zambia()){
      #
      #   recode.data <- as.data.frame(zmb.ex.IR.dat)
      #   CountryInfo$update_svy_dat(recode_abbrev='IR', new_dat=recode.data)
      #
      #   GPS.dat <- zmb.ex.GPS
      #   CountryInfo$svy_GPS_dat(GPS.dat)
      #
      # }else{return()}
      
      
    })
    
    observeEvent(CountryInfo$use_preloaded_Madagascar(), {
      
      # if(CountryInfo$use_preloaded_Madagascar()){
      #
      #   recode.data <- as.data.frame(mdg.ex.KR.dat)
      #   CountryInfo$update_svy_dat(recode_abbrev='KR', new_dat=recode.data)
      #
      #   GPS.dat <- mdg.ex.GPS
      #   CountryInfo$svy_GPS_dat(GPS.dat)
      #
      # }else{return()}
      
      
    })
    
    
    ### load data from server
    observeEvent(input$preload_Svy_Data, {
      if(is.null(CountryInfo$svyYear_selected()) || CountryInfo$svyYear_selected() == ""){
        return(NULL)
      }
      
      
      session$sendCustomMessage('controlSpinner', list(action = "show",
                                                       message = paste0("Loading ",recode_names_abbrev[i]," Recode. Please wait...")))
      
      
      Sys.sleep(1)
      
      recode.path <- paste0(golem::get_golem_options()$server_link,'/DHS_survey_dat/',
                            country_code,'/DHS_',svy_year,'/',
                            recode_names_abbrev[i],'.rds'
      )
      
      #message(recode.path)
      #recode.data <- readRDS(recode.path)
      
      recode.data <- tryCatch(
        {
          readRDS(url(recode.path))
          #readRDS((recode.path))
          
        },
        error = function(e) {
          message("Failed to retrieve ",recode_names_abbrev[i], ' from the server.')
          message("Error: ", e$message)
          
          session$sendCustomMessage('controlSpinner', list(action = "show",
                                                           message = paste0("Fail to retrieve ",
                                                                            recode_names_abbrev[i],
                                                                            " recode from the server. Please contact us.")))
          Sys.sleep(2.5)
          NULL  # return NULL if not found
        }
      )
      
      if(!is.null(recode.data)){
        CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)
        new_dat_num <- new_dat_num+1
      }
      
      session$sendCustomMessage('controlSpinner', list(action = "hide"))
      
      
      ### set survey GPS data
      if(is.null( CountryInfo$svy_GPS_dat())){
        
        session$sendCustomMessage('controlSpinner', list(action = "show",
                                                         message = paste0( 'Loading Geographic Data. Please wait...')))
        
        GPS.path <- paste0(golem::get_golem_options()$server_link,'/DHS_survey_dat/',
                           country_code,'/DHS_',svy_year,'/',
                           'Geographic_Data.rds'
        )
      }
      
      # Show completion modal if all data already loaded
      if(new_dat_num == 0) {
        showDataCompleteModal()
      }
      
    })
    
    
    
    
    observeEvent(input$upload_Svy_Data, {
      
      # Check if a file has been uploaded
      if (is.null(input$Svy_dataFile)&
          !(paste0(CountryInfo$country_code_DHS(),'_',CountryInfo$svyYear_selected(),'_DHS.zip') %in%
            list.files(system.file("preloaded_DHS", package = "sae4health")))
      ) {
        message('This is a survey without preloaded data, and no data was uploaded.')
        showNoFileSelectedModal()
        return()
      }
      
      # if (is.null(input$Svy_dataFile)) {
      #   showNoFileSelectedModal()
      #   return()
      # }
      
      ### check whether all required recode has been uploaded
      required_recode <- recode_list_abbrev[which(ref_tab_all[ref_tab_all$ID==CountryInfo$svy_indicator_var(),
                                                              recode_list_abbrev]==T)]
      #message(required_recode)
      recode_status_check <- CountryInfo$check_svy_dat_upload(required_recode,CountryInfo$svy_dat_list())
      GPS_status_check <- is.null(CountryInfo$svy_GPS_dat())
      
      ### indicator
      data.upload.complete <- (all(c(!recode_status_check,!GPS_status_check)))
      if(data.upload.complete){
        showDataCompleteModal()
        return()
      }
      
      
      #req(input$Svy_dataFile)
      
      # set parameters
      country= CountryInfo$country()
      svy_year = CountryInfo$svyYear_selected()
      recode_names_list=recode_for_ind_names()
      
      ### check whether data is available inside the app, if so, load the data, no need to upload
      if(paste0(CountryInfo$country_code_DHS(),'_',CountryInfo$svyYear_selected(),'_DHS.zip') %in%
         list.files(system.file("preloaded_DHS", package = "sae4health"))){
        file_path <- system.file("preloaded_DHS",
                                 paste0(CountryInfo$country_code_DHS(),'_',CountryInfo$svyYear_selected(),'_DHS.zip'),
                                 package = "sae4health")
        
        message('Use preloaded data.')
        message(CountryInfo$country_code_DHS())
      }else{
        message('Use user uploaded data.')
        file_path <- input$Svy_dataFile$datapath
      }
      
      
      new_dat_num <- 0
      
      
      ## set survey recode data
      
      for (i in 1:length(recode_names_list)){
        
        session$sendCustomMessage('controlSpinner', list(action = "show",
                                                         message = paste0("Parsing .zip files. Please wait...")))
        
        file_prefix <- find_DHS_dat_name(country,svy_year,recode =recode_names_list[i] )
        
        recode_path_found <- find_recode_path(file_path = file_path,
                                              recode_file =file_prefix,
                                              extensions = 'DTA')
        
        session$sendCustomMessage('controlSpinner', list(action = "hide"))
        
        if(!is.null(recode_path_found)){
          #message(paste0(recode_names_list[i] ,' recode found at: ',(recode_path_found)))
          
          session$sendCustomMessage('controlSpinner', list(action = "show",
                                                           message = paste0(recode_names_list[i],
                                                                            " found, loading...")))
          
          
          
          recode.data <- suppressWarnings(haven::read_dta(recode_path_found))
          
          recode.data <- as.data.frame(recode.data)
          
          #mdg.ex.KR.dat <- recode.data
          
          
          #recode.data <- as.data.frame(zmb.ex.IR.dat)
          
          #Sys.sleep(1)
          
          CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)
          
          new_dat_num <- new_dat_num+1
          #message(paste0(recode_for_ind_abbrev()[i]))
          session$sendCustomMessage('controlSpinner', list(action = "hide"))
          
          
          
          
        }
      }
      
      ## set survey GPS data
      
      GPS_prefix <- find_DHS_dat_name(country,svy_year,recode = 'Geographic Data' )
      
      GPS_path_found <- find_recode_path(file_path = file_path,
                                         recode_file = GPS_prefix,
                                         extensions = 'shp')
      
      #message(GPS_path_found)
      
      if(!is.null(GPS_path_found)){
        #message(paste0(GPS_prefix ,' recode found at: ',(GPS_path_found)))
        
        session$sendCustomMessage('controlSpinner', list(action = "show",
                                                         message = paste0( 'Geographic Data',
                                                                           " found, loading...")))
        GPS.dat <- suppressWarnings(sf::st_read(GPS_path_found))
        GPS.dat <- sf::st_set_crs(GPS.dat, 4326)
        
        
        Sys.sleep(1)
        CountryInfo$svy_GPS_dat(GPS.dat)
        
        new_dat_num <- new_dat_num+1
        
        session$sendCustomMessage('controlSpinner', list(action = "hide"))
        
        
        if(FALSE){
          ### assigning cluster and admin information
          gadm.names <- names(CountryInfo$GADM_list())
          
          session$sendCustomMessage('controlSpinner', list(action = "show",
                                                           message = paste0("Processing cluster GPS information, please wait")))
          for(adm.level in gadm.names){
            message(adm.level)
            
            tmp.cluster.adm.info <- cluster_admin_info(cluster.geo= GPS.dat,  #mdg.ex.GPS
                                                       gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                                       model.gadm.level = admin_to_num(adm.level),
                                                       strat.gadm.level = CountryInfo$GADM_strata_level())
            
            
            AnalysisInfo$set_info_list(adm.level,tmp.cluster.adm.info)
            
          }
          
          session$sendCustomMessage('controlSpinner', list(action = "hide"))
          
        }
        
        
        
        
      }
      
      if(new_dat_num==0){
        message('No new updata uploaded.')
        session$sendCustomMessage('controlSpinner', list(action = "show",
                                                         message = paste0( 'No required data found for this indicator .',
                                                                           "Please check out instructions for downloading survey data.")))
        
        
        Sys.sleep(2.5)
        session$sendCustomMessage('controlSpinner', list(action = "hide"))
        
      }
      #message(tmp.list)
      #message(CountryInfo$svy_GPS_dat())
      
      
    })
    
    
    
    ###############################################################
    ### cluster map
    ###############################################################
    
    
    ### determine static vs interactive
    observeEvent(input$ClusterMapType,{
      
      CountryInfo$display_interactive(input$ClusterMapType)
      
    })
    
    observeEvent(CountryInfo$display_interactive(),{
      
      interactive_map <- CountryInfo$display_interactive()
      shinyWidgets::updateMaterialSwitch(session=session, inputId="ClusterMapType", value = interactive_map)
      
    })
    
    ### determine which UI to present plot
    
    output$cluster_map <- renderUI({
      if (input$ClusterMapType) {  # if TRUE, show interactive map
        leaflet::leafletOutput(ns("cluster_map_interactive"))
      } else {  # if FALSE, show static map
        plotOutput(ns("cluster_map_static"))
      }
    })
    
    
    ### update admin-levels
    observe({
      req(CountryInfo$GADM_list())
      
      gadm.names <- names(CountryInfo$GADM_list())
      gadm.names <- gadm.names[gadm.names!='National']
      
      if(length(gadm.names)>0){
        updateRadioButtons(session, "AdminLevel", choices = gadm.names,
                           inline = T)
      }
      
    })
    
    output$cluster_map_interactive <- leaflet::renderLeaflet({
      req(CountryInfo$GADM_list())
      req(CountryInfo$svy_GPS_dat())
      
      ### initialize base map
      cluster.interactive.plot <- leaflet::leaflet()
      
      if(CountryInfo$use_basemap()=='OSM'){
        cluster.interactive.plot <- cluster.interactive.plot %>% leaflet::addTiles()
      }
      
      ### return empty map if no subnational level selected
      if (length(input$AdminLevel) == 0 || input$AdminLevel == ""||is.null(CountryInfo$GADM_list())||
          is.null(CountryInfo$svy_GPS_dat())) {
        return(cluster.interactive.plot)
      }
      
      session$sendCustomMessage('controlSpinner', list(action = "show",
                                                       message = paste0( 'Calculating Cluster ',
                                                                         "Info...")))
      
      ncluster.res <- ncluster.map.interactive(gadm.level=input$AdminLevel,
                                               gadm.list=CountryInfo$GADM_list(),
                                               cluster.geo=CountryInfo$svy_GPS_dat(),
                                               use.basemap = CountryInfo$use_basemap(),
                                               legend.color.reverse=CountryInfo$legend_color_reverse())
      
      session$sendCustomMessage('controlSpinner', list(action = "hide"))
      
      
      return(ncluster.res$map)
      
      
    })
    
    output$cluster_map_static  <- renderPlot({
      req(CountryInfo$GADM_list())
      req(CountryInfo$svy_GPS_dat())
      
      
      
      ### return empty map if no subnational level selected
      if (length(input$AdminLevel) == 0 || input$AdminLevel == ""||is.null(CountryInfo$GADM_list())||
          is.null(CountryInfo$svy_GPS_dat())) {
        return(NULL)
      }
      
      session$sendCustomMessage('controlSpinner', list(action = "show",
                                                       message = paste0( 'Calculating Cluster ',
                                                                         "Info...")))
      ncluster.res <- ncluster.map.static(gadm.level=input$AdminLevel,
                                          gadm.list=CountryInfo$GADM_list(),
                                          cluster.geo=CountryInfo$svy_GPS_dat())
      
      session$sendCustomMessage('controlSpinner', list(action = "hide"))
      
      return(ncluster.res$map)
      
      
    })
    
    ###############################################################
    ### Data preview
    ###############################################################
    
    ### when survey uploaded or new indicator specified, update analysis data set
    data_pre_snapshot <- reactive({
      list(
        indicator_selected = CountryInfo$svy_indicator_var(),
        svy_dat_uploaded = CountryInfo$svy_dat_list()
      )
    })
    
    
    ### prepare analysis data set
    observeEvent(data_pre_snapshot(),{
      
      prepare_analysis_data(CountryInfo, AnalysisInfo, session)
      
      
      
    })
    
    
    ### analysis set data set preview
    output$Dat_Preview <- DT::renderDataTable({
      
      
      req(CountryInfo$svy_dat_list())
      req(CountryInfo$svy_indicator_var())
      
      
      if(!dat.complete.ind()){
        return()
      }
      
      analysis_dat <-CountryInfo$svy_analysis_dat()
      
      
      
      if(is.null(analysis_dat)){return()
      }else{
        
        analysis_dat <- haven::as_factor(analysis_dat)
        
        ### do not display strata info for WHO version of the app
        if(CountryInfo$WHO_version()){
          analysis_dat <- analysis_dat[ , !(names(analysis_dat) %in% c('v024'))]
        }
        
        message(colnames(analysis_dat))
        
        if(CountryInfo$server_version()){
          
          
          set.seed(2025)
          subset_analysis_dat <- analysis_dat[sample(nrow(analysis_dat), 25), ]
          
          subset_analysis_dat <- subset_analysis_dat[ , !(names(subset_analysis_dat) %in% c('householdID'))]
          
          dt <- DT::datatable(subset_analysis_dat,
                              options = list(pageLength = 5,scrollX = TRUE,
                                             scroller = TRUE),
                              filter = 'top', rownames = FALSE,
                              caption = htmltools::tags$caption(
                                style = 'caption-side: top; text-align: left; font-weight: bold;',
                                'Note: only a small subset of data is shown due to confidentiality.'
                              ))
          
          # Apply formatting styles
          dt <- DT::formatStyle(dt,
                                columns = names(subset_analysis_dat),
                                backgroundColor = 'rgba(255, 255, 255, 0.8)',
                                border = '1px solid #ddd',
                                fontSize = '14px',
                                fontWeight = 'normal',
                                lineHeight = '1.42857143')
          
        }else{
          dt <- DT::datatable(analysis_dat,
                              options = list(pageLength = 5,scrollX = TRUE,
                                             scroller = TRUE),
                              filter = 'top', rownames = FALSE)
          
          # Apply formatting styles
          dt <- DT::formatStyle(dt,
                                columns = names(analysis_dat),
                                backgroundColor = 'rgba(255, 255, 255, 0.8)',
                                border = '1px solid #ddd',
                                fontSize = '14px',
                                fontWeight = 'normal',
                                lineHeight = '1.42857143')
          
        }
        
        dt
        
        #DT::datatable(analysis_dat)
        
        
        
      }
      
    })
    
    
    ### download button - analysis data set
    
    output$download_button_ui <- renderUI({
      
      analysis_dat <-CountryInfo$svy_analysis_dat()
      
      if(!CountryInfo$server_version()){
        # csv download
        downloadButton(ns("download_csv"), "Download as csv", icon = icon("download"),
                       class = "btn-primary")
      } else {
        NULL
      }
      
      # if(!paste0(CountryInfo$country_code_DHS(),'_',CountryInfo$svyYear_selected(),'_DHS.zip') %in%
      #    list.files(system.file("preloaded_DHS", package = "sae4health"))){
      #   # csv download
      #   downloadButton(ns("download_csv"), "Download as csv", icon = icon("download"),
      #                  class = "btn-primary")
      # } else {
      #   NULL
      # }
    })
    
    ### download analysis data set
    
    output$download_csv <- downloadHandler(
      filename = function() {
        file.prefix <- paste0(CountryInfo$country(),'_',
                              CountryInfo$svy_indicator_var(),'_')
        file.prefix <- gsub("[-.]", "_", file.prefix)
        
        return(paste0(file.prefix,'raw_data.csv'))
      },
      content = function(file) {
        analysis_dat <- as.data.frame(CountryInfo$svy_analysis_dat())
        readr::write_csv(analysis_dat, file)
      }
    )
    
    ###############################################################
    ### Generate checklist UI
    ###############################################################
    
    
    output$checklist <- renderUI({
      
      req(recode_for_ind_abbrev())
      
      #message(recode_for_ind_abbrev())
      recode_status_check <- CountryInfo$check_svy_dat_upload(recode_for_ind_abbrev(),CountryInfo$svy_dat_list())
      
      #message(recode_status_check)
      GPS_status_check <- is.null(CountryInfo$svy_GPS_dat())
      
      
      
      ## update data upload status, if all uploaded, complete=T
      dat.complete.ind(all(c(!recode_status_check,!GPS_status_check)))
      #message((c(!recode_status_check,!GPS_status_check)))
      #message(dat.complete.ind())
      
      tagList(
        div(style = "margin-top: -5px;margin-bottom: -10px",
            tags$h4("Upload Checklist:")
        ),
        ### display progress once indicator selected
        
        lapply(1:length(recode_for_ind_abbrev()), function(i) {
          checklistItem(paste(recode_for_ind_abbrev()[i],' Recode'), !recode_status_check[i])
        }),
        checklistItem(paste("GPS data"), !GPS_status_check)
        
        
      )
    })
    
    
    
  })
  
}

checklistItem <- function(name, completed = FALSE) {
  icon <- if(completed) "fas fa-check-circle" else "fas fa-times-circle"
  color <- if(completed) "green" else "orange"
  statusText <- if(completed) "Completed" else "Pending"
  
  tags$div(class = "checklist-item",
           tags$i(class = icon, style = sprintf("color: %s; margin-right: 5px;", color)),
           tags$span(style = sprintf("color: %s;", color), sprintf("%s: %s", name, statusText)),
           style = "margin-bottom: -2px; cursor: pointer;font-size: 1.2em;"
           #onclick = sprintf("this.style.opacity = '%s'", if(completed) "0.5" : "1")
  )
}

## To be copied in the UI
# mod_survey_dat_input_ui("survey_dat_input_1")

## To be copied in the server
# mod_survey_dat_input_server("survey_dat_input_1")

