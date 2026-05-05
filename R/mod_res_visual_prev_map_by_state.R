#' result_visual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_visual_prev_map_by_state_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    shinyWidgets::chooseSliderSkin("Flat", color = "#b0c4de"),
    tags$head(
      tags$style(
        type = "text/css",
        "#big_slider .irs-grid-text, #big_slider .irs-min,
         #big_slider .irs-max,#big_slider .irs-single {font-size: 14px;}"
      ),
      tags$style(HTML("
        .button-container {
          display: flex;
          justify-content: center;
          width: max(50%, 600px);
          margin: 20px auto;
        }
      "))
    ),
    
    div(class = "module-title",
        h4("Subnational Results Mapping")
    ),
    
    fluidRow(
      column(
        12,
        div(
          style = "margin: auto; float: left; margin-top: 5px",
          uiOutput(ns("info_display"))
        )
      )
    ),
    
    fluidRow(
      column(
        4,
        selectInput(
          ns("selected_adm"),
          "Select Admin Level",
          choices = character(0)
        )
      ),
      column(
        4,
        selectInput(
          ns("selected_focus"),
          "Look Within",
          choices = "National",
          selected = "National"
        )
      ),
      column(
        4,
        selectInput(
          ns("selected_method"),
          "Select Method",
          choices = c(
            "Direct Estimates" = "Direct",
            "Area-level Model" = "FH",
            "Unit-level Model" = "Unit"
          )
        )
      )
    ),
    
    fluidRow(
      column(
        4,
        selectInput(
          ns("selected_measure"),
          "Select Statistics",
          choices = c(
            "Mean" = "mean",
            "Coefficient of Variation" = "cv",
            "Width of 95% Credible Interval" = "CI.width",
            "Exceedance Probability" = "exceed_prob"
          )
        )
      ),
      div(
        id = "big_slider",
        column(
          4,
          uiOutput(ns("choose_prob"))
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        div(
          style = "margin: auto; float: left;",
          uiOutput(ns("text_display"))
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        hr(style = "border-top-color: #E0E0E0;"),
        shinyWidgets::materialSwitch(
          inputId = ns("PrevmapType"),
          label = "Interactive Map Enabled",
          status = "success",
          value = TRUE
        ),
        div(
          id = "map-container",
          style = "width: max(50%, 600px); margin-top: 20px;",
          uiOutput(ns("prev_map"))
        ),
        div(
          style = "width: max(50%, 600px); margin-top: 20px; display: flex; justify-content: center;",
          uiOutput(ns("download_button_ui"))
        )
      )
    )
  )
}


#' result_visual Server Functions
#'
#' @noRd
mod_res_visual_prev_map_by_state_server <- function(id, CountryInfo, AnalysisInfo) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    
    ###############################################################
    ### helpers
    ###############################################################
    
    analysis_levels <- reactive({
      levs <- CountryInfo$GADM_analysis_levels()
      req(levs)
      levs
    })
    
    available_map_levels <- reactive({
      levs <- analysis_levels()
      levs[levs != "National"]
    })
    
    available_methods <- reactive({
      res <- AnalysisInfo$model_res_list()
      if (is.null(res)) {
        return(c("Direct", "FH", "Unit"))
      }
      names(res)
    })
    
    level_num <- function(level_name) {
      if (identical(level_name, "National")) return(0L)
      out <- suppressWarnings(as.integer(gsub(".*Admin-", "", level_name)))
      if (is.na(out)) return(NA_integer_)
      out
    }
    
    get_parent_level <- reactive({
      req(input$selected_adm)
      this_num <- level_num(input$selected_adm)
      req(!is.na(this_num))
      
      if (this_num <= 1) {
        "National"
      } else {
        paste0("Admin-", this_num - 1)
      }
    })
    
    get_sf_for_level <- function(level_name) {
      shp_list <- CountryInfo$GADM_list_smoothed()
      req(shp_list)
      shp <- shp_list[[level_name]]
      req(!is.null(shp))
      shp
    }
    
    guess_name_col <- function(sf_obj, level_name = NULL) {
      nm <- names(sf_obj)
      
      candidates <- character(0)
      
      if (!is.null(level_name) && !identical(level_name, "National")) {
        lvl_num <- level_num(level_name)
        if (!is.na(lvl_num)) {
          candidates <- c(
            candidates,
            paste0("NAME_", lvl_num),
            paste0("name_", lvl_num),
            paste0("ADM", lvl_num, "_EN"),
            paste0("ADM", lvl_num, "_NAME"),
            paste0("admin", lvl_num),
            paste0("Admin", lvl_num)
          )
        }
      }
      
      candidates <- c(
        candidates,
        "shapeName",
        "NAME_1", "NAME_2", "NAME_3", "NAME_4",
        "name_1", "name_2", "name_3", "name_4",
        "ADM1_EN", "ADM2_EN", "ADM3_EN", "ADM4_EN",
        "ADM1_NAME", "ADM2_NAME", "ADM3_NAME", "ADM4_NAME",
        "admin1", "admin2", "admin3", "admin4",
        "Admin1", "Admin2", "Admin3", "Admin4",
        "state", "State", "district", "District",
        "county", "County", "province", "Province"
      )
      
      hit <- unique(candidates[candidates %in% nm])
      
      if (length(hit) == 0) {
        stop("Could not infer region-name column for shapefile.")
      }
      
      hit[1]
    }
    
    guess_parent_col_in_child <- function(child_sf, parent_level) {
      nm <- names(child_sf)
      
      if (identical(parent_level, "National")) {
        return(NULL)
      }
      
      parent_num <- level_num(parent_level)
      req(!is.na(parent_num))
      
      candidates <- c(
        paste0("NAME_", parent_num),
        paste0("name_", parent_num),
        paste0("ADM", parent_num, "_EN"),
        paste0("ADM", parent_num, "_NAME"),
        paste0("admin", parent_num),
        paste0("Admin", parent_num)
      )
      
      hit <- unique(candidates[candidates %in% nm])
      
      if (length(hit) == 0) {
        stop(
          paste0(
            "Could not infer parent-region column for ",
            parent_level,
            " inside child shapefile."
          )
        )
      }
      
      hit[1]
    }
    
    get_focus_choices <- reactive({
      req(input$selected_adm)
      
      parent_level <- get_parent_level()
      
      if (identical(parent_level, "National")) {
        return("National")
      }
      
      parent_sf <- get_sf_for_level(parent_level)
      parent_name_col <- guess_name_col(parent_sf, parent_level)
      
      vals <- parent_sf[[parent_name_col]]
      vals <- vals[!is.na(vals)]
      vals <- sort(unique(as.character(vals)))
      
      c("National", vals)
    })
    
    get_selected_poly <- reactive({
      req(input$selected_adm, input$selected_focus)
      
      child_level <- input$selected_adm
      focus_region <- input$selected_focus %||% "National"
      
      child_sf <- get_sf_for_level(child_level)
      
      if (identical(focus_region, "National")) {
        return(child_sf)
      }
      
      parent_level <- get_parent_level()
      req(!identical(parent_level, "National"))
      
      parent_col_in_child <- guess_parent_col_in_child(child_sf, parent_level)
      
      keep <- as.character(child_sf[[parent_col_in_child]]) == focus_region
      keep[is.na(keep)] <- FALSE
      
      child_sf_subset <- child_sf[keep, , drop = FALSE]
      req(nrow(child_sf_subset) > 0)
      
      child_sf_subset
    })
    
    get_selected_results <- reactive({
      req(input$selected_method, input$selected_adm)
      res_all <- AnalysisInfo$model_res_list()
      req(res_all)
      
      method_res <- res_all[[input$selected_method]]
      req(!is.null(method_res))
      
      method_res[[input$selected_adm]]
    })
    
    method_label <- reactive({
      req(input$selected_method)
      method_match <- c(
        "Direct" = "Direct estimates",
        "Unit" = "Unit-level",
        "FH" = "Area-level"
      )
      method_match[[input$selected_method]] %||% input$selected_method
    })
    
    ###############################################################
    ### dynamic inputs
    ###############################################################
    
    observe({
      method_choices <- available_methods()
      
      label_map <- c(
        "Direct" = "Direct Estimates",
        "FH" = "Area-level Model",
        "Unit" = "Unit-level Model"
      )
      
      named_choices <- stats::setNames(
        method_choices,
        label_map[method_choices] %||% method_choices
      )
      
      selected_method <- if ("Direct" %in% method_choices) "Direct" else method_choices[1]
      
      updateSelectInput(
        session = session,
        inputId = "selected_method",
        choices = named_choices,
        selected = selected_method
      )
    })
    
    observe({
      levs <- available_map_levels()
      req(length(levs) > 0)
      
      selected_adm <- if ("Admin-2" %in% levs) "Admin-2" else levs[1]
      
      updateSelectInput(
        session = session,
        inputId = "selected_adm",
        choices = levs,
        selected = selected_adm
      )
    })
    
    observe({
      req(input$selected_adm)
      
      focus_choices <- get_focus_choices()
      parent_level <- get_parent_level()
      
      updateSelectInput(
        session = session,
        inputId = "selected_focus",
        label = if (identical(parent_level, "National")) {
          "Look Within"
        } else {
          paste("Look Within", parent_level)
        },
        choices = focus_choices,
        selected = "National"
      )
    })
    
    ###############################################################
    ### display info
    ###############################################################
    
    output$info_display <- renderUI({
      HTML(paste0(
        "<p style='font-size: medium;'>",
        "Choose the admin level to map. ",
        "For Admin-2 or higher, you can optionally focus the map within one sub-region, ",
        "or keep <b>National</b> selected to view the full country.",
        "</p>"
      ))
    })
    
    # output$choose_prob <- renderUI({
    #   req(input$selected_measure)
    #   
    #   if (input$selected_measure == "exceed_prob") {
    #     shinyWidgets::sliderTextInput(
    #       inputId = ns("selected_threshold"),
    #       label = "Choose Exceedance Threshold",
    #       choices = as.character(seq(0, 1, by = 0.05)),
    #       selected = "0.20",
    #       grid = TRUE,
    #       width = "100%"
    #     )
    #   } else {
    #     NULL
    #   }
    # })
    
    output$choose_prob <- renderUI({
      req(input$selected_measure)
      
      if (input$selected_measure == "exceed_prob") {
        
        tmp.natl.res <- AnalysisInfo$Natl_res()
        
        if (!is.null(tmp.natl.res) && !is.null(tmp.natl.res$direct.est)) {
          initial.val <- round(as.numeric(tmp.natl.res$direct.est), digits = 2)
        } else {
          initial.val <- 0.50
        }
        
        initial.val <- min(max(initial.val, 0), 1)
        
        sliderInput(
          ns("selected_threshold"),
          "Select Threshold",
          min = 0,
          max = 1,
          value = initial.val,
          step = 0.01
        )
      } else {
        NULL
      }
    })
    
    output$text_display <- renderUI({
      req(input$selected_adm, input$selected_focus, input$selected_method, input$selected_measure)
      req(AnalysisInfo$model_res_list())
      
      selected_adm <- input$selected_adm
      selected_focus <- input$selected_focus
      selected_measure_code <- input$selected_measure
      
      measure_labels <- c(
        "mean" = "Mean",
        "cv" = "Coefficient of Variation",
        "CI.width" = "Width of 95% Credible Interval",
        "exceed_prob" = "Exceedance Probability"
      )
      
      selected_measure <- measure_labels[[selected_measure_code]] %||% selected_measure_code
      method_des <- method_label()
      
      model_res_selected <- tryCatch(
        get_selected_results(),
        error = function(e) NULL
      )
      
      focus_text <- if (identical(selected_focus, "National")) {
        "the whole country"
      } else {
        paste("within", selected_focus)
      }
      
      if (is.null(model_res_selected)) {
        HTML(paste0(
          "<p style='font-size: large;'>",
          "Results for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> ",
          "are <strong style='color: red;'>NOT</strong> available. ",
          "Please make sure the model has been successfully fitted.",
          "</p>"
        ))
      } else {
        HTML(paste0(
          "<p style='font-size: large;'>",
          "Presenting ",
          "<span style='background-color: #D0E4F7;'><b>", selected_measure, "</b></span> ",
          "for the ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> ",
          "for ",
          "<span style='background-color: #D0E4F7;'><b>", focus_text, "</b></span>.",
          "</p>"
        ))
      }
    })
    
    ###############################################################
    ### map output selector
    ###############################################################
    
    output$prev_map <- renderUI({
      if (isTRUE(input$PrevmapType)) {
        leaflet::leafletOutput(ns("prev_map_interactive"), height = "700px")
      } else {
        plotOutput(ns("prev_map_static"), height = "700px")
      }
    })
    
    output$download_button_ui <- renderUI({
      downloadButton(
        ns("download_static"),
        "Download Static Map",
        icon = icon("download"),
        class = "btn-primary"
      )
    })
    
    ###############################################################
    ### interactive map
    ###############################################################
    
    prev.map.interactive.output <- reactiveVal(NULL)
    
    output$prev_map_interactive <- leaflet::renderLeaflet({
      
      prev.interactive.plot <- leaflet::leaflet()
      
      if (CountryInfo$use_basemap() == "OSM") {
        prev.interactive.plot <- prev.interactive.plot %>% leaflet::addTiles()
      }
      
      req(input$selected_adm, input$selected_focus, input$selected_method, input$selected_measure)
      req(AnalysisInfo$model_res_list())
      
      if (isTRUE(CountryInfo$use_preloaded_Madagascar())) {
        AnalysisInfo$model_res_list(mdg.ex.model.res)
      }
      
      model_res_selected <- tryCatch(
        get_selected_results(),
        error = function(e) NULL
      )
      
      if (is.null(model_res_selected)) {
        return(prev.interactive.plot)
      }
      
      poly_selected <- tryCatch(
        get_selected_poly(),
        error = function(e) {
          message("Polygon selection failed: ", e$message)
          NULL
        }
      )
      
      if (is.null(poly_selected) || nrow(poly_selected) == 0) {
        return(prev.interactive.plot)
      }
      
      selected_threshold <- if (input$selected_measure == "exceed_prob") {
        req(input$selected_threshold)
        as.numeric(input$selected_threshold)
      } else {
        NULL
      }
      
      prev.interactive.plot <- tryCatch({
        suppressWarnings(
          surveyPrev::prevMap.web(
            res.obj = model_res_selected,
            poly.shp = poly_selected,
            admin1.focus = NULL,
            value.to.plot = input$selected_measure,
            legend.label = "Estimates",
            map.title = NULL,
            threshold.p = selected_threshold,
            use.basemap = CountryInfo$use_basemap(),
            legend.color.reverse = CountryInfo$legend_color_reverse()
          )
        )
      }, error = function(e) {
        message("prevMap.web failed: ", e$message)
        prev.interactive.plot
      })
      
      prev.map.interactive.output(prev.interactive.plot)
      prev.interactive.plot
    })
    
    ###############################################################
    ### static map
    ###############################################################
    
    prev.map.static.output <- reactiveVal(NULL)
    
    output$prev_map_static <- renderPlot({
      req(input$selected_adm, input$selected_focus, input$selected_method, input$selected_measure)
      req(AnalysisInfo$model_res_list())
      
      if (isTRUE(CountryInfo$use_preloaded_Madagascar())) {
        AnalysisInfo$model_res_list(mdg.ex.model.res)
      }
      
      model_res_selected <- tryCatch(
        get_selected_results(),
        error = function(e) NULL
      )
      
      if (is.null(model_res_selected)) {
        return(NULL)
      }
      
      poly_selected <- tryCatch(
        get_selected_poly(),
        error = function(e) {
          message("Polygon selection failed: ", e$message)
          NULL
        }
      )
      
      if (is.null(poly_selected) || nrow(poly_selected) == 0) {
        return(NULL)
      }
      
      selected_threshold <- if (input$selected_measure == "exceed_prob") {
        req(input$selected_threshold)
        as.numeric(input$selected_threshold)
      } else {
        NULL
      }
      
      prev.static.plot <- tryCatch({
        suppressWarnings(
          surveyPrev::prevMap(
            res.obj = model_res_selected,
            poly.shp = poly_selected,
            admin1.focus = NULL,
            value.to.plot = input$selected_measure,
            threshold.p = selected_threshold,
            legend.label = "Estimates",
            color.reverse = CountryInfo$legend_color_reverse(),
            map.title = NULL
          )
        )
      }, error = function(e) {
        message("prevMap failed: ", e$message)
        NULL
      })
      
      prev.map.static.output(prev.static.plot)
      prev.static.plot
    })
    
    ###############################################################
    ### download static map
    ###############################################################
    
    output$download_static <- downloadHandler(
      filename = function() {
        DHS_country_code <- DHS.country.meta[
          DHS.country.meta$CountryName == CountryInfo$country(),
        ]$DHS_CountryCode
        
        focus_part <- gsub("[^A-Za-z0-9]+", "_", input$selected_focus %||% "National")
        
        file.prefix <- paste0(
          DHS_country_code,
          CountryInfo$svyYear_selected(), "_",
          CountryInfo$svy_indicator_var(), "_",
          input$selected_adm, "_",
          focus_part, "_",
          input$selected_method, "_",
          input$selected_measure
        )
        
        file.prefix <- gsub("[-.]", "_", file.prefix)
        
        paste0(file.prefix, "_prevMap.pdf")
      },
      
      content = function(file) {
        pdf(file, width = 10, height = 10)
        print(prev.map.static.output())
        dev.off()
      }
    )
    
  })
}

## To be copied in the UI
# mod_res_visual_prev_map_by_state_ui("result_visual_1")

## To be copied in the server
# mod_res_visual_prev_map_by_state_server("result_visual_1", CountryInfo, AnalysisInfo)